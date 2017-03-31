/* unix-memory.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Allocated BLOCK_SZB aligned memory chunks using mmap.  The public API for
 * this code is in os-memory.h.
 *
 * NOTE: on MacOS X, we could use the vm_map() Mach system call, which allows an
 * alignment mask to be set.
 */

#include "manticore-rt.h"
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include "os-memory.h"
#include "heap.h"
#include "internal-heap.h"
#include <stdio.h>

#define PROT_ALL        PROT_EXEC|PROT_READ|PROT_WRITE
#ifndef MAP_ANON
#  ifdef MAP_ANONYMOUS
#    define MAP_ANON MAP_ANONYMOUS
#  else
#    error MAP_ANON not defined
#  endif
#endif

// initialized using sysconf call in heap.c
long GUARD_PAGE_BYTES = 0;

STATIC_INLINE void *MapMemory (void *base, size_t szb)
{
  /* NOTE: we use -1 as the fd argument, because Mac OS X uses the fd for
   * Mach VM flags when MAP_ANON has been specified.
   */
    int flags = MAP_PRIVATE|MAP_ANON;
    if (base != 0)
	flags |= MAP_FIXED;

    return mmap(base, szb, PROT_ALL, flags, -1, 0);

}

STATIC_INLINE void UnmapMemory (void *base, size_t szb)
{
    munmap (base, szb);
}

void* SimpleAlloc(size_t szb) {
    void* mem = MapMemory(0, szb);
    
    if(mem == MAP_FAILED) {
        return 0;
    }
    
    return mem;
}

/* AllocMemory:
 *
 * Allocate nBlocks of blkSzB bytes (aligned on blkSzB boundary).  A
 * pointer to the memory is returned and nBlocks is set to the number
 * of allocated blocks.
 */
void *AllocMemory (int *nBlocks, int blkSzB, int minNumBlocks, void **unalignedBase)
{
    void	*memObj, *base, *orig, *unmap;
    size_t	szb;

  /* first, we try to allocate a chunk that is one block bigger than
   * requested and at least one block bigger than the minimum block size.
   */
    int n = *nBlocks + 1;
    do {
	szb = n * blkSzB;
	memObj = MapMemory(0, szb);
        if (memObj == MAP_FAILED) {
	    if ((errno == ENOMEM) && (n > minNumBlocks+1)) {
	      /* try a smaller request */
		n--;
		continue;
	    }
	    else {
		*nBlocks = 0;
		return 0;
	    }
	}
    } while (memObj == MAP_FAILED);

    orig = memObj;
  /* now compute the lowest aligned address in the allocated block. */
    base = (void *)(((Addr_t)memObj & ~(blkSzB-1)) + blkSzB);

    assert (((uint64_t)base)+(*nBlocks*blkSzB) <= ((uint64_t)memObj)+szb);

    *unalignedBase = memObj;
    return base;
} /* end of AllocMemory */

/* FreeMemory:
 *
 * free a memory object allocated by AllocMemory (its size is 
 * szB bytes).
 */
void FreeMemory (void *base, int szB)
{
    TotalVM -= szB;
    UnmapMemory (base, szB);

} /* end of FreeMemory */

// Allocates a region of memory suitable for
// use as a stack. 
//
// Returns the pointer to the mmap information of the stack for GC tracking, etc.
//
// The stack pointer p returned is guarenteed to be such that p+8 is 
// 16-byte aligned, per the SysV ABI. The pointer returned
// is ready to be used as a stack pointer after writing a ret addr.
// Here's a picture (where numBytes is approximate):
//
//                            16-byte aligned
//                                   v
// | guard |  numBytes-ish  |bbbbbbbb| ... StackInfo_t ... |  high addresses >
//                          ^
//                   info->initialSP 
//
StackInfo_t* AllocStack(size_t numBytes, uint8_t** top, uint8_t* lim) {
    StackInfo_t* info;
    
    // NOTE automatic resizing using MAP_GROWSDOWN has
	// been deprecated: https://lwn.net/Articles/294001/
    
	size_t guardSz = GUARD_PAGE_BYTES;
    size_t stackLen = numBytes + guardSz;
    size_t totalSz = stackLen + sizeof(StackInfo_t);
    
    totalSz = ROUNDUP(totalSz, guardSz);
    
    void* mem;
    if (top == 0 || ((*top) + totalSz) >= lim) {
        mem = MapMemory(0, totalSz);
        
        if(mem == MAP_FAILED) {
            return 1;
        }
    } else {
        mem = *top;
        *top = (*top) + totalSz;
    }
    
    // we protect the low end of the block to
    // detect stack overflow. this is done manually
    // because mmap on OS X seems to only place a protected
    // page after the buffer, not before it.
    if(mprotect(mem, guardSz, PROT_NONE)) {
        // failed to initialize guard area.
        return 2;
    }
    
    uint64_t val = (uint64_t) mem;
    
    // initialize the stack's info descriptor
    info = (StackInfo_t*)(val + stackLen);
    info->mmapBase = mem;
    info->mmapSize = totalSz;
    info->deepestScan = info;
    info->age = AGE_Minor;
    info->next = NULL;
    info->prev = NULL;
    info->prevSegment = NULL;
    info->currentSP = NULL;
    
    // setup stack pointer
    val = val + stackLen - 16;		// switch sides, leaving some headroom.
    val = ROUNDDOWN(val, 16ULL);	// realign downwards.
    val = val - 8;					// make space for return addr.
    
	void* sp = (void*)val;
    
    info->initialSP = sp;
    
    return info;
}

// Allocates a region of memory suitable for
// use as a stack segment
//
// Returns the pointer to the mmap information of the stack for GC tracking, etc.
//
// The stack pointer p returned is guarenteed to be such that p+8 is 
// 16-byte aligned, per the SysV ABI. The pointer returned
// is ready to be used as a stack pointer after writing a ret addr.
// Here's a picture (where numBytes is approximate):
//
//                 16-byte aligned --| |-- dummy watermark
//                                   v v 
// | guard |  STACK_REGION  |bbbbbbbb| 2 | ~0 | ... StackInfo_t ... |  high addresses >
//                          ^              ^
//                   info->initialSP     invalid frame size
//
//  where STACK_REGION looks like this:
//  
//                  info->stkLimit
//                        v
//  | C stack area | slop | usable stack space |
//         ^          ^             ^    
//      1kb-ish      128b      numBytes-ish
//
StackInfo_t* AllocStackSegment(size_t numBytes, uint8_t** top, uint8_t* lim) {
    StackInfo_t* info;
    
	size_t guardSz = GUARD_PAGE_BYTES;
    size_t slopSz = 128;
    size_t ccallSz = 8192;
    size_t bonusSz = 8 * sizeof(uint64_t);  // watermark + frame size + max realign
    
    size_t totalRegion = ccallSz + slopSz + numBytes + bonusSz;
    size_t stackLen = guardSz + totalRegion;
    size_t totalSz = stackLen + sizeof(StackInfo_t);
    
    totalSz = ROUNDUP(totalSz, guardSz);
    
    void* mem;
    if (top == 0 || ((*top) + totalSz) >= lim) {
        mem = MapMemory(0, totalSz);
        
        if(mem == MAP_FAILED) {
            return 1;
        }
    } else {
        mem = *top;
        uint64_t asI = (uint64_t)mem;
        mem = (void*) (ROUNDUP(asI, guardSz));
        *top = mem + totalSz;
    }
    
    // we protect the low end of the block to
    // detect stack overflow. this is done manually
    // because mmap on OS X seems to only place a protected
    // page after the buffer, not before it.
    if(mprotect(mem, guardSz, PROT_NONE)) {
        // failed to initialize guard area.
        return 2;
    }
    
    uint64_t val = (uint64_t) mem;
    
    // initialize the stack's info descriptor
    info = (StackInfo_t*)(val + stackLen);
    info->mmapBase = mem;
    info->mmapSize = totalSz;
    info->deepestScan = info;
    info->age = AGE_Minor;
    info->next = NULL;
    info->prev = NULL;
    info->prevSegment = NULL;
    info->currentSP = NULL;
    
    // setup stack pointer
    val = val + stackLen - 16;		// switch sides, leaving some headroom.
    val = ROUNDDOWN(val, 16ULL);	// realign downwards.
        
    uint8_t* valP = (uint8_t*)val;
    
    // push an invalid frame size
    valP -= sizeof(uint64_t);
    *((uint64_t*)valP) = ~0ULL;
    
    // push a dummy watermark
    valP -= sizeof(uint64_t);
    *((uint64_t*)valP) = AGE_Global;
    
    // leave space for a return addr
    valP -= sizeof(uint64_t);
    
	void* sp = (void*)valP;
    void* spLim = (void*)(valP - numBytes);
    
    info->initialSP = sp;
    info->stkLimit = spLim;
    
    return info;
}

// void FreeStack(StackInfo_t* info) {
//     UnmapMemory(info->mmapBase, info->mmapSize);
// }
