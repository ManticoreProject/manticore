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


/* determine guard size */
#ifdef _SC_PAGESIZE
# define GUARD_PAGE_BYTES _SC_PAGESIZE
#else /* no _SC_PAGESIZE, even though unistd.h defines it. */
# define GUARD_PAGE_BYTES 4096
#endif /* _SC_PAGESIZE */


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

// allocates a region of memory suitable for
// use as a stack. Note that it returns the base
// of the block so it can be freed, you must prepare
// the stack before use. Returns 0 if failed.
void* AllocStack(size_t numBytes) {
    
    // NOTE automatic resizing using MAP_GROWSDOWN has
	// been deprecated: https://lwn.net/Articles/294001/
    
	size_t guardSz = GUARD_PAGE_BYTES;
    
    void* mem = MapMemory(0, numBytes + guardSz);
    
    if(mem == MAP_FAILED) {
        return 0;
    }
    
    // we protect the low end of the block to
    // detect stack overflow. this is done manually
    // because mmap on OS X seems to only place a protected
    // page after the buffer, not before it.
    if(mprotect(mem, guardSz, PROT_NONE)) {
        // failed to initialize guard area.
        return 0;
    }
    
    return mem;
}

void FreeStack(void* base, size_t numBytes) {
    size_t guardSz = GUARD_PAGE_BYTES;
    
    UnmapMemory(base, numBytes + guardSz);
}


// Takes a region of memory from AllocStack and returns a pointer suitable
// for use as a stack pointer into that region of memory, ignoring any
// data already in the region.
//
// The pointer p returned is guarenteed to be such that p+8 is 
// 16-byte aligned, per the SysV ABI. The pointer returned
// is ready to be used as a stack pointer after writing a ret addr.
// Here's a picture (where numBytes is approximate):
//
//                            16-byte aligned
//                                   v
// | guard |  numBytes-ish  |bbbbbbbb|  high addresses >
// ^                        ^
// base ptr             returned ptr 
//
void* GetStackPtr(void* base, size_t numBytes) {
    size_t guardSz = GUARD_PAGE_BYTES;
    
    uint64_t val = (uint64_t) base;
    uint64_t len = numBytes + guardSz;
    
    val = val + len - 8;			// switch sides, leaving some headroom.
    val = ROUNDDOWN(val, 16ULL);	// realign downwards.
    val = val - 8;					// make space for return addr.
    
	void* sp = (void*)val;

	return sp;
}
