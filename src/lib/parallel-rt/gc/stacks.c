/* stacks.c
 *
 * utilities for initializing and allocating stacks.
 *
 */



#include "manticore-rt.h"
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include "os-memory.h"
#include "heap.h"
#include "gc.h"
#include "internal-heap.h"
#include "value.h"
#include "string.h"
#include "large-object.h"
#include <stdio.h>

// Allocates a region of memory suitable for use as a stack segment.
//
// Returns the pointer to the descriptor information of the stack.
//
// The stack pointer initialSP returned is guarenteed to be such that p+8 is
// 16-byte aligned, per the SysV ABI. That pointer is ready to be used as a
// stack pointer after writing a ret addr.
// Here's a picture:
//
//                 16-byte aligned --| |-- dummy watermark
//                                   v v
// | guard |  STACK_REGION  |bbbbbbbb| mark | ~0 | ... StackInfo_t ... |  high addresses >
//                          ^               ^
//                   info->initialSP     invalid frame size
//
//  where STACK_REGION looks like this:
//
//                  info->stkLimit
//                        v
//  | C stack area | slop | usable stack space |
//                                 ^
//                             numBytes
//
// Note that the guard page is omitted if guardSz is 0.
//
// In addition, if isSegment is false, then the C Stack Area
// has size 0. Otherwise their sizes are fixed (see implementation).
//
StackInfo_t* AllocStackMem(VProc_t *vp, size_t numBytes, size_t guardSz, bool isSegment) {
    StackInfo_t* info;
    bool haveGuardPage = guardSz > 0;
    // NOTE automatic resizing using MAP_GROWSDOWN has
    // been deprecated: https://lwn.net/Articles/294001/

    // According to the LLVM codegen, only 128 bytes of space exists.
    // We add a bit more for safety.
    size_t slopSz = isSegment ? 128 + 16 : 0;

    size_t ccallSz = isSegment && haveGuardPage ? 8192 : 0; // 8KB ought to be enough for anybody (tm)
    size_t bonusSz = 8 * sizeof(uint64_t); // extra space for realigning, etc.

    size_t totalRegion = ccallSz + slopSz + numBytes + bonusSz;
    size_t stackLen = guardSz + totalRegion;
    size_t totalSz = stackLen + sizeof(StackInfo_t);

    totalSz = haveGuardPage ? ROUNDUP(totalSz, guardSz) : totalSz;

    uint8_t* mem = NULL;

    if (haveGuardPage) {
        // we protect the low end of the block to
        // detect stack overflow.

        mem = lo_alloc_aligned(vp, totalSz, guardSz);

        if (mprotect(mem, guardSz, PROT_NONE)) {
            Die("AllocStackMem: failed to initialize guard area");
            return NULL;
          }
    } else {
        mem = lo_alloc(vp, totalSz);
    }

    if (mem == NULL) {
      Die("AllocStackMem: unable to allocate memory.");
      return NULL;
    }

    uint64_t val = (uint64_t) mem;

    // initialize the stack's info descriptor
    info = (StackInfo_t*)(val + stackLen);
    info->deepestScan = info;
    info->age = AGE_Minor;
    info->next = NULL;
    info->prev = NULL;
    info->prevSegment = NULL;
    info->currentSP = NULL;
    info->owner = vp;
    info->guardSz = guardSz;
    info->usableSpace = numBytes;
    info->memAlloc = mem;

    // setup stack pointer
    val = val + stackLen - 16;        // switch sides, leaving some headroom.
    val = ROUNDDOWN(val, 16ULL);    // realign downwards.

    uint8_t* valP = (uint8_t*)val;

    // push an invalid frame size
    valP -= sizeof(uint64_t);
    *((uint64_t*)valP) = ~0ULL; // this value is checked for by segment overflow

    // push a dummy watermark
    valP -= sizeof(uint64_t);
    *((uint64_t*)valP) = AGE_Global;

    // leave space for a return addr
    valP -= sizeof(uint64_t);

    uint8_t* sp = (uint8_t*)valP;
    uint8_t* spLim = (uint8_t*)(valP - numBytes);

    info->initialSP = sp;
    info->stkLimit = spLim;

    return info;
}

// returns a stack pointer SP such that SP+8 is 16-byte aligned.
uint8_t* AllocFFIStack(VProc_t *vp, size_t numBytes) {
    StackInfo_t* ffiInfo = AllocStackMem(vp, numBytes, GUARD_PAGE_BYTES, false);
    return ffiInfo->initialSP;
}

void InvalidReturnAddr() {
    Die("an unexpected return has occurred!");
}

void EndOfStack() {
    Die("unexpected stack underflow has occurred!");
}



#ifdef DIRECT_STYLE

extern int ASM_DS_Return;
extern int ASM_DS_StartStack;
extern int ASM_DS_EscapeThrow;
extern int ASM_DS_SegUnderflow;

// Checks for and retrieves a stack from the VProc's local free-stack cache.
// Returns NULL is there are no available stacks that meet the required size.
#if defined(RESIZESTACK)
ALWAYS_INLINE StackInfo_t* CheckFreeStacks(VProc_t *vp, size_t requiredSpace) {
  // TODO
  return NULL;
}

#else

ALWAYS_INLINE StackInfo_t* CheckFreeStacks(VProc_t *vp, size_t requiredSpace) {
  // Free-list contains stacks all of the same usable size.
  StackInfo_t* info = NULL;
  if (vp->freeStacks != NULL) {
    // pop an existing stack
    info = vp->freeStacks;
    vp->freeStacks = info->next;
    assert(info->usableSpace == requiredSpace && "expected uniform sizes!");
  }
  return info;
}
#endif

// Retrieves an unused stack for the given vproc.
StackInfo_t* GetStack(VProc_t *vp, size_t usableSpace) {
    StackInfo_t* info = CheckFreeStacks(vp, usableSpace);

    if (info == NULL) {
        // Allocate new memory for this stack.
        size_t guardSz = FFIStackFlag ? 0 : GUARD_PAGE_BYTES;
        bool isSegment = false;
  #if defined(SEGSTACK) || defined(RESIZESTACK)
        isSegment = true;
  #endif
        info = AllocStackMem(vp, usableSpace, guardSz, isSegment);
    }

    // push on alloc'd list
    StackInfo_t* cur = vp->allocdStacks;
    if (cur != NULL) {
        cur->prev = info;
    }
    info->next = cur;
    info->prev = NULL;

    vp->allocdStacks = info;

    return info;
}

// NOTE: exposed to BOM code.
Value_t NewStack (VProc_t *vp, Value_t funClos) {
    StackInfo_t* info = GetStack(vp, dfltStackSz);

    uint64_t* sp = (uint64_t*)(info->initialSP);

    /* we initialize one frame:
        low                                            high
                                                                       16-byte
                     v                        v                           v
        [ &ApplyClos | watermark | frame size | funClos ][ invalidRetAddr ]
        ^                                                ^
  returned stkPtr                                    initial sp

    */
    sp[0] = (uint64_t)&EndOfStack; // funClos should not try to return!
    sp[-1] = (uint64_t)funClos;
    sp[-2] = 24; // 24 bytes, including watermark and frame size
    sp[-3] = 0;  // watermark.
    sp[-4] = (uint64_t)&ASM_DS_StartStack;
    sp = sp - 4;

    // now we need to allocate the stack cont object
    Value_t resumeK = AllocStkCont(vp, (Addr_t)&ASM_DS_EscapeThrow,
                                        PtrToValue(sp), // stack ptr
                                        PtrToValue(info)); // stack info

    return resumeK;
}

StackInfo_t* NewMainStack (VProc_t* vp, void** initialSP) {
    StackInfo_t* info = GetStack(vp, dfltStackSz);

    // initialize stack for a return from manticore's main fun.
    void* stkPtr = info->initialSP;
    uint64_t* ptrToRetAddr = (uint64_t*)stkPtr;
    *ptrToRetAddr = (uint64_t)&ASM_DS_Return;

    // return values
    *initialSP = stkPtr;
    return info;
}

void* GetStkLimit(StackInfo_t* info) {
    return info->stkLimit;
}

void WarmUpFreeList(VProc_t* vp, uint64_t numBytes) {
    uint64_t N = numBytes / dfltStackSz;
    // make sure we allocate at least one.
    N = (N == 0 ? 1 : N);

    StackInfo_t* info;
    size_t guardSz = FFIStackFlag ? 0 : GUARD_PAGE_BYTES;
    bool isSegment = false;
#if defined(SEGSTACK) || defined(RESIZESTACK)
    isSegment = true;
#endif

    for(uint64_t i = 0; i < N; i++) {
        info = AllocStackMem(vp, dfltStackSz, guardSz, isSegment);

        // push
        info->next = vp->freeStacks;
        vp->freeStacks = info;
    }
}

void FreeStackMem(VProc_t *vp, StackInfo_t* info) {
    size_t guardSz = info->guardSz;
    uint8_t* mem = info->memAlloc;

    if (guardSz) {
      // clear protections on the guard page.
      if (mprotect(mem, guardSz, PROT_READ | PROT_WRITE | PROT_EXEC))
        Die("FreeStackMem failed to clear the guard page.");
    }

    lo_free(vp, mem);
}



#if defined(SEGSTACK)

// In this SEGMENTED STACKS version, we copy a bounded number of frames,
// or none at all.
__attribute__ ((hot)) uint8_t* StkSegmentOverflow (VProc_t* vp, uint8_t *restrict old_origStkPtr, uint64_t shouldCopy) {
    StackInfo_t* fresh = GetStack(vp, dfltStackSz);
    StackInfo_t* old = (StackInfo_t*) (vp->stdCont);

    uint8_t* old_stkPtr = old_origStkPtr;

    if (shouldCopy) {

        uint64_t bytesSeen = 0;

        // NOTE what if the default segment size < size of the frame that
        // caused the overflow? Should we take the size as an argument to
        // this function and allocate a segment that is larger if nessecary?
        // This will complicate the free list as segments will have various
        // sizes. I think in practice this is unnessecary since a realistic segment
        // size will always be much larger than any one frame in the program.

        const uint64_t maxBytes = dfltStackSz / 8;
        const int maxFrames = 4; // TODO make this a parameter of the compiler
        const uint64_t szOffset = 2 * sizeof(uint64_t);

        for(int i = 0; i < maxFrames; i++) {
            // grab the size field
            uint64_t* p = (uint64_t*)(old_stkPtr + szOffset);
            uint64_t sz = *p;

            // hit the end of the segment?
            if(sz == ~0ULL) {
                // copying the whole segment to the new one defeats the
                // purpose of this optimization, so
                // we will simply provide an empty segment.
                old_stkPtr = old_origStkPtr;
                break;
            }

            uint64_t frameBytes = sz + sizeof(uint64_t);
            bytesSeen += frameBytes;

            if (bytesSeen >= maxBytes) {
                // do not include this frame.
                // it would put us over the max.
                break;
            }

            // include this frame
            old_stkPtr += frameBytes;
        }
    }

    uint64_t bytesToCopy = old_stkPtr - old_origStkPtr;

    // fprintf(stderr, "copying %llu bytes\n", bytesToCopy);

    // stkPtr now points to the ret addr of the new top of old segment

    /* Goal:
    high addresses                               low addresses
                                        ptrB          ptrA
                                         v             v
        [ &UnderflowHandler ][ remainder | copiedData ]     <- old segment
        [ &UnderflowHandler ][ copiedData ]       <- fresh segment
                                          ^
                                         ptrC
        where:
        ptrA = old_origStkPtr
        ptrB = ptrA + bytesToCopy
        old->currentSP = ptrB
        returned SP = ptrC
    */

    uint8_t* newStkPtr = fresh->initialSP;

    // install underflow handler
    *((uint64_t*)newStkPtr) = (uint64_t)(&ASM_DS_SegUnderflow);

    if (bytesToCopy) {
        // pull pointer down
        newStkPtr -= bytesToCopy;

        // copy frames to fresh segment. realignment should be unnessecary
        // and we know its a multiple of 8 because of alignment
        // bytesToCopy /= 8;
        // uint64_t* to = newStkPtr;
        // uint64_t* from = old_origStkPtr;
        // for(uint64_t i = 0; i < bytesToCopy; i++) {
        //     to[i] = from[i];
        // }

        // memcpy is faster than the loop above
        memcpy(newStkPtr, old_origStkPtr, bytesToCopy);
    }

    // initialize backwards link and save old segment's new top
    fresh->prevSegment = old;
    old->currentSP = old_stkPtr;

    // install the fresh segment as the current stack descriptor
    vp->stdCont = PtrToValue(fresh);
    vp->stdEnvPtr = fresh->stkLimit;

    // return the new SP in the new segment
    return newStkPtr;
}




#elif defined(RESIZESTACK)

// on overflow we RESIZE the stack and discard the old one.
// In the case of callec, we link a new segment instead.
__attribute__ ((hot)) uint8_t* StkSegmentOverflow (VProc_t* vp, uint8_t *restrict old_origStkPtr, uint64_t notCallec) {
  StackInfo_t* old = (StackInfo_t*) (vp->stdCont);

  size_t newSize = notCallec ? old->usableSpace * 2 : dfltStackSz;

  assert(newSize >= dfltStackSz);
  StackInfo_t* fresh = GetStack(vp, newSize);


  ////////
  // setup the new stack segment

  uint8_t* newStkPtr = fresh->initialSP;

  if (notCallec) {
    // printf("resizing to %llu\n", newSize);

    // then we need to copy everything over.
    uint8_t* oldBase = old->initialSP;
    uint64_t numBytes = oldBase - old_origStkPtr;

    // pull down the stack ptr to same point as old stack's top
    newStkPtr -= numBytes;

    // +8 to include the retAddr
    memcpy(newStkPtr, old_origStkPtr, numBytes + 8);

    // initialize other fields.
    fresh->age = old->age;
    fresh->prevSegment = old->prevSegment;

    FreeStackMem(vp, old);

  } else {
    // NOTE: because we don't have a mechanism to recognize
    // pointers to frames within a segment (to update them when
    // moving the frame), we link a new segment on the end for callec.

    // initialize backwards link and its underflow handler.
    *((uint64_t*)newStkPtr) = (uint64_t)(&ASM_DS_SegUnderflow);
    fresh->prevSegment = old;
    old->currentSP = old_origStkPtr;
  }

  // install the fresh segment as the current stack descriptor
  vp->stdCont = PtrToValue(fresh);
  vp->stdEnvPtr = fresh->stkLimit;

  // return the new SP in the new segment
  return newStkPtr;
}

#endif // Segment Overflow versions

#endif // DIRECT_STYLE
