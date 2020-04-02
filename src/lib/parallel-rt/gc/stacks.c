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

    numBytes = ROUNDUP(numBytes, 16ULL);
    size_t ccallSz = isSegment && haveGuardPage ? (8 * ONE_K) : 0; // 8KB ought to be enough for anybody (tm)
    size_t bonusSz = 2 * sizeof(uint64_t); // extra space for realigning, etc.

    size_t totalRegion = ccallSz + slopSz + numBytes + bonusSz;
    size_t stackLen = guardSz + totalRegion;
    size_t totalSz = stackLen + sizeof(StackInfo_t);

    totalSz = haveGuardPage ? ROUNDUP(totalSz, guardSz) : ROUNDUP(totalSz, 16ULL);

    uint8_t* mem = NULL;

    if (haveGuardPage) {
        // we protect the low end of the block to
        // detect stack overflow.

        mem = lo_alloc_aligned(vp, totalSz, guardSz);

        if (mem == NULL) {
            Die("AllocStackMem: unable to allocate aligned memory.");
            return NULL;
          }

        if (mprotect(mem, guardSz, PROT_NONE)) {
            perror("AllocStackMem errono msg");
            Die("mprotect");
            return NULL;
          }
    } else {
        mem = lo_alloc(vp, totalSz);

        if (mem == NULL) {
          Die("AllocStackMem: unable to allocate memory.");
          return NULL;
        }
    }

    uint64_t val = (uint64_t) mem;

    // initialize the stack's info descriptor
    info = (StackInfo_t*)(val + stackLen);
    info->deepestScan = 0;
    info->age = AGE_Minor;
    info->next = NULL;
    info->prev = NULL;
    info->prevSegment = NULL;
    info->currentSP = NULL;
    info->owner = vp;
    info->canCopy = 1; // default is to allow copying.
    info->context = 0; // an INVALID context.
    info->guardSz = guardSz;
    info->usableSpace = numBytes;
    info->memAlloc = mem;
    MutexInit(&info->gcLock);
    #ifndef NO_GC_STATS
      info->totalSz = totalSz;
    #endif

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
    *((uint64_t*)valP) = (uint64_t)&EndOfStack; // write the stack-end ret addr

    uint8_t* sp = (uint8_t*)valP;
    uint8_t* spLim = (uint8_t*)(valP - numBytes);

    assert((uint64_t)sp % 8 == 0 && "SP must start 8-byte aligned");
    assert((uint64_t)spLim % 8 == 0 && "SP limit must start 8-byte aligned");

    info->initialSP = sp;
    info->stkLimit = spLim;

    return info;
}

// the vp does _not_ need to be the owner of the stack segment.
void DeallocateStackMem(VProc_t *vp, StackInfo_t* info) {
    size_t guardSz = info->guardSz;
    uint8_t* mem = info->memAlloc;

    if (guardSz) {
      // clear protections on the guard page.
      if (mprotect(mem, guardSz, PROT_READ | PROT_WRITE | PROT_EXEC))
        Die("DeallocateStackMem failed to clear the guard page.");
    }

    #ifndef NO_GC_STATS
      vp->largeObjStats.nBytesCollected += info->totalSz;
    #endif

    lo_free(vp, mem);
}

// returns a stack pointer SP such that SP+8 is 16-byte aligned.
uint8_t* AllocFFIStack(VProc_t *vp, size_t numBytes) {
    StackInfo_t* ffiInfo = AllocStackMem(vp, numBytes, GUARD_PAGE_BYTES, false);
    return ffiInfo->initialSP;
}





#ifdef DIRECT_STYLE

extern int ASM_DS_Return;
extern int ASM_DS_StartStack;
extern int ASM_DS_EscapeThrow;
extern int ASM_DS_SegUnderflow;


void ResetSegment(VProc_t* vp, StackInfo_t* info) {
  // reset some fields
  info->deepestScan = 0;
  info->age = AGE_Minor;
  info->prevSegment = NULL;
  info->currentSP = NULL;
  info->owner = vp;
  info->canCopy = 1; // default is to allow copying.
}

// Checks for and retrieves a stack from the VProc's local free-stack cache
// or its global free-stack cache.
// Returns NULL is there are no available stacks that meet the required size.
#if defined(RESIZESTACK)
ALWAYS_INLINE StackInfo_t* CheckFreeStacks(VProc_t *vp, size_t requiredSpace) {
  #define NUM_SOURCES 2
  StackInfo_t** sources[NUM_SOURCES] = { &vp->freeStacks,
                                         &GlobFreeStacks[vp->id].top };

  for (int i = 0; i < NUM_SOURCES; ++i) {
    StackInfo_t** listTop = sources[i];

    // Use a basic first-fit strategy (without splitting).
    // We giveup after a certian number since the free list may be long
    size_t checked = 0;
    StackInfo_t* prev = NULL;
    StackInfo_t* cur = *listTop;

    while (cur != NULL && checked < FIRST_FIT_MAX_CHK) {
      if (cur->usableSpace >= requiredSpace) {

        // unlink cur
        if (prev == NULL)
          *listTop = cur->next;
        else
          prev->next = cur->next;

        ResetSegment(vp, cur);
        return cur;
      }

      // advance
      prev = cur;
      cur = cur->next;
      checked++;
    }
  }

  return NULL;
  #undef NUM_SOURCES
}


#else

ALWAYS_INLINE StackInfo_t* CheckFreeStacks(VProc_t *vp, size_t requiredSpace) {
  // Free-lists all contain stacks all of the same usable size.
  #define NUM_SOURCES 2
  StackInfo_t** sources[NUM_SOURCES] = { &vp->freeStacks,
                                         &GlobFreeStacks[vp->id].top };

  for (int i = 0; i < NUM_SOURCES; ++i) {
    StackInfo_t** listTop = sources[i];
    StackInfo_t* info = *listTop;
    if (info != NULL) {
      // pop the segment
      *listTop = info->next;

      ResetSegment(vp, info);

      assert(info->usableSpace == requiredSpace && "expected uniform sizes!");

      // Say("Stack-cache hit at level %i\n", i);
      return info;
    }
  }

  // Say("Stack-cache miss by vproc %i.\n", vp->id);
  return NULL;

  #undef NUM_SOURCES
}
#endif

// Retrieves an unused stack for the given vproc.
StackInfo_t* GetStack(VProc_t *vp, size_t usableSpace) {
  #ifndef NO_GC_STATS
      vp->stkCacheReqs++;
  #endif

    StackInfo_t* info = CheckFreeStacks(vp, usableSpace);

    if (info == NULL) {
        // Allocate new memory for this stack.
        #ifndef NO_GC_STATS
            vp->stkCacheMisses++;
        #endif
        bool isSegment = false;
  #if defined(SEGSTACK) || defined(RESIZESTACK)
        isSegment = true;
  #endif

        size_t guardSz = FFIStackFlag && isSegment ? 0 : GUARD_PAGE_BYTES;
        info = AllocStackMem(vp, usableSpace, guardSz, isSegment);

    #ifndef NDEBUG
        // overwrite the contents
        uint64_t right = ((uint64_t) info->initialSP) + 8; // cause we left space for RA
        uint64_t left = (uint64_t) info->stkLimit + 32; // segfaults if we're too close to limit.
        assert(left < right && "segment limits are backwards");
        assert(right % 16ULL == 0 && "bad alignment");
        assert(left % 8ULL == 0 && "bad alignment");
        uint64_t bytes = right - left;
        memset((void*)left, (unsigned char) 0, bytes);
    #endif


} // end of alloc new memory

    // push on alloc'd list
    StackInfo_t* oldTop = vp->allocdStacks;
    if (oldTop != NULL) {
        assert(oldTop->prev == NULL && "malformed list");
        oldTop->prev = info;
    }
    info->next = oldTop;
    info->prev = NULL;

    vp->allocdStacks = info;

    return info;
}

StackInfo_t* NewStackForClos(VProc_t *vp, Value_t funClos) {
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
    sp[-3] = AGE_Minor;  // watermark.
    sp[-4] = (uint64_t)&ASM_DS_StartStack;
    sp = sp - 4;

    info->currentSP = sp;

    return info;
}

// NOTE: exposed to BOM code.
Value_t NewStack (VProc_t *vp, Value_t funClos) {
  #ifndef NO_GC_STATS
      TIMER_Start(&(vp->largeObjStats.timer));
  #endif

    StackInfo_t* info = NewStackForClos(vp, funClos);
    uint64_t* sp = info->currentSP;

  #if defined(NOSEALING_CAPTURE)
    // context is the tuple (vproc ID, vproc unique ticket)
    // We combine this into one value by placing the vproc id
    // in the upper bits of the context value.
    const int log2_num_tickets = 40; // 2^n max tickets, 2^(64-n) max vprocs.
    info->context = vp->nextStackContext | (((uint64_t)vp->id) << log2_num_tickets);
    vp->nextStackContext += 1;
  #endif

    // now we need to allocate the stack cont object
    Value_t resumeK = AllocStkCont(vp, (Addr_t)&ASM_DS_EscapeThrow,
                                        PtrToValue(sp), // stack ptr
                                        PtrToValue(info)); // stack info

    #ifndef NO_GC_STATS
        TIMER_Stop(&(vp->largeObjStats.timer));
    #endif
    return resumeK;
}


StackInfo_t* NewMainStack (VProc_t* vp, void** initialSP) {
  #ifndef NO_GC_STATS
      TIMER_Start(&(vp->largeObjStats.timer));
  #endif
    StackInfo_t* info = GetStack(vp, dfltStackSz);

    #if defined(NOSEALING_CAPTURE)
      // see NewStack for details.
      const int log2_num_tickets = 40; // 2^n max tickets, 2^(64-n) max vprocs.
      info->context = vp->nextStackContext | (((uint64_t)vp->id) << log2_num_tickets);
      vp->nextStackContext += 1;
    #endif

    // initialize stack for a return from manticore's main fun.
    void* stkPtr = info->initialSP;
    uint64_t* ptrToRetAddr = (uint64_t*)stkPtr;
    *ptrToRetAddr = (uint64_t)&ASM_DS_Return;

    // return values
    *initialSP = stkPtr;
  #ifndef NO_GC_STATS
      TIMER_Stop(&(vp->largeObjStats.timer));
  #endif
    return info;
}

void* GetStkLimit(StackInfo_t* info) {
    return info->stkLimit;
}

void* GetCurrentSP(StackInfo_t* info) {
    return info->currentSP;
}

void SetCanCopy(StackInfo_t* info, uint64_t val) {
    info->canCopy = val;
}

void WarmUpFreeList(VProc_t* vp, uint64_t numElms) {
    uint64_t N = numElms;
    // make sure we allocate at least one.
    N = (N == 0 ? 1 : N);

    StackInfo_t* info;
    bool isSegment = false;
#if defined(SEGSTACK) || defined(RESIZESTACK)
    isSegment = true;
#endif
    size_t guardSz = FFIStackFlag && isSegment ? 0 : GUARD_PAGE_BYTES;
    for(uint64_t i = 0; i < N; i++) {
        info = AllocStackMem(vp, dfltStackSz, guardSz, isSegment);

        // push
        info->next = vp->freeStacks;
        vp->freeStacks = info;
    }
}

// NOTE: called by ASM code
void TakeOwnership(VProc_t* vp, StackInfo_t* segment) {
  #ifndef NO_GC_STATS
      TIMER_Start(&(vp->largeObjStats.timer));
  #endif

  for (StackInfo_t* cur = segment; cur != NULL; cur = cur->prevSegment) {
    if (cur->owner != NULL)
      Die("invalid segment to take ownership of!");

    // critical section for modifying the GlobAllocdList
    if (NumVProcs > 1) MutexLock(&GlobStackMutex);
      RemoveFromAllocList(&GlobAllocdList, cur);
    if (NumVProcs > 1) MutexUnlock(&GlobStackMutex);

    // push it onto this vproc's allocdStacks list
    StackInfo_t* oldTop = vp->allocdStacks;
    if (oldTop != NULL)
      oldTop->prev = cur;

    cur->prev = NULL;
    cur->next = oldTop;
    vp->allocdStacks = cur;

    // update ownership status
    cur->owner = vp;
    // cur->age = AGE_Minor;  // QUESTION: is this correct?
  } // end loop

  #ifndef NO_GC_STATS
      TIMER_Stop(&(vp->largeObjStats.timer));
  #endif

  return;
}


#if defined(SEGSTACK)

/////////////////////////////////////////////////////////////////////////////
///////////////// SEGMENTED STACKS /////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////

// In this SEGMENTED STACKS version, we copy a bounded number of frames,
// or none at all.
// NOTE: called by ASM code
uint8_t* StkSegmentOverflow (VProc_t* vp, uint8_t* old_origStkPtr, uint64_t shouldCopy) {
  #ifndef NO_GC_STATS
      TIMER_Start(&(vp->largeObjStats.timer));
  #endif

    StackInfo_t* fresh = GetStack(vp, dfltStackSz);
    StackInfo_t* old = (StackInfo_t*) (vp->stdCont);

    uint64_t bytesSeen = 0;

    if (shouldCopy) {

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
            uint64_t* p = (uint64_t*)(old_origStkPtr + bytesSeen + szOffset);
            uint64_t sz = *p;

            // hit the end of the segment?
            if(sz == ~0ULL) {
                // copying the whole segment to the new one defeats the
                // purpose of this optimization, so
                // we will simply provide an empty segment.
                bytesSeen = 0;
                break;
            }

            uint64_t frameBytes = sz + sizeof(uint64_t);

            if (bytesSeen + frameBytes >= maxBytes) {
                // do not include this frame.
                // it would put us over the max.
                break;
            }

            // include this frame
            bytesSeen += frameBytes;
        }
    }

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

    if (bytesSeen) {
        // pull pointer down
        newStkPtr -= bytesSeen;

        memcpy(newStkPtr, old_origStkPtr, bytesSeen);
    }

    uint8_t* old_stkPtr = old_origStkPtr + bytesSeen;

    // initialize backwards link and save old segment's new top
    fresh->prevSegment = old;
    old->currentSP = old_stkPtr;

    // carry-over the root context
    fresh->context = old->context;

    // install the fresh segment as the current stack descriptor
    vp->stdCont = PtrToValue(fresh);
    vp->stdEnvPtr = fresh->stkLimit;

    #ifndef NO_GC_STATS
        TIMER_Stop(&(vp->largeObjStats.timer));
    #endif

    // return the new SP in the new segment
    return newStkPtr;
}




#elif defined(RESIZESTACK) && !defined(HYBRIDSTACK)

/////////////////////////////////////////////////////////////////////////////
///////////////// RESIZING STACKS /////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////

// on overflow we RESIZE the stack and discard the old one.
// In the case of callec, we link a new segment instead.
// NOTE: called by ASM code
uint8_t* StkSegmentOverflow (VProc_t* vp, uint8_t* old_origStkPtr, uint64_t shouldCopy) {
  #ifndef NO_GC_STATS
      TIMER_Start(&(vp->largeObjStats.timer));
  #endif

  StackInfo_t* old = (StackInfo_t*) (vp->stdCont);

  #ifdef NOSEALING_CAPTURE
    size_t newSize = old->usableSpace * 2;
  #else
    // prevent unbounded growth by NOT doubling on overflow for a sealing capture.
    size_t newSize = shouldCopy ? old->usableSpace * 2 : dfltStackSz;
  #endif

  // the size we grow to is capped.
  if (newSize > RESIZED_SEG_LIMIT) {
    newSize = RESIZED_SEG_LIMIT;
    shouldCopy = false;
  }

  assert(newSize >= dfltStackSz);
  StackInfo_t* fresh = GetStack(vp, newSize);


  ////////
  // setup the new stack segment

  uint8_t* newStkPtr = fresh->initialSP;

  if (shouldCopy) {
    // copy everything over and discard old segment.
    uint8_t* oldBase = old->initialSP;
    uint64_t numBytes = oldBase - old_origStkPtr;

    // pull down the stack ptr to same point as old stack's top
    newStkPtr -= numBytes;

    // +8 to include the retAddr
    memcpy(newStkPtr, old_origStkPtr, numBytes + 8);

    // initialize other fields.
    fresh->age = old->age;
    fresh->prevSegment = old->prevSegment;
    fresh->canCopy = old->canCopy;
    fresh->context = old->context;

    if (old->owner == vp) {
      old = ReleaseOneStack(vp, &(vp->allocdStacks), old, false);  // add back to cache, it's hot
      assert(old == NULL && "if failed, then stack memory would leak");
    }

  } else {
    // link a new segment

    // initialize backwards link and its underflow handler.
    *((uint64_t*)newStkPtr) = (uint64_t)(&ASM_DS_SegUnderflow);
    fresh->prevSegment = old;
    old->currentSP = old_origStkPtr;

    // carry-over the root context
    fresh->context = old->context;
  }

  // install the fresh segment as the current stack descriptor
  vp->stdCont = PtrToValue(fresh);
  vp->stdEnvPtr = fresh->stkLimit;

  #ifndef NO_GC_STATS
      TIMER_Stop(&(vp->largeObjStats.timer));
  #endif

  // return the new SP in the new segment
  return newStkPtr;
}


#elif defined(HYBRIDSTACK)

/////////////////////////////////////////////////////////////////////////////
///////////////// HYBRID STACKS /////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////

uint8_t* StkSegmentOverflow (VProc_t* vp, uint8_t* old_origStkPtr, uint64_t shouldCopy) {
  #ifndef NO_GC_STATS
      TIMER_Start(&(vp->largeObjStats.timer));
  #endif

  enum Decision { Resize, Append, StartFresh };

  StackInfo_t* old = (StackInfo_t*) (vp->stdCont);
  enum Decision D;

  size_t oldSize = old->usableSpace;
  const size_t normalSize = hybridThresholdSz;
  const size_t doubled = oldSize * 2;
  size_t newSize = (doubled < normalSize) ? doubled : normalSize;

  if (!shouldCopy) {
    D = StartFresh; // act like a seg stack that cannot copy frames
  } else if (oldSize < normalSize) {
    D = Resize; // act like a resizing stack, growing to at most the threshold size.
  } else {
    D = Append; // the usual case where we act like a seg stack that can copy.
  }

  assert(newSize >= dfltStackSz);
  StackInfo_t* fresh = GetStack(vp, newSize);
  uint8_t* newStkPtr = fresh->initialSP;


  if (D == StartFresh) {
    // create a fresh stack segment with the default size and append it

    // initialize backwards link and its underflow handler.
    *((uint64_t*)newStkPtr) = (uint64_t)(&ASM_DS_SegUnderflow);
    fresh->prevSegment = old;
    old->currentSP = old_origStkPtr;

    // carry-over the root context
    fresh->context = old->context;


    ///////////////////////////////
  } else if (D == Resize) {
    // copy everything over to the resized segment and discard old segment.
    uint8_t* oldBase = old->initialSP;
    uint64_t numBytes = oldBase - old_origStkPtr;

    // pull down the stack ptr to same point as old stack's top
    newStkPtr -= numBytes;

    // +8 to include the retAddr
    memcpy(newStkPtr, old_origStkPtr, numBytes + 8);

    // initialize other fields.
    fresh->age = old->age;
    fresh->prevSegment = old->prevSegment;
    fresh->canCopy = old->canCopy;
    fresh->context = old->context;

    if (old->owner == vp) {
      old = ReleaseOneStack(vp, &(vp->allocdStacks), old, false);  // add back to cache, it's hot
      assert(old == NULL && "if failed, then stack memory would leak");
    }


    ///////////////////////////////
  } else {
    assert(D == Append);

    uint64_t bytesSeen = 0;
    const uint64_t maxBytes = dfltStackSz / 8;
    const int maxFrames = 4; // TODO make this a parameter of the compiler
    const uint64_t szOffset = 2 * sizeof(uint64_t);

    for(int i = 0; i < maxFrames; i++) {
      // grab the size field
      uint64_t* p = (uint64_t*)(old_origStkPtr + bytesSeen + szOffset);
      uint64_t sz = *p;

      // hit the end of the segment?
      if(sz == ~0ULL) {
          // copying the whole segment to the new one defeats the
          // purpose of this optimization, so
          // we will simply provide an empty segment.
          bytesSeen = 0;
          break;
      }

      uint64_t frameBytes = sz + sizeof(uint64_t);

      if (bytesSeen + frameBytes >= maxBytes) {
          // do not include this frame.
          // it would put us over the max.
          break;
      }

      // include this frame
      bytesSeen += frameBytes;
    }

    // install underflow handler
    *((uint64_t*)newStkPtr) = (uint64_t)(&ASM_DS_SegUnderflow);

    if (bytesSeen) {
        // pull pointer down
        newStkPtr -= bytesSeen;

        memcpy(newStkPtr, old_origStkPtr, bytesSeen);
    }

    uint8_t* old_stkPtr = old_origStkPtr + bytesSeen;

    // initialize backwards link and save old segment's new top
    fresh->prevSegment = old;
    old->currentSP = old_stkPtr;

    // carry-over the root context
    fresh->context = old->context;
  }



  // install the fresh segment as the current stack descriptor
  vp->stdCont = PtrToValue(fresh);
  vp->stdEnvPtr = fresh->stkLimit;

  #ifndef NO_GC_STATS
      TIMER_Stop(&(vp->largeObjStats.timer));
  #endif

  // return the new SP in the new segment
  return newStkPtr;
}


#endif // Segment Overflow versions

#endif // DIRECT_STYLE
