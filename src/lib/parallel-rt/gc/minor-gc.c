/* minor-gc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Minor GCs are local collections of a vproc's allocation space.
 */

#include <strings.h>
#include <stdio.h>

#include "manticore-rt.h"
#include "heap.h"
#include "gc.h"
#include "vproc.h"
#include "value.h"
#include "internal-heap.h"
#include "gc-inline.h"
#include "inline-log.h"
#include "work-stealing-deque.h"
#include "bibop.h"
#include "gc-scan.h"

extern Addr_t   MajorGCThreshold;   /* when the size of the nursery goes below */
                    /* this limit it is time to do a GC. */

#ifdef DIRECT_STYLE
  extern int ASM_DS_Return;
#endif

//ForwardObject of MinorGC
/* Copy an object to the old region */
Value_t ForwardObjMinor (Value_t v, Word_t **nextW)
{
    Word_t  *p = (Word_t *)ValueToPtr(v);
    Word_t  hdr = p[-1];

    if (isForwardPtr(hdr)) {
        return PtrToValue(GetForwardPtr(hdr));
    } else {
        int len = GetLength(hdr);
        assert(len > 0);
#ifdef LINKSTACK
        if (isLinkedFrameHdr(hdr))
          roundUpFwdPtr(nextW, 16);
#endif
        Word_t *newObj = *nextW;
        newObj[-1] = hdr;
        for (int i = 0;  i < len;  i++) {
            newObj[i] = p[i];
        }
        *nextW = newObj+len+1;

        p[-1] = MakeForwardPtr(hdr, newObj);
        return PtrToValue(newObj);
    }

}

#ifndef NDEBUG
static void CheckMinorGC (VProc_t *self, Value_t **roots);
#endif

void ScanStackMinor (
    void* origStkPtr,
    StackInfo_t* stkInfo,
    Addr_t nurseryBase,
    Addr_t allocSzB,
    Word_t **nextW) {

// #define DEBUG_STACK_SCAN_MINOR

#ifdef DEBUG_STACK_SCAN_MINOR
    uint64_t framesSeen = 0;
#endif

    enum LimitState {
        LS_NoMark,
        LS_MarkSeen,
        LS_Stop
    };

  const Age_t promoteGen = AGE_Major;
  enum LimitState state = LS_NoMark;

#if defined(SEGSTACK) || defined(RESIZESTACK)
  stkInfo->currentSP = origStkPtr;

  while (stkInfo != NULL) {

    origStkPtr = stkInfo->currentSP;
#endif // SEGSTACK

    frame_info_t* frame;
    uint64_t stackPtr = (uint64_t)origStkPtr;

    uint64_t deepest = (uint64_t)stkInfo->deepestScan;
    if(deepest <= (uint64_t)origStkPtr) {
        goto nextIter; // this part of the stack has already been scanned.
    }

    stkInfo->deepestScan = origStkPtr; // mark that we've seen this stack

    while (((frame = lookup_return_address(SPTbl, *(uint64_t*)(stackPtr))) != 0)
           && state != LS_Stop) {

#ifdef DEBUG_STACK_SCAN_MINOR
        framesSeen++;
        print_frame(stderr, frame);
#endif

        // step into frame
        stackPtr += sizeof(uint64_t);

        // handle watermark
        uint64_t* watermark = (uint64_t*)stackPtr;

        if (state == LS_MarkSeen) {
            // this is the last frame we'll check.
            state = LS_Stop;
        } else if (*watermark >= promoteGen) {
            // saw the limit in this frame.
            state = LS_MarkSeen;
        } else {
            // overwrite the watermark
            *watermark = promoteGen;
        }

        // process pointers
        for (uint16_t i = 0; i < frame->numSlots; i++) {
            pointer_slot_t slotInfo = frame->slots[i];
            if (slotInfo.kind >= 0) {
                Die("unexpected derived pointer\n");
            }

            Value_t *root = (Value_t *)(stackPtr + slotInfo.offset);
            Value_t p = *root;

            if (isPtr(p) && inAddrRange(nurseryBase, allocSzB, ValueToAddr(p))) {
#ifdef DEBUG_STACK_SCAN_MINOR
                fprintf(stderr, "[slot %u : %p] forward %p --> %p\n", i, root, p, *nextW);
#endif
                *root = ForwardObjMinor(p, nextW);
            }
        } // end for

#ifdef DEBUG_STACK_SCAN_MINOR
        fprintf(stderr, "------------------------------------------\n");
#endif

        // move to next frame
        stackPtr += frame->frameSize;

    } // end while

#ifdef DEBUG_STACK_SCAN_MINOR
 #ifdef DIRECT_STYLE
    uint64_t lastRetAddr = *(uint64_t*)(stackPtr);
    if (lookup_return_address(SPTbl, lastRetAddr) == 0
            && lastRetAddr != (uint64_t)&EndOfStack
            && lastRetAddr != (uint64_t)&ASM_DS_Return)
        Die("Encountered an unexpected return address on the stack: %p\n", (void*)lastRetAddr);
 #endif
#endif

    // the roots have been forwarded out of the nursery.
    // we are careful to make sure the age doesn't go down.
    if(stkInfo->age < promoteGen) {
        stkInfo->age = promoteGen;
    }

nextIter:
#if defined(SEGSTACK) || defined(RESIZESTACK)
    stkInfo = stkInfo->prevSegment;

    #ifdef DEBUG_STACK_SCAN_MINOR
        // end of a stack segment
        fprintf(stderr, "=============================================\n");
    #endif

  } // end stkInfo while
#endif // SEGSTACK

  #ifdef DEBUG_STACK_SCAN_MINOR
          if (framesSeen == 0) {
              Die("MinorGC: Should have seen at least one frame!");
          }
          fprintf(stderr, "###########################################################\n");
  #endif

    return;
}

// Deallocates excess stacks in the cache, with the goal of
// maintaining at least maxCache bytes of usable space, but
// not more than 1 segment's worth of extra space beyond that amount.
//
// If any stack segment's usableSpace > maxSegSz, then it is freed
// no matter what.
int LimitNumStacks(VProc_t *vp, const size_t maxCache, const size_t maxSegSz) {
  int released = 0;
  #ifndef NO_GC_STATS
      TIMER_Start(&(vp->largeObjStats.timer));
  #endif

  size_t usableTot = 0;
  StackInfo_t* cur = vp->freeStacks;
  StackInfo_t* prev = NULL;

  // skip through the first bunch up to total limit, freeing a segment only if
  // its usableSpace is too large
  while (cur != NULL && usableTot < maxCache) {
      size_t usableSpace = cur->usableSpace;
      if (usableSpace > maxSegSz) {
        // drop it
        StackInfo_t* next = cur->next;
        DeallocateStackMem(vp, cur);
        cur = next;
        released++;
      } else {
        // keep it.
        usableTot += usableSpace;

        if (prev != NULL)
          prev->next = cur;
        else
          vp->freeStacks = cur;

        prev = cur;
        cur = cur->next;
      }
  }

  // truncate the free-list.
  if (prev != NULL)
    prev->next = NULL;
  else
    vp->freeStacks = NULL;

  // release the list from cur onwards.
  while (cur != NULL) {
    StackInfo_t* next = cur->next;
    DeallocateStackMem(vp, cur);
    cur = next;
    released++;
  }

  #ifndef NO_GC_STATS
      TIMER_Stop(&(vp->largeObjStats.timer));
  #endif

  return released;
}

bool DebugListContains(StackInfo_t* start, StackInfo_t* elm) {
  StackInfo_t* cur = start;
  while (cur != elm && cur != NULL) {
    cur = cur->next;
    if (cur == start)
      Die("circular list!");
  }

  return cur == elm;
}

void RemoveFromAllocList(StackInfo_t** head, StackInfo_t* elm) {
  assert(DebugListContains(*head, elm) && "trying to remove a stack not in the allocd list given!");

  StackInfo_t *nextS = elm->next;
  StackInfo_t *prevS = elm->prev;

  if (prevS != NULL) {
    assert(prevS->next == elm && "malformed list");
    prevS->next = nextS;
  } else {
    assert(*head == elm && "malformed list");
    *head = nextS;
  }

  if (nextS != NULL) {
    assert(nextS->prev == elm && "malformed list");
    nextS->prev = prevS;
  }

}


// Adds a stack to the VProc's stack cache if owned by that vproc.
//
// Otherwise, depending on whether the stack is being freed during
// a GlobalGC, the stack is either returned or deallocated.
//
// Updates here must also be reflected in asm-glue-ds, since
// ASM_DS_SegUnderflow will also free a stack!
StackInfo_t* ReleaseOneStack(VProc_t *vp, StackInfo_t* allocd, bool GlobalGC) {

  RemoveFromAllocList(&(vp->allocdStacks), allocd);

  // demote
  allocd->age = AGE_Minor;
  allocd->canCopy = 1;
  allocd->prev = NULL;

  if (allocd->owner == vp) {
    // put it on the free list
    // note that we don't bother with prev links
    // on the free list since we never unlink
    // in the middle.
    allocd->next = vp->freeStacks;
    vp->freeStacks = allocd;

  } else if (GlobalGC) {
    return allocd;

  } else {
    // release the memory associated with this stack.
    DeallocateStackMem(vp, allocd);
  }

  return NULL;
}

int RedistributeOrphanStack(int nextVP, StackInfo_t* free) {
  // recycle this stack by pushing this onto one of the vproc's
  // global free list. We use a basic round-robin approach to
  // redistributing stacks.

  // FIXME: not all vprocs are guaranteed to use these stacks at the same rate,
  // so at some point you need to stop adding and deallocate the excess!

  StackInfo_t* old = GlobFreeStacks[nextVP].top;
  free->next = old;
  free->prev = NULL;
  GlobFreeStacks[nextVP].top = free;

  // Say("Distributed stack to %i\n", nextVP);

  return (nextVP + 1) % NumVProcs;
}

/*
 * We perform a pass over the allocated list of stacks,
 * freeing any unmarked stacks who are young enough either to
 * a VProc's stack cache (local / global) or by releasing it totally.
 *
 * Otherwise, marked stacks are unmarked.
 *
 * The value returned indicates the new top of the list of stacks
 * that was initially passed in.
 *
 * This function is also used by later GCs.
 */
StackInfo_t* ReclaimStacks(VProc_t *vp, StackInfo_t* top, Age_t epoch, bool GlobalGCLeader) {
  #ifndef NO_GC_STATS
      TIMER_Start(&(vp->largeObjStats.timer));
  #endif

  // Say("Start of reclaim stacks...");

    int NextVProc = 0;
    StackInfo_t* current = top;
    top = NULL;

    while (current != NULL) {
        StackInfo_t* nextIter = current->next;

        bool marked = (current->deepestScan != current);
        bool safe = current->age <= epoch; // young enough

        if (!marked && safe) {
            // we can free it
            StackInfo_t* maybeStack = ReleaseOneStack(vp, current, GlobalGCLeader);
            if (maybeStack != NULL) {
              if (!GlobalGCLeader) // is it safe to mutate the global free stack list?
                Die("should not have got back a stack if not the global gc leader.");

              NextVProc = RedistributeOrphanStack(NextVProc, maybeStack);
            }
        }

        if (marked) {
          // clear the marking since we're keeping it.
          current->deepestScan = current;
          if (top == NULL)
            top = current;
        }

        // advance position
        current = nextIter;
    }
    #ifndef NO_GC_STATS
        TIMER_Stop(&(vp->largeObjStats.timer));
    #endif

    // Say("done!\n");
    return top;
}

/* MinorGC:
 */
void MinorGC (VProc_t *vp)
{
    Addr_t  nurseryBase = vp->nurseryBase;
    Addr_t  allocSzB = vp->allocPtr - nurseryBase - WORD_SZB;
    Word_t  *nextScan = (Word_t *)(vp->oldTop); /* current top of to-space */
    Word_t  *nextW = nextScan + 1;      /* next object address in to-space */

    //LogMinorGCStart (vp, (uint32_t)allocSzB);
    // LogStartGC(vp); // FIXME logging is a mess right now

#ifndef NO_GC_STATS
    TIMER_Start(&(vp->minorStats.timer));
#endif

    assert (vp->heapBase <= (Addr_t)nextScan);
    assert ((Addr_t)nextScan < vp->nurseryBase);
    assert (vp->nurseryBase < vp->allocPtr);

#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_MINOR) {
        SayDebug("[%2d] Minor GC starting\n", vp->id);
    }
#endif

  /* gather the roots.  The protocol is that the stdCont register holds
   * the return address (which is not in the heap) and that the stdEnvPtr
   * holds the GC root.
   */
    int nWorkStealingRoots = M_NumDequeRoots (vp);
    Value_t *roots[18 + nWorkStealingRoots], **rp;
    rp = roots;
    *rp++ = &(vp->currentFLS);
    *rp++ = &(vp->actionStk);
    *rp++ = &(vp->schedCont);
    *rp++ = &(vp->dummyK);
    *rp++ = &(vp->wakeupCont);
    *rp++ = &(vp->shutdownCont);
    *rp++ = &(vp->rdyQHd);
    *rp++ = &(vp->rdyQTl);
    *rp++ = &(vp->sndQHd);
    *rp++ = &(vp->sndQTl);

#ifdef LINKSTACK
    // protocol is different: stdCont holds the stack pointer
    *rp++ = &(vp->stdCont);

#elif ! defined(DIRECT_STYLE)
    // In the in the CPS-style RTS, the stdEnvPtr is a pointer to the root set.
    *rp++ = &(vp->stdEnvPtr);
#endif

    rp = M_AddDequeEltsToLocalRoots(vp, rp);
    *rp++ = 0;

#ifndef NDEBUG
  /* nullify non-live registers */
    vp->stdArg = M_UNIT;
    vp->stdExnCont = M_UNIT;
#endif

  /* process the roots */
    for (int i = 0;  roots[i] != 0;  i++) {
        Value_t p = *roots[i];

        if (isPtr(p)) {
            if (inAddrRange(nurseryBase, allocSzB, ValueToAddr(p))) {
                *roots[i] = ForwardObjMinor(p, &nextW);
            }
            #if defined(LINKSTACK)
                else {
                    Word_t* ptr = (Word_t*)p;
                    Word_t hdr = ptr[-1];
                    if (isLinkedFrameHdr(hdr)) {
                        // the root is a frame in a later generation.
                        // we don't forward or move the frame, but instead simply scan it,
                        // looking for nursery pointers.

                        // NOTE that we throw away the returned value because we are
                        // not scanning the object adjacent to this frame.
                        minorGCscanLINKFRAMEpointer (ptr, &nextW, allocSzB, nurseryBase);
                    }
                }
            #endif
        }

    }

#ifdef DIRECT_STYLE
    /* stdEnvPtr contains a raw pointer to the top of the current stack. */
    StackInfo_t* stkInfo = (StackInfo_t*)(vp->stdCont);
    void* stkPtr = vp->stdEnvPtr;
    ScanStackMinor(stkPtr, stkInfo, nurseryBase, allocSzB, &nextW);
#endif

  /* scan to space */
    while (nextScan < nextW-1) {

        assert ((Addr_t)(nextW-1) <= vp->nurseryBase);

        Word_t hdr = *nextScan++;

        int id = getID(hdr);
        if (unlikely(id >= tableMaxID))
            Die("MinorGC: invalid header ID!");

        // All objects jump to their table entry function.
        // See minor-gc-scan.c
        nextScan = table[id].minorGCscanfunction(nextScan, &nextW, allocSzB, nurseryBase);

    }

    assert ((Addr_t)nextScan >= vp->heapBase);
    Addr_t avail = VP_HEAP_SZB - ((Addr_t)nextScan - vp->heapBase);

#ifndef NO_GC_STATS
    vp->nMinorGCs++;
    vp->minorStats.nBytesAlloc += vp->allocPtr - vp->nurseryBase - WORD_SZB;
    vp->minorStats.nBytesCollected = vp->minorStats.nBytesAlloc;
    vp->minorStats.nBytesCopied += (Addr_t)nextScan - vp->oldTop;
    vp->majorStats.nBytesAlloc += (Addr_t)nextScan - vp->oldTop;
    TIMER_Stop(&(vp->minorStats.timer));
#endif

#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_MINOR) {
        bzero(nextScan, avail); /* clear unused part of local heap */
        SayDebug("[%2d] Minor GC finished: %ld/%ld bytes live; %d available\n",
                vp->id, (Addr_t)nextScan - vp->oldTop,
                vp->allocPtr - vp->nurseryBase - WORD_SZB,
                (int)avail);
    }
#endif /* !NDEBUG */

    #ifdef DIRECT_STYLE
        /* try to free unreachable stacks */
        vp->allocdStacks = ReclaimStacks(vp, vp->allocdStacks, AGE_Minor, false);
    #endif

    //LogMinorGCEnd (vp, (uint32_t)((Addr_t)nextScan - vp->oldTop), (uint32_t)avail);

    /* remember information about the final state of the heap */
    vp->oldTop = (Addr_t)nextScan;

    if ((avail < MajorGCThreshold) || vp->globalGCPending) {
        /* time to do a major collection. */
        MajorGC (vp, roots, (Addr_t)nextScan);
    }

#ifndef NDEBUG
    if (HeapCheck >= GC_DEBUG_MINOR) {
        CheckMinorGC (vp, roots);
    }
#endif


#if defined(DIRECT_STYLE)
    /* Now that GC is over, thin-out the free-stack cache */
    // int numReleased =
      LimitNumStacks(vp, MAX_STACK_CACHE_SZ, MAX_SEG_SIZE_IN_CACHE);
    // Say("Released %i segments\n", numReleased);
#endif

    /* reset the allocation pointer */
    SetAllocPtr (vp);
    // LogEndGC(vp); // FIXME logging is a mess right now

}

#ifndef NDEBUG
void CheckLocalPtrMinor (VProc_t *self, void *addr, const char *where)
{
    Value_t v = *(Value_t *)addr;
    if (isPtr(v)) {
        MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
        if (cq->sts == TO_SP_CHUNK) {
            return;
        } else if (cq->sts == FROM_SP_CHUNK) {
            SayDebug("CheckLocalPtrMinor: unexpected from-space pointer %p at %p in %s\n",
                        ValueToPtr(v), addr, where);
        } else if (IS_VPROC_CHUNK(cq->sts)) {

            if (cq->sts != VPROC_CHUNK(self->id)) {
                SayDebug("CheckLocalPtrMinor: unexpected remote pointer %p at %p in %s\n",
                    ValueToPtr(v), addr, where);
            } else if (! inAddrRange(self->heapBase, self->oldTop - self->heapBase, ValueToAddr(v))) {
                SayDebug("CheckLocalPtrMinor: local pointer %p at %p in %s is out of bounds\n",
                    ValueToPtr(v), addr, where);
            }

        } else if (cq->sts == FREE_CHUNK) {
            SayDebug("CheckLocalPtrMinor: unexpected free-space pointer %p at %p in %s\n",
            ValueToPtr(v), addr, where);
        }
    }
}

static void CheckMinorGC (VProc_t *self, Value_t **roots)
{

    char buf[32];
    // check the roots
    for (int i = 0;  roots[i] != 0;  i++) {
        sprintf(buf, "root[%d]", i);
        // Value_t v = *roots[i];
        CheckLocalPtrMinor (self, roots[i], buf);
    }

    // check the local heap
    {

    Word_t *top = (Word_t *)(self->oldTop);
    Word_t *p = (Word_t *)self->heapBase;
    while (p < top) {

        Word_t hdr = *p++;
        Word_t *scanptr = p;

        if (isForwardPtr(hdr)) {
          // forward pointer
            Word_t *forwardPtr = GetForwardPtr(hdr);
            CheckLocalPtrMinor(self, forwardPtr, "forward pointer");
            Word_t hdr = forwardPtr[-1];

            p += GetLength(hdr);

        } else {
            tableDebug[getID(hdr)].minorGCdebug(self,scanptr);
            p += GetLength(hdr);
        }
    }

    }

    // check the global heap allocation space
    MemChunk_t  *cp = self->globAllocChunk;
    assert (cp->sts = TO_SP_CHUNK);
    Word_t *p = (Word_t *)(cp->baseAddr);
    Word_t *top = UsedTopOfChunk(self, cp);
    while (p < top) {
        Word_t hdr = *p++;
        Word_t *scanptr = p;

        tableDebug[getID(hdr)].minorGCdebugGlobal(self,scanptr);
        p += GetLength(hdr);
    }
}
#endif /* NDEBUG */
