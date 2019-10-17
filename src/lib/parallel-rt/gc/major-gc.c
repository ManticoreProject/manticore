/* major-gc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A major GC is the process of copying live data from a VProc's local heap
 * to the global heap.  It will occur immediately after a minor GC in the
 * case where the amount of free space falls below some threshold.
 *
 * TODO:
 *    update ToSpaceSize
 */

#include "manticore-rt.h"
#include <string.h>
#include "heap.h"
#include "gc.h"
#include "vproc.h"
#include "gc-inline.h"
#include "internal-heap.h"
#include "inline-log.h"
#ifndef NDEBUG
#include "bibop.h"
#endif
#include "gc-scan.h"
#include <stdio.h>
#include <inttypes.h>

//ForwardObject of MajorGC
/*! \brief Forward an object into the global-heap chunk reserved for the given vp.
 *  \param vp the vproc
 *  \param v  the heap object that is to be forwarded
 *  \return the forwarded value
 */
Value_t ForwardObjMajor (VProc_t *vp, Value_t v)
{
    Word_t    *p = ((Word_t *)ValueToPtr(v));
    Word_t    hdr = p[-1];
    if (isForwardPtr(hdr))
        return PtrToValue(GetForwardPtr(hdr));
    else {
        /* forward object to global heap. */
        Word_t *nextW = (Word_t *)vp->globNextW;
        int len = GetLength(hdr);
        if (nextW+len+1 >= (Word_t *)(vp->globLimit)) {
            AllocToSpaceChunk (vp);
            nextW = (Word_t *)vp->globNextW;
        }
        Word_t *newObj = nextW;
        newObj[-1] = hdr;
        for (int i = 0;  i < len;  i++) {
            newObj[i] = p[i];
        }
        vp->globNextW = (Addr_t)(newObj+len+1);
        p[-1] = MakeForwardPtr(hdr, newObj);

                assert (AddrToChunk(ValueToAddr(v))->sts == FROM_SP_CHUNK ||
                        IS_VPROC_CHUNK(AddrToChunk(ValueToAddr(v))->sts));
                assert (AddrToChunk((Addr_t)newObj)->sts == TO_SP_CHUNK);

        return PtrToValue(newObj);
    }

}

static void ScanMajorToSpace (
    VProc_t *vp, Addr_t heapBase, MemChunk_t *scanChunk, Word_t *scanPtr);
#ifndef NDEBUG
void CheckAfterGlobalGC (VProc_t *self, Value_t **roots);
void CheckToSpacesAfterGlobalGC (VProc_t *vp);
#endif

/* PushToSpaceChunks:
 *
 * Move tospace pages into the global unscanned chunk list. Before entry into the
 * unscanned chunk list, vproc-private global chunks are connected in a linked
 * list with the last entry as the current alloc chunk. This function moves all but
 * the last chunk into the to list.
 */
MemChunk_t *PushToSpaceChunks (VProc_t *vp, MemChunk_t *scanChunk, bool inGlobal) {
    if (scanChunk->next != NULL) {
        int node = LocationNode(vp->location);
        MutexLock (&NodeHeaps[node].lock);
        while (scanChunk->next != NULL) {
            MemChunk_t *tmp = scanChunk;
            scanChunk = scanChunk->next;

#ifndef NDEBUG
            if (GCDebug >= GC_DEBUG_MAJOR ||
                ((GCDebug >= GC_DEBUG_GLOBAL) && inGlobal))
                SayDebug("[%2d]   PushToSpaceChunk %p..%p\n",
                         vp->id, (void *)(tmp->baseAddr),
                         (void *)(tmp->baseAddr+tmp->szB));
#endif
            assert (tmp->sts == TO_SP_CHUNK);

            tmp->next = NodeHeaps[node].unscannedTo;
            NodeHeaps[node].unscannedTo = tmp;
        }

        MutexUnlock (&NodeHeaps[node].lock);
        if (inGlobal)
            CondSignal (&NodeHeaps[node].scanWait);
    }

    return scanChunk;
}


void ScanStackMajor (
    void* origStkPtr,
    StackInfo_t* stkInfo,
    Addr_t heapBase,
    VProc_t *vp) {

// #define DEBUG_STACK_SCAN_MAJOR

#ifdef DEBUG_STACK_SCAN_MAJOR
    uint64_t framesSeen = 0;
#endif

    enum LimitState {
        LS_NoMark,
        LS_MarkSeen,
        LS_Stop
    };

    Age_t promoteGen = AGE_Global;
    enum LimitState state = LS_NoMark;

#if defined(SEGSTACK) || defined(RESIZESTACK)
  stkInfo->currentSP = origStkPtr;

  while (stkInfo != NULL) {

    origStkPtr = stkInfo->currentSP;
#endif // SEGSTACK

    frame_info_t* frame;
    uint64_t stackPtr = (uint64_t)origStkPtr;


    if (vp->inPromotion) {
      // remove this segment from our local list
      RemoveFromAllocList(vp, stkInfo);
      stkInfo->owner = NULL; // it's not owned by anyone in particular now.

      // then push it onto the global allocd list
      MutexLock(&GlobStackMutex);
      StackInfo_t* top = GlobAllocdList;
      GlobAllocdList = stkInfo;

      stkInfo->prev = NULL;
      stkInfo->next = top;

      if (top != NULL)
        top->prev = stkInfo;

      MutexUnlock(&GlobStackMutex);

    } else {
      // only during a GC cycle is it valid to do this test of the deepestScan,
      // because otherwise during a PromoteObj, we never end up clearing this,
      // and will not scan the stack.
        uint64_t deepest = (uint64_t)stkInfo->deepestScan;
        if(deepest <= (uint64_t)origStkPtr) {
            goto nextIter; // this part of the stack has already been scanned.
        }
        stkInfo->deepestScan = origStkPtr; // mark that we've seen this stack
    }

    while (((frame = lookup_return_address(SPTbl, *(uint64_t*)(stackPtr))) != 0)
           && state != LS_Stop) {

#ifdef DEBUG_STACK_SCAN_MAJOR
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
            Value_t newP;

            if (isPtr(p) && inVPHeap(heapBase, ValueToAddr(p))) {
                newP = ForwardObjMajor(vp, p);
                *root = newP;
#ifdef DEBUG_STACK_SCAN_MAJOR
                fprintf(stderr, "[slot %u : %p] forward %p --> %p\n", i, root, p, newP);
#endif
            }

        } // end for

#ifdef DEBUG_STACK_SCAN_MAJOR
        fprintf(stderr, "------------------------------------------\n");
#endif

        // move to next frame
        stackPtr += frame->frameSize;

    } // end while

    // the roots have been forwarded to the global heap
    stkInfo->age = promoteGen;

nextIter:
#if defined(SEGSTACK) || defined(RESIZESTACK)
    stkInfo = stkInfo->prevSegment;

    #ifdef DEBUG_STACK_SCAN_MAJOR
        // end of a stack segment
        fprintf(stderr, "=============================================\n");
    #endif

  } // end stkInfo while
#endif // SEGSTACK

#ifdef DEBUG_STACK_SCAN_MAJOR
        if (framesSeen == 0) {
            Die("MajorGC: Should have seen at least one frame!");
        }
        fprintf(stderr, "##########################################\n");
#endif

    return;
}


/*! \brief Perform a major collection on a vproc's local heap.
 *  \param vp the vproc that is performing the collection.
 *  \param roots a null-terminated array of root addresses.
 *  \param top the address of the top of the old region in
 *         the local heap after the minor GC.
 */
void MajorGC (VProc_t *vp, Value_t **roots, Addr_t top)
{
    Addr_t    heapBase = vp->heapBase;
    Addr_t    oldSzB  __attribute__((unused));
    oldSzB = top - heapBase;
  /* NOTE: we must subtract WORD_SZB here because globNextW points to the first
   * data word of the next object (not the header word)!
   */
    Word_t    *globScan = (Word_t *)(vp->globNextW - WORD_SZB);
    MemChunk_t    *scanChunk = vp->globAllocChunk;

    LogMajorGCStart (vp, (uint32_t)(top - vp->oldTop), (uint32_t)oldSzB);



#ifndef NO_GC_STATS
    vp->nMajorGCs++;
    vp->majorStats.nBytesCollected += top - heapBase;
    TIMER_Start(&(vp->majorStats.timer));
#endif

    assert (heapBase <= vp->oldTop);
    assert (vp->oldTop <= top);

#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_MAJOR)
    SayDebug("[%2d] Major GC starting\n", vp->id);
#endif


#ifdef DIRECT_STYLE

    /* scan the current stack. */
    vp->inPromotion = false;
    StackInfo_t* stkInfo = (StackInfo_t*)(vp->stdCont);
    void* stkPtr = vp->stdEnvPtr;
    ScanStackMajor(stkPtr, stkInfo, heapBase, vp);
#endif

  /* process the roots */
    for (int i = 0;  roots[i] != 0;  i++) {
    Value_t p = *roots[i];
    if (isPtr(p)) {
        if (inVPHeap(heapBase, ValueToAddr(p))) {
            *roots[i] = ForwardObjMajor(vp, p);
        }
    #if defined(LINKSTACK)
        else {
            Word_t* ptr = (Word_t*)p;
            Word_t hdr = ptr[-1];
            if (isLinkedFrameHdr(hdr)) {
                // the root is a frame in a later generation.
                // we don't forward or move the frame, but instead simply scan it
                // NOTE: we throw away the return value because we will not scan
                // the adjacent frame.
                majorGCscanLINKFRAMEpointer(ptr, vp, heapBase);
            }
        }
    #endif
      }
    }

  /* scan to-space objects */
    ScanMajorToSpace (vp, heapBase, scanChunk, globScan);

    Addr_t youngSzB = top - vp->oldTop;

    if (youngSzB != 0)
      Die("The optimization to avoid promoting young data was removed.");

    vp->oldTop = vp->heapBase + youngSzB;

#ifndef NO_GC_STATS
  // compute the number of bytes copied into the global heap
    uint32_t nBytesCopied = 0;
    for (MemChunk_t *p = scanChunk; p != (MemChunk_t *)0;  p = p->next) {
    Addr_t base = (p == scanChunk) ? (Addr_t)globScan - WORD_SZB : p->baseAddr;
    Addr_t tp = (p->next == 0) ? vp->globNextW : p->usedTop;
    nBytesCopied += (tp - base);
    }
    vp->majorStats.nBytesCopied += nBytesCopied + youngSzB;
    vp->globalStats.nBytesAlloc += nBytesCopied;
    TIMER_Stop(&(vp->majorStats.timer));
#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_MAJOR) {
    SayDebug("[%2d] Major GC finished: %d/%" PRIu64 " old bytes copied\n",
        vp->id, nBytesCopied, (uint64_t)oldSzB);
    }
#endif /* !NDEBUG */
#endif /* !NO_GC_STATS */

#ifdef DIRECT_STYLE
    // try to reclaim stacks
    vp->allocdStacks = ReclaimStacks(vp, vp->allocdStacks, AGE_Major, false);
#endif

    PushToSpaceChunks (vp, scanChunk, false);

#ifndef NDEBUG
    if (HeapCheck >= GC_DEBUG_MAJOR) {
    if (GCDebug >= GC_DEBUG_MAJOR)
        SayDebug ("[%2d] Checking heap consistency\n", vp->id);
    bzero ((void *)(vp->oldTop), VP_HEAP_SZB - youngSzB);
    CheckAfterGlobalGC (vp, roots);
        CheckToSpacesAfterGlobalGC (vp);
    }
#endif


    LogMajorGCEnd (vp, nBytesCopied, 0); /* FIXME: nCopiedBytes, nAvailBytes */

    if (vp->globalGCPending || (ToSpaceSz >= ToSpaceLimit)) {
        StartGlobalGC (vp, roots);
    }

} /* end of MajorGC */

/* PromoteObj:
 *
 * Promote an object and anything that it transitively refers to to the
 * global heap.
 */
Value_t PromoteObj (VProc_t *vp, Value_t root)
{
    Addr_t    heapBase = (Addr_t)vp->heapBase;

#ifndef NO_GC_STATS
    vp->nPromotes++;
    TIMER_Start(&(vp->promoteTimer));
#endif

    assert ((vp->globNextW % WORD_SZB) == 0);
#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_ALL)
    SayDebug("[%2d] PromoteObj(%p, %p)\n", vp->id, (void *)vp, (void *)root);
#endif

  /* NOTE: the following test probably ought to happen before the runtime
   * system gets called.
   */
    if (isPtr(root)) {
      if (inVPHeap(heapBase, ValueToAddr(root))) {
        assert (AddrToChunk(ValueToAddr(root))->sts == FROM_SP_CHUNK ||
                IS_VPROC_CHUNK(AddrToChunk(ValueToAddr(root))->sts));

    vp->inPromotion = true;

    MemChunk_t    *scanChunk = vp->globAllocChunk;
    Word_t        *scanPtr = (Word_t *)(vp->globNextW - WORD_SZB);

    assert ((Word_t *)(scanChunk->baseAddr) <= scanPtr);
    assert (scanPtr < (Word_t *)(scanChunk->baseAddr + scanChunk->szB));

      /* promote the root to the global heap */
    root = ForwardObjMajor (vp, root);

      /* promote any reachable values */
    ScanMajorToSpace (vp, heapBase, scanChunk, scanPtr);

#ifndef NO_GC_STATS
    uint64_t nBytesCopied = 0;
    for (MemChunk_t *p = scanChunk; p != (MemChunk_t *)0;  p = p->next) {
        Addr_t base = (p == scanChunk) ? (Addr_t)scanPtr - WORD_SZB : p->baseAddr;
        Addr_t tp = (p->next == 0) ? vp->globNextW : p->usedTop;
        nBytesCopied += (tp - base);
    }
    vp->nBytesPromoted += nBytesCopied;
#endif

    PushToSpaceChunks (vp, scanChunk, false);

#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_ALL)
        SayDebug("[%2d]  ==> %p; %"PRIu64" bytes\n", vp->id, (void *)root, nBytesCopied);
#endif

    } // in VP Heap

#if defined(LINKSTACK)
    else if (isLinkedFrameHdr( ((Word_t*)root)[-1] )) {
        // I'm not sure if this case is possible? Direct access to a reified
        // frame would be required to call promote on a pointer to that frame.
        // Otherwise, just scan the frame using the scanning function. ~kavon
        Die("PromoteObj doesn't know how to handle this case.");
    }
#endif

#ifndef NDEBUG
    else {
      /* check for a bogus pointer */
    MemChunk_t *cq = AddrToChunk(ValueToAddr(root));
    if (cq->sts == TO_SP_CHUNK) {
        /* fall through, returning root later */
    }
/*
    else if ((cq->sts == FROM_SP_CHUNK) && (! GlobalGCInProgress))
        Die("PromoteObj: unexpected from-space pointer %p\n", ValueToPtr(root));
*/
    else if (IS_VPROC_CHUNK(cq->sts)) {
        Die("PromoteObj: unexpected remote pointer %p\n", ValueToPtr(root));
    }
    else if (cq->sts == FREE_CHUNK) {
        Die("PromoteObj: unexpected free-space pointer %p\n", ValueToPtr(root));
    }
    }
#endif

} // is ptr

#ifndef NO_GC_STATS
    TIMER_Stop (&(vp->promoteTimer));
#endif

    return root;

}

/* Scan the objects that have been copied to the global heap */
static void ScanMajorToSpace (
    VProc_t *vp,
    Addr_t heapBase,
    MemChunk_t *scanChunk,
    Word_t *scanPtr)
{
    Word_t    *scanTop = UsedTopOfChunk (vp, scanChunk);

    do {

        bool handlingAlloc = scanChunk == vp->globAllocChunk;

        do {
            while (scanPtr < scanTop) {
                Word_t hdr = *scanPtr++;    // get object header

                int id = getID(hdr);
                if (unlikely(id >= tableMaxID))
                    Die("MajorGC, ScanMajorToSpace: invalid header ID!");

                // All objects jump to their table entry function.
                // See major-gc-scan.c
                scanPtr = table[id].majorGCscanfunction(scanPtr, vp, heapBase);
            }

            if (vp->globAllocChunk == scanChunk) {
                // Continue to iterate on the current data, if we are scanning
                // the same block we are allocating into.
                vp->globAllocChunk->scanProgress = (Addr_t)scanPtr;
                scanTop = (Word_t *)(vp->globNextW - WORD_SZB);
            } else if (handlingAlloc) {
                // Ensure we finished the alloc chunk before moving on. In
                // this case, we have changed the allocation chunk but are
                // not guaranteed to have scanned any objects allocated
                // between the previous scanTop value and the final usedTop.
                scanTop = UsedTopOfChunk (vp, scanChunk);
            }
        } while (scanPtr < scanTop);

        if (scanChunk->next != (MemChunk_t *)0) {
            scanChunk = scanChunk->next;

            if (scanChunk->next == NULL) {
                assert ((scanChunk->baseAddr < vp->globNextW)
                        && (vp->globNextW < scanChunk->baseAddr+scanChunk->szB));
                scanTop = (Word_t *)(vp->globNextW - WORD_SZB);
                scanPtr = (Word_t *)(scanChunk->baseAddr);
            } else {
                scanTop = (Word_t *)(scanChunk->usedTop);
                scanPtr = (Word_t *)(scanChunk->baseAddr);
            }
        }
    } while (scanPtr < scanTop);

}
