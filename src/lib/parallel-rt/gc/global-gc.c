/* global-gc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 */

#include <strings.h>
#include <stdio.h>
#include <inttypes.h>

#include "manticore-rt.h"
#include "gc.h"
#include "vproc.h"
#include "os-threads.h"
#include "os-memory.h"
#include "internal-heap.h"
#include "gc-inline.h"
#include "atomic-ops.h"
#include "bibop.h"
#include "inline-log.h"
#include "work-stealing-deque.h"
#include "gc-scan.h"

#ifdef ENABLE_PERF_COUNTERS
    #include "perf.h"
#endif

static Mutex_t        GCLock;        // Lock that protects the following variables:
static Cond_t        LeaderWait;    // The leader waits on this for the followers
static Cond_t        FollowerWait;    // followers block on this until the leader starts the GC
static volatile int    NReadyForGC;    // number of vprocs that are ready for GC
static volatile bool    GlobalGCInProgress; // true, when a global GC has been initiated
static volatile bool    AllReadyForGC;    // true when all vprocs are ready to start GC
static Barrier_t        GCBarrier0;    // for synchronizing on completion of setup phase
static Barrier_t        GCBarrier1;    // for synchronizing on completion of copying phase
static Barrier_t    GCBarrier2;    // for synchronizing on completion of GC

#if (! defined(NDEBUG)) || defined(ENABLE_LOGGING)
/* summary statistics for global GC */
static uint64_t        FromSpaceSzb __attribute__((aligned(64)));
static uint64_t        NBytesCopied __attribute__((aligned(64)));
#endif

#ifdef ENABLE_LOGGING
uint64_t        GlobalGCUId;    // Unique ID of current global GC
#endif

static void GlobalGC (VProc_t *vp, Value_t **roots);
static void ScanVProcHeap (VProc_t *vp);
static void ScanGlobalToSpace (VProc_t *vp);
#ifndef NDEBUG
void CheckAfterGlobalGC (VProc_t *self, Value_t **roots);
void CheckToSpacesAfterGlobalGC (VProc_t *self);
#endif

/* Forward an object into the global-heap chunk reserved for the current VP */
Value_t ForwardObjGlobal (VProc_t *vp, Value_t v)
{
    Word_t    *p = ((Word_t *)ValueToPtr(v));
    volatile _Atomic Word_t *hdrPtr = (volatile _Atomic Word_t *) p - 1;
    Word_t    oldHdr = AtomicReadWord(hdrPtr);
    if (isForwardPtr(oldHdr)) {
        Value_t v = PtrToValue(GetForwardPtr(oldHdr));
        assert (isPtr(v));
#ifndef NDEBUG
        MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
        if (cq->sts != TO_SP_CHUNK) {
            fprintf(stderr, "[%2d] Value %llx is not in to-space\n", vp->id, (long long unsigned int) v);}
#endif
                assert (AddrToChunk(ValueToAddr(v))->sts == TO_SP_CHUNK);
        return v;
    }
    else {
        // we need to atomically update the header to a forward pointer, so frst
        // we allocate space for the object and then we try to install the forward
        // pointer.
        Word_t *nextW = (Word_t *)vp->globNextW;
        int len = GetLength(oldHdr);
        if (nextW+len+1 >= (Word_t *)(vp->globLimit)) {
            AllocToSpaceChunk (vp);
            nextW = (Word_t *)vp->globNextW;
        }
        // try to install the forward pointer
        Word_t fwdPtr = MakeForwardPtr(oldHdr, nextW);
        Word_t hdr = CompareAndSwapWord_Atomic(hdrPtr, oldHdr, fwdPtr);
        if (oldHdr == hdr) {
            Word_t *newObj = nextW;
            newObj[-1] = hdr;
            for (int i = 0;  i < len;  i++) {
                newObj[i] = p[i];
            }
            vp->globNextW = (Addr_t)(newObj+len+1);

            assert (AddrToChunk(ValueToAddr(v))->sts == FROM_SP_CHUNK ||
                    IS_VPROC_CHUNK(AddrToChunk(ValueToAddr(v))->sts));
            assert (AddrToChunk((Addr_t)newObj)->sts == TO_SP_CHUNK);

            return PtrToValue(newObj);
        }
        else {
            // some other vproc forwarded the object, so return the forwarded
            // object.
            assert (isForwardPtr(hdr));
            assert (AddrToChunk((Addr_t)GetForwardPtr(hdr))->sts == TO_SP_CHUNK);
            return PtrToValue(GetForwardPtr(hdr));
        }
    }

}

// #define DEBUG_STACK_SCAN_GLOBAL

// returns the number of frames encountered
void ScanOneSegmentGlobal (
  void* origStkPtr,
  StackInfo_t* stkInfo,
  VProc_t* vp) {

    ////////////////////////////////////////////////////////
    ///////////// critical section start
    MutexLock(&stkInfo->gcLock);

    uint64_t deepest = (uint64_t)stkInfo->deepestScan;
    if (deepest <= (uint64_t)origStkPtr) {
      // this part of the segment has already been scanned by someone else.

      ///////////////////////////////////////////////////
      ///////////// critical section end
      MutexUnlock(&stkInfo->gcLock);
      return;
    }

    frame_info_t* frame;
    uint64_t stackPtr = (uint64_t)origStkPtr;

    while ((frame = lookup_return_address(SPTbl, *(uint64_t*)(stackPtr))) != 0) {

#ifdef DEBUG_STACK_SCAN_GLOBAL
        print_frame(stderr, frame);
#endif

        // step into frame
        stackPtr += sizeof(uint64_t);

        // everything is being forwarded to another part of global heap
        uint64_t* watermark = (uint64_t*)stackPtr;
        *watermark = AGE_Global;

        // process pointers
        for (uint16_t i = 0; i < frame->numSlots; i++) {
            pointer_slot_t slotInfo = frame->slots[i];
            if (slotInfo.kind >= 0) {
                Die("unexpected derived pointer\n");
            }

            Value_t *root = (Value_t *)(stackPtr + slotInfo.offset);
            Value_t p = *root;
            Value_t newP;

            if (isFromSpacePtr(p)) {
                newP = ForwardObjGlobal(vp, p);
                *root = newP;

#ifdef DEBUG_STACK_SCAN_GLOBAL
                fprintf(stderr, "[slot %u : %p] forward %p --> %p\n", i, root, p, newP);
#endif
            }
        } // end for

#ifdef DEBUG_STACK_SCAN_GLOBAL
        fprintf(stderr, "------------------------------------------\n");
#endif

        // move to next frame
        stackPtr += frame->frameSize;

    } // end while

    // the roots have been forwarded to another part of the global heap
    stkInfo->age = AGE_Global;

    stkInfo->deepestScan = origStkPtr; // mark that we've seen this stack

    ///////////////////////////////////////////////////
    ///////////// critical section end
    MutexUnlock(&stkInfo->gcLock);
  }

void ScanStackGlobal (
    void* origStkPtr,
    StackInfo_t* stkInfo,
    VProc_t* vp) {

  do {
        if (origStkPtr == NULL)
          origStkPtr = stkInfo->currentSP;

        ScanOneSegmentGlobal(origStkPtr, stkInfo, vp);

        stkInfo = stkInfo->prevSegment;
        origStkPtr = NULL; // after 1st segment, we use currentSP.

        #ifdef DEBUG_STACK_SCAN_GLOBAL
            // end of a stack segment
            fprintf(stderr, "=============================================\n");
        #endif

  } while (stkInfo != NULL);

#ifdef DEBUG_STACK_SCAN_GLOBAL
        fprintf(stderr, "##########################################\n");
#endif

    return;
}

/* \brief initialize the data structures that support global GC
 */
void InitGlobalGC ()
{
    MutexInit (&GCLock);
    CondInit (&LeaderWait);
    CondInit (&FollowerWait);
    GlobalGCInProgress = false;
    NumGlobalGCs = 0;

}

/* \brief Converts the tag for a tospace chunk to that of a fromspace
 * chunk and handles all associated logging and stats.
 */
void ConvertToSpaceChunks (VProc_t *self, MemChunk_t *p) {
    if (p == NULL)
        return;

    while (p != NULL) {
#ifndef NDEBUG
        if (p->sts != TO_SP_CHUNK) {
            SayDebug("Invalid chunk not marked TO_SP %p\n", (void*)p->baseAddr);
        }
#endif
        assert (p->sts == TO_SP_CHUNK);
#ifndef NDEBUG
        if (GCDebug >= GC_DEBUG_GLOBAL)
            SayDebug("[%2d]   From-Space chunk %p..%p\n",
                     self->id, (void *)(p->baseAddr),
                     (void *)(p->baseAddr+p->szB));
#endif
        p->sts = FROM_SP_CHUNK;
        p->scanProgress = 0;
#ifndef NO_GC_STATS
        uint32_t used = (p == self->globAllocChunk)
            ? (self->globNextW - WORD_SZB) - p->baseAddr
            : p->usedTop - p->baseAddr;
        self->globalStats.nBytesCollected += used;
#if (! defined(NDEBUG)) || defined(ENABLE_LOGGING)
        FetchAndAddU64 (&FromSpaceSzb, (uint64_t)used);
#endif
#endif /* !NO_GC_STATS */

        MemChunk_t *next = p->next;
        int node = LocationNode(self->location);
        p->next = NodeHeaps[node].fromSpace;
        NodeHeaps[node].fromSpace = p;
        p = next;
    }
}

/*! \brief attempt to start a global GC.
 *  \param vp the host vproc
 *  \param roots the array of root pointers for this vproc
 */
void StartGlobalGC (VProc_t *self, Value_t **roots)
{

    bool    leaderVProc;

#ifndef NO_GC_STATS
    TIMER_Start(&(self->globalStats.timer));
#endif

#ifdef ENABLE_PERF_COUNTERS
    PERF_StartGC(&self->misses);
    PERF_StartGC(&self->reads);
#endif

    self->globalGCPending = false;
    self->sigPending = M_FALSE;

    MutexLock (&GCLock);
    if (!GlobalGCInProgress) {
      /* this vproc is leading the global GC */
        leaderVProc = true;
        GlobalGCInProgress = true;
        AllReadyForGC = false;
        NReadyForGC = 1;
        NumGlobalGCs++;
#ifdef ENABLE_LOGGING
        GlobalGCUId = LogGlobalGCInit (self, NumGlobalGCs);
#endif
#ifndef NDEBUG
        if (GCDebug >= GC_DEBUG_GLOBAL)
            SayDebug("[%2d] Initiating global GC %d (%d processors)\n",
            self->id, NumGlobalGCs, NumVProcs);
#endif
#if (! defined(NDEBUG)) || defined(ENABLE_LOGGING)
        FromSpaceSzb = 0;
        NBytesCopied = 0;
#endif
      /* signal the other vprocs that GlobalGC is needed */
        for (int i = 0;  i < NumVProcs;  i++) {
        if (VProcs[i] != self)
            VProcGlobalGCInterrupt (self, VProcs[i]);
        }
    }
    else {
      // we are a follower
        leaderVProc = false;
    }

      // here the leader waits for the followers and the followers wait for the
      // leader to say "go"
    if (leaderVProc) {
      /* wait for the other vprocs to start global GC */
        while (NReadyForGC < NumVProcs)
        CondWait(&LeaderWait, &GCLock);
      /* reset the size of to-space */
        ToSpaceSz = 0;
      /* all followers are ready to do GC, so initialize the barriers
       * and then wake them up.
       */
        BarrierInit (&GCBarrier0, NumVProcs);
        BarrierInit (&GCBarrier1, NumVProcs);
        BarrierInit (&GCBarrier2, NumVProcs);
        AllReadyForGC = true;
        CondBroadcast(&FollowerWait);
    }
    else {
        if (++NReadyForGC == NumVProcs)
        CondSignal (&LeaderWait);
        while (! AllReadyForGC)
            CondWait (&FollowerWait, &GCLock);
    }
    MutexUnlock (&GCLock);

    /* Phase 1
     * Each vproc places their current alloc chunk
     * and any per-node unscannedTo space chunks into the
     * per-node fromSpace list */
    int        node = LocationNode(self->location);
    MutexLock(&NodeHeaps[node].lock);
    assert(NodeHeaps[node].scannedTo == NULL);
    NodeHeaps[node].completed = false;

    if (self->globAllocChunk != NULL) {
        self->globAllocChunk->usedTop = self->globNextW - WORD_SZB;
        assert (self->globAllocChunk->next == NULL);
    }

    ConvertToSpaceChunks (self, self->globAllocChunk);
    ConvertToSpaceChunks (self, NodeHeaps[node].unscannedTo);
    NodeHeaps[node].unscannedTo = NULL;
    MutexUnlock(&NodeHeaps[node].lock);

    /* finish the GC setup for this vproc */
    self->globAllocChunk = NULL;

    /* synchronize on every vproc finishing setup (so we know all
       from spaces are appropraitely tagged). */
    BarrierWait (&GCBarrier0);

  /* allocate the initial chunk for the vproc */
    AllocToSpaceChunk (self);

  /* start GC for this vproc */
    GlobalGC (self, roots);

#ifndef NDEBUG
    if (HeapCheck >= GC_DEBUG_GLOBAL) {
    if (GCDebug >= GC_DEBUG_GLOBAL)
        SayDebug ("[%2d] Checking heap consistency\n", self->id);
    CheckAfterGlobalGC (self, roots);
    }
#endif

  /* synchronize on every vproc finishing GC */
    BarrierWait (&GCBarrier1);

#ifndef NO_GC_STATS
    // compute the number of bytes copied in this GC on this vproc

    uint32_t used = (self->globNextW - WORD_SZB) -
        self->globAllocChunk->baseAddr;
    self->globalStats.nBytesCopied += used;
#if (! defined(NDEBUG)) || defined(ENABLE_LOGGING)
    // include in total for this GC
    FetchAndAddU64 (&NBytesCopied, (uint64_t)used);
#endif
    if (leaderVProc) {
        // The leader counts all of the bytes copied to chunks other than
        // those still being used as global allocation chunks for each
        // vproc
        for (int i = 0; i < NumHWNodes; i++) {
            for (MemChunk_t *p = NodeHeaps[i].scannedTo;  p != NULL;  p = p->next) {
                uint32_t used = p->usedTop - p->baseAddr;
                self->globalStats.nBytesCopied += used;
#if (! defined(NDEBUG)) || defined(ENABLE_LOGGING)
                // include in total for this GC
                FetchAndAddU64 (&NBytesCopied, (uint64_t)used);
#endif
            }
        }
    }
#endif /* !NO_GC_STATS */

    /* Phase 4
     * the leader reclaims the from-space pages
     * Technically, the locking is extraneous (since every other VProc
     * will be waiting on the barrier below), but I may parallelize this
     * in the future.
     */
    if (leaderVProc) {
        MutexLock (&HeapLock);
        for (int i = 0; i < NumHWNodes; i++) {
            MutexLock(&NodeHeaps[i].lock);
            assert(NodeHeaps[i].unscannedTo == NULL);
            NodeHeaps[i].unscannedTo = NodeHeaps[i].scannedTo;
            NodeHeaps[i].scannedTo = NULL;
            MemChunk_t *cp = NodeHeaps[i].fromSpace;
            while (cp != NULL) {
                cp->sts = FREE_CHUNK;
                cp->usedTop = cp->baseAddr;
#ifndef NDEBUG

                if (GCDebug >= GC_DEBUG_GLOBAL)
                    SayDebug("[%2d]   Free-Space chunk %#tx..%#tx\n",
                             self->id, cp->baseAddr, cp->baseAddr+cp->szB);
#endif
                MemChunk_t *cq = cp->next;
                assert (i == cp->where);
                cp->next = NodeHeaps[i].freeChunks;
                NodeHeaps[i].freeChunks = cp;
                cp = cq;
            }
            NodeHeaps[i].fromSpace = NULL;
            MutexUnlock(&NodeHeaps[i].lock);
        }

#ifndef NDEBUG
        if (HeapCheck >= GC_DEBUG_GLOBAL)
            CheckToSpacesAfterGlobalGC(self);
#endif

/* NOTE: at some point we may want to release memory back to the OS */
        GlobalGCInProgress = false;
        MutexUnlock (&HeapLock);
      // recalculate the ToSpaceLimit
    Addr_t baseLimit = (HeapScaleNum * ToSpaceSz) / HeapScaleDenom;
    baseLimit = (baseLimit < ToSpaceSz) ? ToSpaceSz : baseLimit;
    ToSpaceLimit = baseLimit + (Addr_t)NumVProcs * (Addr_t)PER_VPROC_HEAP_SZB;
#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_GLOBAL)
        SayDebug("[%2d] ToSpaceLimit = %ldMb\n",
        self->id, (unsigned long)(ToSpaceLimit >> 20));
#endif
    }

#ifdef DIRECT_STYLE
    /* another part of phase 4 is for each vproc to reclaim unmarked stacks,
       with the leader taking care of cleaning the global alloc'd list */

    if (leaderVProc) {
      // NOTE: the global alloc'd list has no owners, so FreeStacks will
      // release the memory instead of, say, distributing the free stacks to
      // vprocs or maintaining a global free list. This may not be ideal for
      // performance.
      GlobAllocdList = FreeStacks(self, GlobAllocdList, AGE_Global);
    }

    self->allocdStacks = FreeStacks(self, self->allocdStacks, AGE_Global);
#endif

  /* synchronize on from-space being reclaimed */
    BarrierWait (&GCBarrier2);

    LogGlobalGCEnd (self, NumGlobalGCs);

#ifndef NO_GC_STATS
    TIMER_Stop(&(self->globalStats.timer));
#endif
#ifdef ENABLE_PERF_COUNTERS
    PERF_StopGC(&self->misses);
    PERF_StopGC(&self->reads);
#endif

#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_GLOBAL) {
    if (leaderVProc)
        SayDebug("[%2d] Completed global GC; %"PRIu64"/%"PRIu64" bytes copied\n",
        self->id, NBytesCopied, FromSpaceSzb);
    else
        SayDebug("[%2d] Leaving global GC\n", self->id);
    }
#endif

} /* end of StartGlobalGC */

/* GlobalGC:
 */
static void GlobalGC (VProc_t *vp, Value_t **roots)
{
    LogGlobalGCVPStart (vp);

    MemChunk_t *original = vp->globAllocChunk;

  /* collect roots that were pruned away from the minor collector's root set */
    M_AddDequeEltsToGlobalRoots(vp, roots);


#ifdef DIRECT_STYLE
    /* scan the current stack. */
    StackInfo_t* stkInfo = (StackInfo_t*)(vp->stdCont);
    void* stkPtr = vp->stdEnvPtr;
    ScanStackGlobal(stkPtr, stkInfo, vp);
#endif

    /* Phase 2
     * scan the vproc's roots */
    for (int i = 0;  roots[i] != 0;  i++) {
      Value_t p = *roots[i];
      if (isFromSpacePtr(p)) {
          *roots[i] = ForwardObjGlobal(vp, p);
      }
    }

    // The landingPad is not in the root set until global GC, because
    // all of the elements in that list should be in the global heap already.
    // We can also only scan it safely during a global GC because this field of
    // a VProc is accessed in parallel by other threads.
    // These threads are are sending messages at the same time that a VProc may
    // be performing an asynchronous Minor or Major GC.
    // Since the Global GC is synchronized it's safe to scan and update like this.
    Value_t pad = vp->landingPad;
    if (isFromSpacePtr(pad))
        vp->landingPad = ForwardObjGlobal(vp, pad);

    ScanVProcHeap (vp);

    PushToSpaceChunks (vp, original, true);

  /* scan to-space chunks */
    ScanGlobalToSpace (vp);
    LogGlobalGCVPDone (vp, 0/*FIXME*/);

#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_GLOBAL)
    SayDebug("[%2d] Global GC finished on vproc\n", vp->id);
#endif

} /* end of GlobalGC */

/*! \brief Scan the vproc's local heap.  Since we have already done a major GC,
 *  all objects are known to be live.
 */
static void ScanVProcHeap (VProc_t *vp)
{
    Word_t *top = (Word_t *)(vp->oldTop);
    Word_t *scanPtr = (Word_t *)vp->heapBase;

    while (scanPtr < top) {
        Die ("early-tenuring optimization should have been disabled.");

        Word_t hdr = *scanPtr++;    // get object header

        int id = getID(hdr);
        if (unlikely(id >= tableMaxID))
            Die("GlobalGC, ScanVProcHeap: invalid header ID!");

        // All objects jump to their table entry function.
        // See header-tbl-print.sml, and use -saveTemps to view
        // the C code generated by that module.
        scanPtr = table[id].globalGCscanfunction(scanPtr,vp);
    }
    assert (scanPtr == top);

} /* end of ScanVProcHeap */

/* Returns a chunk of unscanned to-space. This function will wait
 * for more per-node work to become available. If all vprocs in a
 * node are waiting, then the per-node GC is complete and this
 * function returns NULL
 */
MemChunk_t *GetNextScanChunk(VProc_t *vp, int node) {
    MemChunk_t *chunk = NULL;

    MutexLock(&NodeHeaps[node].lock);
    while (true) {
#ifdef SINGLE_THREAD_PER_PACKAGE
        if (LogicalId(vp->location) == MinVProcPerNode[node]) {
#endif
        if (NodeHeaps[node].unscannedTo != NULL) {
            chunk = NodeHeaps[node].unscannedTo;
            NodeHeaps[node].unscannedTo = chunk->next;
            MutexUnlock(&NodeHeaps[node].lock);
            chunk->next = NULL;
#ifndef NDEBUG
            if (GCDebug >= GC_DEBUG_GLOBAL)
                SayDebug("[%2d]   GetNextScanChunk %p..%p\n",
                         vp->id, (void *)(chunk->baseAddr),
                         (void *)(chunk->baseAddr+chunk->szB));
#endif
            return chunk;
        }
#ifdef SINGLE_THREAD_PER_PACKAGE
        }
#endif

        if (vp->globAllocChunk->scanProgress < (vp->globNextW - WORD_SZB)) {
            MutexUnlock(&NodeHeaps[node].lock);
#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_GLOBAL)
    SayDebug("[%2d]   Returning allocation chunk for scan %p..%p at %p\n",
                 vp->id, (void *)(vp->globAllocChunk->baseAddr),
             (void*)vp->globAllocChunk->usedTop, (void*)vp->globAllocChunk->scanProgress);
#endif
            assert(vp->globAllocChunk->next == NULL);
            return vp->globAllocChunk;
        }

        if (++NodeHeaps[node].numWaiting == NumVProcsPerNode[node]) {
            NodeHeaps[node].completed = true;
            NodeHeaps[node].numWaiting--;
            MutexUnlock(&NodeHeaps[node].lock);
            CondBroadcast(&NodeHeaps[node].scanWait);
            return NULL;
        }

        if (!NodeHeaps[node].completed) {
            CondWait(&NodeHeaps[node].scanWait, &NodeHeaps[node].lock);
        }

        NodeHeaps[node].numWaiting--;
        if (NodeHeaps[node].completed) {
            MutexUnlock(&NodeHeaps[node].lock);
            return NULL;
        }
    }
}

/*! \brief Scan the to-space objects that have been copied by this vproc
 *  \param vp the vproc doing the scanning
 * Phase 3
 */
static void ScanGlobalToSpace (VProc_t *vp)
{
    int node = LocationNode(vp->location);
    MemChunk_t    *scanChunk = NULL;

    while ((scanChunk = GetNextScanChunk(vp, node)) != NULL) {
        Word_t    *scanPtr = (Word_t *)(scanChunk->baseAddr>scanChunk->scanProgress?
                                      scanChunk->baseAddr:scanChunk->scanProgress);
        Word_t    *scanTop = UsedTopOfChunk(vp, scanChunk);
        MemChunk_t *origAlloc = vp->globAllocChunk;
        bool handlingAlloc = scanChunk == vp->globAllocChunk;

        do {
            while (scanPtr < scanTop) {
                Word_t hdr = *scanPtr++;    // get object header

                int id = getID(hdr);
                if (unlikely(id >= tableMaxID))
                    Die("GlobalGC, ScanGlobalToSpace: invalid header ID!");

                // All objects jump to their table entry function.
                // See header-tbl-print.sml, and use -saveTemps to view
                // the C code generated by that module.
                scanPtr = table[id].globalGCscanfunction(scanPtr,vp);
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

        if (handlingAlloc) {
            if (origAlloc != vp->globAllocChunk) {
                MemChunk_t *tmp = origAlloc->next;

                MutexLock(&NodeHeaps[node].lock);
                origAlloc->next = NodeHeaps[node].scannedTo;
                NodeHeaps[node].scannedTo = origAlloc;
                MutexUnlock(&NodeHeaps[node].lock);

                PushToSpaceChunks (vp, tmp, true);
            }
        } else {
            assert(scanChunk->next == NULL);

            MutexLock(&NodeHeaps[node].lock);
            scanChunk->next = NodeHeaps[node].scannedTo;
            NodeHeaps[node].scannedTo = scanChunk;
            MutexUnlock(&NodeHeaps[node].lock);

            if (origAlloc != vp->globAllocChunk) {
                PushToSpaceChunks (vp, origAlloc, true);
            }
        }

    }
}

#ifndef NDEBUG
void CheckGlobalAddr (VProc_t *self, void *addr, char *where);

/* Check that the given address points *to* an object in the global heap.
 */
void CheckGlobalPtr (VProc_t *self, void *addr, char *where)
{
  /*
    if (isHeapPtr(PtrToValue(addr))) {
    Word_t *ptr = (Word_t*)addr;
    Word_t hdr = ptr[-1];
    if (isMixedHdr(hdr) || isVectorHdr(hdr) || isRawHdr(hdr)) {
      // the header word is valid
    }
    else if (isRawHdr(hdr)) {
        SayDebug("[%2d] CheckGlobalPtr: unexpected raw header for %p in %s \n",
        self->id, addr, where);
    }
    else {
        MemChunk_t *cq = AddrToChunk((Addr_t)addr);
        SayDebug("[%2d] CheckGlobalPtr: unexpected bogus header %p for %p[%d] in %s\n",
        self->id, (void *)hdr, addr, cq->sts, where);
    }
    }
  */
    CheckGlobalAddr (self, addr, where);
}

/* Check that the given address points *into* an object in the global heap. That is,
 * addr might point into the middle of a heap object.
 */
void CheckGlobalAddr (VProc_t *self, void *addr, char *where)
{
    assert(VProcSelf() == self);
    Value_t v = (Value_t)addr;
    if (isHeapPtr(v)) {
    MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
    if (cq->sts == TO_SP_CHUNK)
        return;
    else if (cq->sts == FROM_SP_CHUNK) {
      if (!GlobalGCInProgress) {
        /* it is safe to point to from-space pages just before performing a global gc */
          SayDebug("[%2d] CheckGlobalAddr: unexpected from-space pointer %p in %s\n",
               self->id, ValueToPtr(v), where);
      }
    }
    else if (isLimitPtr(v, cq))
        return;
    else if (IS_VPROC_CHUNK(cq->sts)) {
        if (inAddrRange(ValueToAddr(v) & ~VP_HEAP_MASK, sizeof(VProc_t), ValueToAddr(v))) {
          /* IMPORTANT: we make an exception for objects stored in the vproc structure */
            return;
        }
        else if (cq->sts != VPROC_CHUNK(self->id)) {
               SayDebug("[%2d] CheckGlobalAddr: bogus remote pointer %p in %s\n",
                        self->id, ValueToPtr(v), where);
        }
        else if (cq->sts == VPROC_CHUNK(self->id)) {
           SayDebug("[%2d] CheckGlobalAddr: bogus local pointer %p in %s\n",
                self->id, ValueToPtr(v), where);
        }
        else if (! inAddrRange(self->heapBase, self->oldTop - self->heapBase, ValueToAddr(v))) {
        SayDebug("[%2d] CheckGlobalAddr: bogus local pointer %p is out of bounds in %s\n",
             self->id, ValueToPtr(v), where);
        }
    }
    else if (cq->sts == FREE_CHUNK) {
        SayDebug("[%2d] CheckGlobalAddr: unexpected free-space pointer %p at %p from %s\n",
             self->id, ValueToPtr(v), addr, where);
    }
    }
}


/* Check the invariant that the value pointed to by addr is a pointer living in either
 * the root set or the local heap.
 * Precondition: this check should only occur just after a global collection.
 */
void CheckLocalPtrGlobal (VProc_t *self, void *addr, const char *where)
{
    Value_t v = *(Value_t *)addr;
    if (isPtr(v)) {
    MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
    if (cq->sts == TO_SP_CHUNK)
        return;
    else if (cq->sts == FROM_SP_CHUNK)
        SayDebug("[%2d] ** unexpected from-space pointer %p at %p in %s\n",
        self->id, ValueToPtr(v), addr, where);
    else if (IS_VPROC_CHUNK(cq->sts)) {
        if (cq->sts != VPROC_CHUNK(self->id)) {
        SayDebug("[%2d] ** unexpected remote pointer %p at %p in %s\n",
            self->id, ValueToPtr(v), addr, where);
        }
        else if (! inAddrRange(self->heapBase, self->oldTop - self->heapBase, ValueToAddr(v))) {
        SayDebug("[%2d] ** local pointer %p at %p in %s is out of bounds\n",
            self->id, ValueToPtr(v), addr, where);
        }
    }
    else if (cq->sts == FREE_CHUNK) {
        SayDebug("[%2d] ** unexpected free-space pointer %p at %p in %s\n",
        self->id, ValueToPtr(v), addr, where);
    }
    }
}

void CheckAfterGlobalGC (VProc_t *self, Value_t **roots)
{
  // check the roots
    for (int i = 0;  roots[i] != 0;  i++) {
    char buf[18];
    sprintf(buf, "root[%d]", i);
    // Value_t v = *roots[i];
    CheckLocalPtrGlobal (self, roots[i], buf);
    }

  // check the local heap
    {
    Word_t *top = (Word_t *)(self->oldTop);
    Word_t *p = (Word_t *)self->heapBase;
    while (p < top) {
        Word_t hdr = *p++;
        Word_t *scanptr = p;
        tableDebug[getID(hdr)].globalGCdebug(self,scanptr);

        p += GetLength(hdr);
    }
    }

    // check the vproc's global allocation area
    if (self->globAllocChunk != NULL) {
        Word_t *p = (Word_t *)(self->globAllocChunk->baseAddr);
        Word_t *top = UsedTopOfChunk(self, self->globAllocChunk);

        assert(self->globAllocChunk->sts == TO_SP_CHUNK);

        while (p < top) {
            Word_t hdr = *p++;
            Word_t *scanptr = p;
            tableDebug[getID(hdr)].globalGCdebugGlobal(self,scanptr);

            p += GetLength(hdr);
        }
    }

  // check the VProc structure
#define CHECK_VP(fld)    CheckLocalPtrGlobal(self, &(self->fld), "self->" #fld)
    CHECK_VP(atomic);
    CHECK_VP(sigPending);
    CHECK_VP(sleeping);
    CHECK_VP(currentFLS);
    CHECK_VP(actionStk);
    CHECK_VP(schedCont);
    CHECK_VP(dummyK);
    CHECK_VP(wakeupCont);
    CHECK_VP(rdyQHd);
    CHECK_VP(rdyQTl);
    CHECK_VP(sndQHd);
    CHECK_VP(sndQTl);
    CHECK_VP(stdArg);
    CHECK_VP(stdEnvPtr);
    CHECK_VP(stdCont);
    CHECK_VP(stdExnCont);
    CHECK_VP(landingPad);

}

void CheckToSpacesAfterGlobalGC (VProc_t *self)
{
    for (int i = 0; i < NumHWNodes; i++) {
        assert (NodeHeaps[i].scannedTo == NULL);
        assert (NodeHeaps[i].fromSpace == NULL);
        MemChunk_t    *cp = NodeHeaps[i].unscannedTo;

        while (cp != (MemChunk_t *)0) {
            assert (cp->sts = TO_SP_CHUNK);
            Word_t *p = (Word_t *)(cp->baseAddr);
            Word_t *top = UsedTopOfChunk(self, cp);
            while (p < top) {
                Word_t hdr = *p++;
                Word_t *scanptr = p;
                //assert (!isForwardPtr(hdr));
                if (isForwardPtr(hdr)) {
                    p++;
                    continue;
                }

                tableDebug[getID(hdr)].globalGCdebugGlobal(self,scanptr);

                p += GetLength(hdr);
            }
            cp = cp->next;
        }

        cp = NodeHeaps[i].freeChunks;
        while (cp != NULL) {
            assert (cp->sts == FREE_CHUNK);
            cp = cp->next;
        }
    }

    for (int i = 0; i < NumVProcs; i++) {
        int node = LocationNode (VProcs[i]->location);

        MemChunk_t *cp = NodeHeaps[node].unscannedTo;

        while (cp != NULL) {
            assert (cp != VProcs[i]->globAllocChunk);
            cp = cp->next;
        }
    }
}

#endif
