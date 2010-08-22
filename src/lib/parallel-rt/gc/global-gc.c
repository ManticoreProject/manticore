/* global-gc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include <strings.h>
#include <stdio.h>

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

static Mutex_t		GCLock;		// Lock that protects the following variables:
static Cond_t		LeaderWait;	// The leader waits on this for the followers
static Cond_t		FollowerWait;	// followers block on this until the leader starts the GC
static volatile int	NReadyForGC;	// number of vprocs that are ready for GC
static volatile bool	GlobalGCInProgress; // true, when a global GC has been initiated
static volatile bool	AllReadyForGC;	// true when all vprocs are ready to start GC
static Barrier_t        GCBarrier1;	// for synchronizing on completion of copying phase
static Barrier_t	GCBarrier2;	// for synchronizing on completion of GC

#if (! defined(NDEBUG)) || defined(ENABLE_LOGGING)
/* summary statistics for global GC */
static uint64_t		FromSpaceSzb __attribute__((aligned(64)));
static uint64_t		NBytesCopied __attribute__((aligned(64)));
#endif

#ifdef ENABLE_LOGGING
uint64_t		GlobalGCUId;	// Unique ID of current global GC
#endif

static void GlobalGC (VProc_t *vp, Value_t **roots);
static void ScanVProcHeap (VProc_t *vp);
static void ScanGlobalToSpace (VProc_t *vp);
#ifndef NDEBUG
void CheckAfterGlobalGC (VProc_t *self, Value_t **roots);
#endif


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

/*! \brief attempt to start a global GC.
 *  \param vp the host vproc
 *  \param roots the array of root pointers for this vproc
 */
void StartGlobalGC (VProc_t *self, Value_t **roots)
{
    bool	leaderVProc;

#ifndef NO_GC_STATS
    TIMER_Start(&(self->globalStats.timer));
#endif

#ifdef ENABLE_PERF_COUNTERS
    PERF_StartGC(&self->misses);
    PERF_StartGC(&self->reads);
#endif

    self->globalGCPending = false;
    self->sigPending = false;

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

      /* add the vproc's pages to the from-space list */
	MemChunk_t *p;
	for (p = self->globToSpHd;  p != (MemChunk_t *)0;  p = p->next) {
	    assert (p->sts == TO_SP_CHUNK);
#ifndef NDEBUG
	    if (GCDebug >= GC_DEBUG_GLOBAL)
		SayDebug("[%2d]   From-Space chunk %p..%p\n",
		    self->id, (void *)(p->baseAddr),
		    (void *)(p->baseAddr+p->szB));
#endif
	    p->sts = FROM_SP_CHUNK;
#ifndef NO_GC_STATS
	    uint32_t used = (p == self->globToSpTl)
		? (self->globNextW - WORD_SZB) - p->baseAddr
		: p->usedTop - p->baseAddr;
	    self->globalStats.nBytesCollected += used;
#if (! defined(NDEBUG)) || defined(ENABLE_LOGGING)
	    FetchAndAddU64 (&FromSpaceSzb, (uint64_t)used);
#endif
#endif /* !NO_GC_STATS */
	}
	p = self->globToSpTl;
	if (p != (MemChunk_t *)0) {
	    p->usedTop = self->globNextW - WORD_SZB;
	    p->next = FromSpaceChunks;
	    FromSpaceChunks = self->globToSpHd;
	}

      /* finish the GC setup for this vproc */
	self->globToSpTl = (MemChunk_t *)0;
	self->globToSpHd = (MemChunk_t *)0;

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

#ifndef NO_GC_STATS
  // compute the number of bytes copied in this GC on this vproc
    for (p = self->globToSpHd;  p != (MemChunk_t *)0;  p = p->next) {
	uint32_t used = (p == self->globToSpTl)
	    ? (self->globNextW - WORD_SZB) - p->baseAddr
	    : p->usedTop - p->baseAddr;
	self->globalStats.nBytesCopied += used;
#if (! defined(NDEBUG)) || defined(ENABLE_LOGGING)
      // include in total for this GC
	FetchAndAddU64 (&NBytesCopied, (uint64_t)used);
#endif
    }
#endif /* !NO_GC_STATS */

  /* synchronize on every vproc finishing GC */
    BarrierWait (&GCBarrier1);

  /* the leader reclaims the from-space pages */
    if (leaderVProc) {
	MutexLock (&HeapLock);
	    MemChunk_t *cp = FromSpaceChunks;
	    FromSpaceChunks = (MemChunk_t *)0;
	    while (cp != (MemChunk_t *)0) {
		cp->sts = FREE_CHUNK;
		cp->usedTop = cp->baseAddr;
#ifndef NDEBUG
		if (GCDebug >= GC_DEBUG_GLOBAL)
		    SayDebug("[%2d]   Free-Space chunk %#tx..%#tx\n",
			self->id, cp->baseAddr, cp->baseAddr+cp->szB);
#endif
		MemChunk_t *cq = cp->next;
		int nd = cp->where;
		cp->next = FreeChunks[nd];
		FreeChunks[nd] = cp;
		cp = cq;
	    }
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
	    SayDebug("[%2d] Completed global GC; %lld/%lld bytes copied\n",
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

  /* collect roots that were pruned away from the minor collector's root set */
    M_AddDequeEltsToGlobalRoots(vp, roots);

  /* scan the vproc's roots */
    for (int i = 0;  roots[i] != 0;  i++) {
	Value_t p = *roots[i];
	if (isFromSpacePtr(p)) {
	    *roots[i] = ForwardObjGlobal(vp, p);
	}
    }

    ScanVProcHeap (vp);

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
		
		Word_t hdr = *scanPtr++;	// get object header
		
		if (isVectorHdr(hdr)) {
			//Word_t *nextScan = ptr;
			int len = GetLength(hdr);
			for (int i = 0;  i < len;  i++, scanPtr++) {
				Value_t *scanP = (Value_t *)scanPtr;
				Value_t v = *scanP;
				if (isFromSpacePtr(v)) {
					*scanP = ForwardObjGlobal(vp, v);
				}
			}
			
			
		}else if (isRawHdr(hdr)) {
			assert (isRawHdr(hdr));
			scanPtr += GetLength(hdr);
		}else {
			
			scanPtr = table[getID(hdr)].globalGCscanfunction(scanPtr,vp);

		}
		

    }
    assert (scanPtr == top);

} /* end of ScanVProcHeap */

/*! \brief Scan the to-space objects that have been copied by this vproc
 *  \param vp the vproc doing the scanning
 */
static void ScanGlobalToSpace (VProc_t *vp)
{
    MemChunk_t	*scanChunk = vp->globToSpHd;
    Word_t	*scanPtr = (Word_t *)(scanChunk->baseAddr);
    Word_t	*scanTop = UsedTopOfChunk(vp, scanChunk);
	
    do {
	while (scanPtr < scanTop) {
		
		Word_t hdr = *scanPtr++;	// get object header
		
		if (isVectorHdr(hdr)) {
			//Word_t *nextScan = ptr;
			int len = GetLength(hdr);
			for (int i = 0;  i < len;  i++, scanPtr++) {
				Value_t *scanP = (Value_t *)scanPtr;
				Value_t v = *scanP;
				if (isFromSpacePtr(v)) {
					*scanP = ForwardObjGlobal(vp, v);
				}
			}
			
			
		}else if (isRawHdr(hdr)) {
			assert (isRawHdr(hdr));
			scanPtr += GetLength(hdr);
		}else {
			
			scanPtr = table[getID(hdr)].globalGCscanfunction(scanPtr,vp);
		
		}
			
	}

      /* recompute the scan top, switching chunks if necessary */
	if (vp->globToSpTl == scanChunk)
	    scanTop = (Word_t *)(vp->globNextW - WORD_SZB);
	else if (scanPtr == (Word_t *)scanChunk->usedTop) {
	    scanChunk = scanChunk->next;
	    assert (scanChunk != (MemChunk_t *)0);
	    scanPtr = (Word_t *)(scanChunk->baseAddr);
	    scanTop = UsedTopOfChunk(vp, scanChunk);
	}
	else
	    scanTop = (Word_t *)(scanChunk->usedTop);

    } while (scanPtr < scanTop);

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
	char buf[16];
	sprintf(buf, "root[%d]", i);
	Value_t v = *roots[i];
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

  // check to space
    MemChunk_t	*cp = self->globToSpHd;
    while (cp != (MemChunk_t *)0) {
	assert (cp->sts = TO_SP_CHUNK);
	Word_t *p = (Word_t *)(cp->baseAddr);
	Word_t *top = UsedTopOfChunk(self, cp);
	while (p < top) {
	    Word_t hdr = *p++;
	    Word_t *scanptr = p;
		tableDebug[getID(hdr)].globalGCdebugGlobal(self,scanptr);
		
		p += GetLength(hdr);
	}
	cp = cp->next;
    }

  // check the VProc structure
#define CHECK_VP(fld)	CheckLocalPtrGlobal(self, &(self->fld), "self->" #fld)
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
    CHECK_VP(stdArg);
    CHECK_VP(stdEnvPtr);
    CHECK_VP(stdCont);
    CHECK_VP(stdExnCont);
    CHECK_VP(landingPad);

}
#endif

