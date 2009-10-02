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

static Mutex_t		GCLock;		// Lock that protects the following variables:
static Cond_t		LeaderWait;	// The leader waits on this for the followers
static Cond_t		FollowerWait;	// followers block on this until the leader starts the GC
static volatile int	NReadyForGC;	// number of vprocs that are ready for GC
static volatile bool	GlobalGCInProgress; // true, when a global GC has been initiated
static volatile bool	AllReadyForGC;	// true when all vprocs are ready to start GC
uint32_t		NumGlobalGCs;	// the total number of global GCs.
static Barrier_t        GCBarrier1;	// for synchronizing on completion of copying phase
static Barrier_t	GCBarrier2;	// for synchronizing on completion of GC

#ifndef NO_GC_STATS
static uint64_t		FromSpaceSzb;
static uint64_t		NWordsScanned;
static uint64_t		NBytesCopied;
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

/*! \brief Return true if a value is a from-space pointer.
 *
 * Note that this function relies on the fact that unmapped addresses are
 * mapped to the "UnmappedChunk" by the BIBOP.
 */
STATIC_INLINE bool isFromSpacePtr (Value_t p)
{
    return (isPtr(p) && (AddrToChunk(ValueToAddr(p))->sts == FROM_SP_CHUNK));

}

/* Forward an object into the global-heap chunk reserved for the current VP */
STATIC_INLINE Value_t ForwardObj (VProc_t *vp, Value_t v)
{
    Word_t	*p = ((Word_t *)ValueToPtr(v));
    Word_t	oldHdr = p[-1];
    if (isForwardPtr(oldHdr)) {
	Value_t v = PtrToValue(GetForwardPtr(oldHdr));
	assert (isPtr(v) && (AddrToChunk(ValueToAddr(v))->sts == TO_SP_CHUNK));
	return v;
    }
    else {
      // we need to atomically update the header to a forward pointer, so frst
      // we allocate space for the object and then we try to install the forward
      // pointer.
	Word_t *nextW = (Word_t *)vp->globNextW;
	int len = GetLength(oldHdr);
	if (nextW+len >= (Word_t *)(vp->globLimit)) {
	    AllocToSpaceChunk (vp);
	    nextW = (Word_t *)vp->globNextW;
	}
     // try to install the forward pointer
	Word_t fwdPtr = MakeForwardPtr(oldHdr, nextW);
	Word_t hdr = CompareAndSwapWord(p-1, oldHdr, fwdPtr);
	if (oldHdr == hdr) {
	    Word_t *newObj = nextW;
	    newObj[-1] = hdr;
	    for (int i = 0;  i < len;  i++) {
		newObj[i] = p[i];
	    }
	    vp->globNextW = (Addr_t)(newObj+len+1);
#ifndef NO_GC_STATS
/* FIXME: we should really compute this information on a per-chunk basis */
	    vp->nBytesCopied += WORD_SZB*(len+1);
#endif
	    return PtrToValue(newObj);
	}
	else {
	  // some other vproc forwarded the object, so return the forwarded
	  // object.
	    assert (isForwardPtr(hdr));
	    return PtrToValue(GetForwardPtr(hdr));
	}
    }

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

/*! \brief attempt to start a global GC.
 *  \param vp the host vproc
 *  \param roots the array of root pointers for this vproc
 */
void StartGlobalGC (VProc_t *self, Value_t **roots)
{
    bool	leaderVProc;

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
	        SayDebug("[%2d] Initiating global GC %d (%d processors)\n", self->id, NumGlobalGCs, NumVProcs);
#endif
#ifndef NO_GC_STATS
	    FromSpaceSzb = 0;
	    NWordsScanned = 0;
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
		SayDebug("[%2d]   From-Space chunk %#tx..%#tx\n",
		    self->id, p->baseAddr, p->baseAddr+p->szB);
#endif
	    p->sts = FROM_SP_CHUNK;
#ifndef NO_GC_STATS
	    if (p == self->globToSpTl)
		FromSpaceSzb += (self->globNextW - WORD_SZB) - p->baseAddr;
	    else
		FromSpaceSzb += p->usedTop - p->baseAddr;
#endif
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
#ifndef NO_GC_STATS
	self->nWordsScanned = 0;
	self->nBytesCopied = 0;
#endif

      // here the leader waits for the followers and the followers wait for the
      // leader to say "go"
	if (leaderVProc) {
	  /* reset the size of to-space */
	    ToSpaceSz = 0;
	    while (NReadyForGC < NumVProcs)
		CondWait(&LeaderWait, &GCLock);
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

#ifndef NO_GC_STATS
  /* add this vproc's counts to the global counters */
    FetchAndAdd64 ((int64_t *)&NWordsScanned, self->nWordsScanned);
    FetchAndAdd64 ((int64_t *)&NBytesCopied, self->nBytesCopied);
#endif

#ifndef NDEBUG
    if (HeapCheck >= GC_DEBUG_GLOBAL) {
	if (GCDebug >= GC_DEBUG_GLOBAL)
	    SayDebug ("[%2d] Checking heap consistency\n", self->id);
	CheckAfterGlobalGC (self, roots);
    }
#endif

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
		/*DEBUG bzero((void*)cp->baseAddr, cp->szB);*/
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
    }

  /* synchronize on from-space being reclaimed */
    BarrierWait (&GCBarrier2);

    LogGlobalGCEnd (self, NumGlobalGCs);

#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_GLOBAL) {
	if (leaderVProc)
	    SayDebug("[%2d] Completed global GC; %lld words scanned; %lld/%lld bytes copied\n",
		self->id, NWordsScanned, NBytesCopied, FromSpaceSzb);
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
	    *roots[i] = ForwardObj(vp, p);
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
    Word_t *scanPtr = (Word_t *)VProcHeap(vp);

#ifndef NO_GC_STATS
    vp->nWordsScanned += (top - scanPtr);
#endif

    while (scanPtr < top) {
	Word_t hdr = *scanPtr++;  // get object header
	if (isMixedHdr(hdr)) {
	  // a record
	    Word_t tagBits = GetMixedBits(hdr);
	    Word_t *scanP = scanPtr;
	    while (tagBits != 0) {
		if (tagBits & 0x1) {
		    Value_t p = *(Value_t *)scanP;
		    if (isFromSpacePtr(p)) {
			*scanP = (Word_t)ForwardObj(vp, p);
		    }
		}
		tagBits >>= 1;
		scanP++;
	    }
	    scanPtr += GetMixedSizeW(hdr);
	}
	else if (isVectorHdr(hdr)) {
	  // an array of pointers
	    int len = GetVectorLen(hdr);
	    for (int i = 0;  i < len;  i++, scanPtr++) {
		Value_t v = (Value_t)*scanPtr;
		if (isFromSpacePtr(v)) {
		    *scanPtr = (Word_t)ForwardObj(vp, v);
		}
	    }
	}
	else {
	    assert (isRawHdr(hdr));
	  // we can just skip raw objects
	    scanPtr += GetRawSizeW(hdr);
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
#ifndef NO_GC_STATS
	vp->nWordsScanned += ((Word_t)scanTop - (Word_t)scanPtr);
#endif
	while (scanPtr < scanTop) {
	    Word_t hdr = *scanPtr++;  // get object header
	    if (isMixedHdr(hdr)) {
	      // a record
		Word_t tagBits = GetMixedBits(hdr);
		Word_t *scanP = scanPtr;
		while (tagBits != 0) {
		    if (tagBits & 0x1) {
			Value_t p = *(Value_t *)scanP;
			if (isFromSpacePtr(p)) {
			    *scanP = (Word_t)ForwardObj(vp, p);
			}
		    }
		    tagBits >>= 1;
		    scanP++;
		}
		scanPtr += GetMixedSizeW(hdr);
	    }
	    else if (isVectorHdr(hdr)) {
	      // an array of pointers
		int len = GetVectorLen(hdr);
		for (int i = 0;  i < len;  i++, scanPtr++) {
		    Value_t v = (Value_t)*scanPtr;
		    if (isFromSpacePtr(v)) {
		        *scanPtr = (Word_t)ForwardObj(vp, v);
		    }
		}
	    }
	    else {
		assert (isRawHdr(hdr));
	      // we can just skip raw objects
		scanPtr += GetRawSizeW(hdr);
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
	if (isMixedHdr(hdr) || isVectorHdr(hdr)) {
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
	    else if (! inAddrRange(VProcHeap(self), self->oldTop - VProcHeap(self), ValueToAddr(v))) {
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
static void CheckLocalPtr (VProc_t *self, void *addr, const char *where)
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
	    else if (! inAddrRange(VProcHeap(self), self->oldTop - VProcHeap(self), ValueToAddr(v))) {
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
	CheckLocalPtr (self, roots[i], buf);
    }

  // check the local heap
    {
	Word_t *top = (Word_t *)(self->oldTop);
	Word_t *p = (Word_t *)VProcHeap(self);
	while (p < top) {
	    Word_t hdr = *p++;
	    if (isMixedHdr(hdr)) {
	      // a record
		Word_t tagBits = GetMixedBits(hdr);
		Word_t *scanP = p;
		while (tagBits != 0) {
		    if (tagBits & 0x1) {
			CheckLocalPtr (self, scanP, "local mixed object");
		    }
		    else {
		      /* check for possible pointers in non-pointer fields */
			Value_t v = *(Value_t *)scanP;
			if (isHeapPtr(v)) {
			    MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
			    switch (cq->sts) {
			      case FREE_CHUNK:
				SayDebug("[%2d] ** possible free-space pointer %p in mixed object %p+%d\n",
				    self->id, (void *)v, (void *)p, (int)(scanP-p));
				break;
			      case TO_SP_CHUNK:
				SayDebug("[%2d] ** possible to-space pointer %p in mixed object %p+%d\n",
				    self->id, (void *)v, (void *)p, (int)(scanP-p));
				break;
			      case FROM_SP_CHUNK:
				SayDebug("[%2d] ** possible from-space pointer %p in mixed object %p+%d\n",
				    self->id, (void *)v, (void *)p, (int)(scanP-p));
				break;
			      case UNMAPPED_CHUNK:
				break;
			      default:
				if (IS_VPROC_CHUNK(cq->sts)) {
				  /* the vproc pointer is pretty common, so filter it out */
				    if (ValueToAddr(v) & ~VP_HEAP_MASK != ValueToAddr(v))
					SayDebug("[%2d] ** possible local pointer %p in mixed object %p+%d\n",
					    self->id, (void *)v, (void *)p, (int)(scanP-p));
				}
				else {
				    SayDebug("[%2d] ** strange pointer %p in mixed object %p+%d\n",
					self->id, (void *)v, (void *)p, (int)(scanP-p));
				}
				break;
			    }
			}
		    }
		    tagBits >>= 1;
		    scanP++;
		}
		p += GetMixedSizeW(hdr);
	    }
	    else if (isVectorHdr(hdr)) {
	      // an array of pointers
		int len = GetVectorLen(hdr);
		for (int i = 0;  i < len;  i++, p++) {
		    CheckLocalPtr (self, p, "local vector");
		}
	    }
	    else {
		assert (isRawHdr(hdr));
		int len = GetRawSizeW(hdr);
	      // look for raw values that might be pointers
		for (int i = 0; i < len; i++) {
		    Value_t v = (Value_t)p[i];
		    if (isPtr(v)) {
		        if (isHeapPtr(v)) {
			    MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
			    if (cq->sts != TO_SP_CHUNK) {
				if (cq->sts == FROM_SP_CHUNK)
				    SayDebug("[%2d] ** suspicious looking from-space pointer %p at %p[%d] in raw object of length %d (in local heap)\n",
					     self->id, ValueToPtr(v), (void *)p, i, len);
				else if (IS_VPROC_CHUNK(cq->sts))
				  /* the vproc pointer is pretty common, so filter it out */
				    if (ValueToAddr(v) & ~VP_HEAP_MASK != ValueToAddr(v))
				        SayDebug("[%2d] ** suspicious looking local pointer %p at %p[%d] in raw object of length %d (in local heap)\n",
						 self->id, ValueToPtr(v), (void *)p, i, len);
				else if (cq->sts == FREE_CHUNK)
				    SayDebug("[%2d] ** suspicious looking free pointer %p at %p[%d] in raw object of length %d (in local heap)\n",
					     self->id, ValueToPtr(v), (void *)p, i, len);
			    }
			} 
		    }
		}
		p += len;
	    }
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
	    if (isMixedHdr(hdr)) {
	      // a record
		Word_t tagBits = GetMixedBits(hdr);
		Word_t *scanP = p;
		while (tagBits != 0) {
		    if (tagBits & 0x1) {
			Value_t v = *(Value_t *)scanP;
			if (isPtr(v)) {
			    MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
			    if (cq->sts != TO_SP_CHUNK) {
				if (cq->sts == FROM_SP_CHUNK)
				    SayDebug("[%2d] ** unexpected from-space pointer %p at %p in mixed object\n",
					self->id, ValueToPtr(v), (void *)p);
				else if (IS_VPROC_CHUNK(cq->sts))
				    SayDebug("[%2d] ** unexpected local pointer %p at %p in mixed object\n",
					self->id, ValueToPtr(v), (void *)p);
				else if (cq->sts == FREE_CHUNK)
				    SayDebug("[%2d] ** unexpected free pointer %p at %p in mixed object\n",
					self->id, ValueToPtr(v), (void *)p);
			    }
			}
		    }
		    else {
		      /* check for possible pointers in non-pointer fields */
			Value_t v = *(Value_t *)scanP;
			if (isHeapPtr(v)) {
			    MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
			    switch (cq->sts) {
			      case FREE_CHUNK:
				SayDebug("[%2d] ** possible free-space pointer %p in mixed object %p+%d\n",
				    self->id, (void *)v, (void *)p, (int)(scanP-p));
				break;
			      case TO_SP_CHUNK:
				SayDebug("[%2d] ** possible to-space pointer %p in mixed object %p+%d\n",
				    self->id, (void *)v, (void *)p, (int)(scanP-p));
				break;
			      case FROM_SP_CHUNK:
				SayDebug("[%2d] ** possible from-space pointer %p in mixed object %p+%d\n",
				    self->id, (void *)v, (void *)p, (int)(scanP-p));
				break;
			      case UNMAPPED_CHUNK:
				break;
			      default:
				if (IS_VPROC_CHUNK(cq->sts)) {
				  /* the vproc pointer is pretty common, so filter it out */
				    if (ValueToAddr(v) & ~VP_HEAP_MASK != ValueToAddr(v))
					SayDebug("[%2d] ** possible local pointer %p in mixed object %p+%d\n",
					    self->id, (void *)v, (void *)p, (int)(scanP-p));
				}
				else {
				    SayDebug("[%2d] ** strange pointer %p in mixed object %p+%d\n",
					self->id, (void *)v, (void *)p, (int)(scanP-p));
				}
				break;
			    }
			}
		    }
		    tagBits >>= 1;
		    scanP++;
		}
		p += GetMixedSizeW(hdr);
	    }
	    else if (isVectorHdr(hdr)) {
	      // an array of pointers
		int len = GetVectorLen(hdr);
		for (int i = 0;  i < len;  i++, p++) {
		    Value_t v = (Value_t)*p;
		    if (isPtr(v)) {
			MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
			if (cq->sts != TO_SP_CHUNK) {
			    if (cq->sts == FROM_SP_CHUNK)
				SayDebug("[%2d] ** unexpected from-space pointer %p at %p in vector\n",
				    self->id, ValueToPtr(v), (void *)p);
			    else if (IS_VPROC_CHUNK(cq->sts)) {
			      if (cq->sts != VPROC_CHUNK(self->id)) {
				SayDebug("[%2d] ** unexpected remote pointer %p at %p in vector\n",
					 self->id, ValueToPtr(v), (void *)p);
			      }
			      else if (ValueToAddr(v) & ~VP_HEAP_MASK != ValueToAddr(v)) {
				SayDebug("[%2d] ** unexpected vproc-structure pointer %p at %p in vector\n",
					 self->id, ValueToPtr(v), (void *)p);
			      }
			      else if (! inAddrRange(VProcHeap(self), self->oldTop - VProcHeap(self), ValueToAddr(v))) {
				SayDebug("[%2d] ** unexpected local pointer %p at %p in vector[%d] is out of bounds\n",
					 self->id, ValueToPtr(v), (void *)p, i);
			      } else {
				SayDebug("[%2d] ** unexpected local pointer %p at %p in vector\n",
					 self->id, ValueToPtr(v), (void *)p);
			      }
			    } 
			    else if (cq->sts == FREE_CHUNK)
				SayDebug("[%2d] ** unexpected free pointer %p at %p in vector\n",
				    self->id, ValueToPtr(v), (void *)p);
			}
		    }
		}
	    }
	    else {
		assert (isRawHdr(hdr));
		int len = GetRawSizeW(hdr);
	      // look for raw values that might be pointers
		for (int i = 0; i < len; i++) {
		    Value_t v = (Value_t)p[i];
		    if (isPtr(v)) {
		        if (isHeapPtr(v)) {
			    MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
			    if (cq->sts != TO_SP_CHUNK) {
				if (cq->sts == FROM_SP_CHUNK)
				   SayDebug("[%2d] ** suspicious looking from-space pointer %p at %p[%d] in raw object of length %d\n",
					self->id, ValueToPtr(v), (void *)p, i, len);
			       else if (IS_VPROC_CHUNK(cq->sts))
				  /* the vproc pointer is pretty common, so filter it out */
				    if (ValueToAddr(v) & ~VP_HEAP_MASK != ValueToAddr(v))
				        SayDebug("[%2d] ** suspicious looking local pointer %p at %p[%d] in raw object of length %d\n",
						 self->id, ValueToPtr(v), (void *)p, i, len);
			       else if (cq->sts == FREE_CHUNK)
				   SayDebug("[%2d] ** suspicious looking free pointer %p at %p[%d] in raw object of length %d\n",
					self->id, ValueToPtr(v), (void *)p, i, len);
			    }
			} 
		    }
		}
		p += len;
	    }
	}
	cp = cp->next;
    }

  // check the VProc structure
#define CHECK_VP(fld)	CheckLocalPtr(self, &(self->fld), "self->" #fld)
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

