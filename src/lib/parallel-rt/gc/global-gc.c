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

static Mutex_t		GCLock;		// Lock that protects the following variables:
static Cond_t		LeaderWait;	// The leader waits on this for the followers
static Cond_t		FollowerWait;	// followers block on this until the leader starts the GC
static int		NReadyForGC;	// number of vprocs that are ready for GC
static Barrier_t	GCBarrier;	// for synchronizing on GC completion
static bool		GlobalGCInProgress; // true, when a global GC has been initiated
uint32_t		NumGlobalGCs;	// the total number of global GCs.

#ifndef NO_GC_STATS
static uint64_t		FromSpaceSzb;
static uint64_t		NWordsScanned;
static uint64_t		NBytesCopied;
#endif

static void GlobalGC (VProc_t *vp, Value_t **roots);
static void ScanVProcHeap (VProc_t *vp);
static void ScanGlobalToSpace (VProc_t *vp);
#ifndef NDEBUG
static void CheckGC (VProc_t *self, Value_t **roots);
#endif

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

/* \brief attempt to start a global GC.
 * \param vp the host vproc
 * \param roots the array of root pointers for this vproc
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
	    NReadyForGC = 1;
	    BarrierInit (&GCBarrier, NumVProcs);
	    NumGlobalGCs++;
	    LogGlobalGCInit (self, NumGlobalGCs);
#ifndef NDEBUG
	    if (GCDebug >= GC_DEBUG_GLOBAL)
		SayDebug("[%2d] Initiating global GC %d\n", self->id, NumGlobalGCs);
#endif
#ifndef NO_GC_STATS
	    FromSpaceSzb = 0;
	    NWordsScanned = 0;
	    NBytesCopied = 0;
#endif
	for (int i = 0;  i < NumVProcs;  i++)
	    if (VProcs[i] != self)
		VProcGlobalGCInterrupt (self, VProcs[i]);
	} else {
	    leaderVProc = false;
	    if (++NReadyForGC == NumVProcs)
		CondSignal (&LeaderWait);
	    CondWait (&FollowerWait, &GCLock);
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

    MutexUnlock (&GCLock);

  /* finish the GC setup for this vproc */
    self->globToSpTl = (MemChunk_t *)0;
    self->globToSpHd = (MemChunk_t *)0;
#ifndef NO_GC_STATS
    self->nWordsScanned = 0;
    self->nBytesCopied = 0;
#endif

    if (leaderVProc) {
      /* reset the size of to-space */
	ToSpaceSz = 0;
	MutexLock(&GCLock);
	    while (NReadyForGC < NumVProcs)
		CondWait(&LeaderWait, &GCLock);
	    CondBroadcast(&FollowerWait);
	MutexUnlock (&GCLock);
    }

    MutexLock(&GCLock);

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
    CheckGC (self, roots);
#endif

    MutexUnlock(&GCLock);

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
		cp->next = FreeChunks;
		FreeChunks = cp;
		cp = cq;
	    }
/* NOTE: at some point we may want to release memory back to the OS */
	    GlobalGCInProgress = false;
	MutexUnlock (&HeapLock);
    }

    LogGlobalGCEnd (self, NumGlobalGCs);

  /* synchronize on every vproc finishing GC */
    BarrierWait (&GCBarrier);

#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_GLOBAL) {
	if (leaderVProc)
	    SayDebug("[%2d] Completed global GC; %ld words scanned; %ld/%ld bytes copied\n",
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

#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_GLOBAL)
	SayDebug("[%2d] Global GC starting\n", vp->id);
#endif

  /* scan the vproc's roots */
    for (int i = 0;  roots[i] != 0;  i++) {
	Value_t p = *roots[i];
	if (isGlobalFromSpacePtr(p)) {
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
		    if (isGlobalFromSpacePtr(p)) {
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
		if (isGlobalFromSpacePtr(v)) {
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
    Word_t	*scanTop = GlobalScanTop(vp, scanChunk);

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
			if (isGlobalFromSpacePtr(p)) {
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
		    if (isGlobalFromSpacePtr(v)) {
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
	    scanTop = GlobalScanTop(vp, scanChunk);
	}
	else
	    scanTop = (Word_t *)(scanChunk->usedTop);

    } while (scanPtr < scanTop);

}

#ifndef NDEBUG
/* Check the invariant that addr is a pointer living in either the root set or the local heap. 
 * Precondition: this check should only occur just after a global collection.
 */
static void CheckLocalPtrPostGGC (VProc_t *self, void *addr, const char *where)
{
    Value_t v = *(Value_t *)addr;
    if (isPtr(v)) {
	MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
	if (cq->sts == TO_SP_CHUNK)
	    return;
	else if (cq->sts == FROM_SP_CHUNK)
	    SayDebug("** unexpected from-space pointer %p at %p in %s\n",
		ValueToPtr(v), addr, where);
	else if (IS_VPROC_CHUNK(cq->sts)) {
	    if (cq->sts != VPROC_CHUNK(self->id)) {
		SayDebug("** unexpected remote pointer %p at %p in %s\n",
		    ValueToPtr(v), addr, where);
	    }
	    else if (! inAddrRange(VProcHeap(self), self->oldTop - VProcHeap(self), ValueToAddr(v))) {
		SayDebug("** local pointer %p at %p in %s is out of bounds\n",
		    ValueToPtr(v), addr, where);
	    }
	}
	else if (cq->sts == FREE_CHUNK) {
	    SayDebug("** unexpected free-space pointer %p at %p in %s\n",
		ValueToPtr(v), addr, where);
	}
    }
}
static void CheckGC (VProc_t *self, Value_t **roots)
{
    if (GCDebug >= GC_DEBUG_GLOBAL)
      SayDebug ("  Checking heap consistency\n");

  // check the roots
    for (int i = 0;  roots[i] != 0;  i++) {
	char buf[16];
	sprintf(buf, "root[%d]", i);
	Value_t v = *roots[i];
	CheckLocalPtrPostGGC (self, roots[i], buf);
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
			CheckLocalPtrPostGGC (self, scanP, "local mixed object");
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
		    CheckLocalPtrPostGGC (self, p, "local vector");
		}
	    }
	    else {
		assert (isRawHdr(hdr));
	      // we can just skip raw objects
		p += GetRawSizeW(hdr);
	    }
	}
    }

  // check to space
    MemChunk_t	*cp = self->globToSpHd;
    while (cp != (MemChunk_t *)0) {
	assert (cp->sts = TO_SP_CHUNK);
	Word_t *p = (Word_t *)(cp->baseAddr);
	Word_t *top = GlobalScanTop(self, cp);
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
				    SayDebug("** unexpected from-space pointer %p at %p in mixed object\n",
					ValueToPtr(v), p);
				else if (IS_VPROC_CHUNK(cq->sts))
				    SayDebug("** unexpected local pointer %p at %p in mixed object\n",
					ValueToPtr(v), p);
				else if (cq->sts == FREE_CHUNK)
				    SayDebug("** unexpected free pointer %p at %p in mixed object\n",
					ValueToPtr(v), p);
			    }
			}
		    }
		    else {
		      /* check for possible pointers in non-pointer fields */
			Value_t v = *(Value_t *)scanP;
			if (isPtr(v)) {
			    MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
			    switch (cq->sts) {
			      case FREE_CHUNK:
				SayDebug(" ** possible free-space pointer %p in mixed object %p+%d\n",
				    v, p, scanP-p);
				break;
			      case TO_SP_CHUNK:
				SayDebug(" ** possible to-space pointer %p in mixed object %p+%d\n",
				    v, p, scanP-p);
				break;
			      case FROM_SP_CHUNK:
				SayDebug(" ** possible from-space pointer %p in mixed object %p+%d\n",
				    v, p, scanP-p);
				break;
			      case UNMAPPED_CHUNK:
				break;
			      default:
				if (IS_VPROC_CHUNK(cq->sts)) {
				  /* the vproc pointer is pretty common, so filter it out */
				    if ((Addr_t)v & ~VP_HEAP_MASK != (Addr_t)v)
					SayDebug(" ** possible local pointer %p in mixed object %p+%d\n",
					    v, p, scanP-p);
				}
				else {
				    SayDebug(" ** strange pointer %p in mixed object %p+%d\n",
					v, p, scanP-p);
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
		    if (isGlobalFromSpacePtr(v)) {
			if (isPtr(v)) {
			    MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
			    if (cq->sts != TO_SP_CHUNK) {
				if (cq->sts == FROM_SP_CHUNK)
				    SayDebug("** unexpected from-space pointer %p at %p in vector\n",
					ValueToPtr(v), p);
				else if (IS_VPROC_CHUNK(cq->sts))
				    SayDebug("** unexpected local pointer %p at %p in vector\n",
					ValueToPtr(v), p);
				else if (cq->sts == FREE_CHUNK)
				    SayDebug("** unexpected free pointer %p at %p in vector\n",
					ValueToPtr(v), p);
			    }
			}
		    }
		}
	    }
	    else {
		assert (isRawHdr(hdr));
	      // we can just skip raw objects
		p += GetRawSizeW(hdr);
	    }
	}
	cp = cp->next;
    }

}
#endif

