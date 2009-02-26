/* global-gc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

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

static Mutex_t		GCLock;
static Cond_t		LeaderWait;
static Cond_t		FollowerWait;	// followers block on this until the leader starts the GC
static int		nReadyForGC;	// number of vprocs that are ready for GC
static int		nParticipants;	// number of vprocs participating in the GC
static Barrier_t	GCBarrier;	// for synchronizing on GC completion
static bool		GlobalGCInProgress; // true, when a global GC has been initiated

static void GlobalGC (VProc_t *vp, Value_t **roots);
static void ScanVProcHeap (VProc_t *vp);
static void ScanGlobalToSpace (VProc_t *vp);


/* Return true if a value is a from-space pointer.  Note that this function relies on
 * the fact that unmapped addresses are mapped to the "UnmappedChunk" by the BIBOP.
 */
STATIC_INLINE bool isFromSpacePtr (Value_t p)
{
    return (isPtr(p) && AddrToChunk(ValueToAddr(p))->sts == FROM_SP_CHUNK);

}

/* Forward an object into the global-heap chunk reserved for the current VP */
STATIC_INLINE Value_t ForwardObj (VProc_t *vp, Value_t v)
{
    Word_t	*p = ((Word_t *)ValueToPtr(v));
    Word_t	oldHdr = p[-1];
    if (isForwardPtr(oldHdr))
	return PtrToValue(GetForwardPtr(oldHdr));
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

STATIC_INLINE Word_t *ScanTop (VProc_t *vp, MemChunk_t *scanChunk)
{
    if (vp->globToSpTl == scanChunk)
      /* NOTE: we must subtract WORD_SZB here because globNextW points to the first
       * data word of the next object (not the header word)!
       */
	return (Word_t *)(vp->globNextW - WORD_SZB);
    else
	return (Word_t *)(scanChunk->usedTop);
}


/* \brief initialize the data structures that support global GC
 */
void InitGlobalGC ()
{
    MutexInit (&GCLock);
    CondInit (&LeaderWait);
    CondInit (&FollowerWait);
    GlobalGCInProgress = false;
}

/* \brief attempt to start a global GC.
 * \param vp the host vproc
 * \param roots the array of root pointers for this vproc
 */
void StartGlobalGC (VProc_t *self, Value_t **roots)
{
    bool	leaderVProc;

    self->globalGCInProgress = true;
    self->globalGCPending = false;
    self->sigPending = false;

    MutexLock (&GCLock);
	if (GlobalGCInProgress) {
	  /* some other vproc has already started the GC */
	    leaderVProc = false;
	    if (++nReadyForGC == nParticipants)
		CondSignal (&LeaderWait);
	    CondWait (&FollowerWait, &GCLock);
	}
	else {
	    LogGlobalGCInit (self);
#ifndef NDEBUG
	    if (DebugFlg)
	      SayDebug("[%2d] Initiating global GC\n", self->id);
#endif
	    GlobalGCInProgress = true;
	    leaderVProc = true;
	    nReadyForGC = 0;
/* FIXME: we probably should initialize nParticipants here, instead of down below! */
	}
      /* add the vproc's pages to the from-space list */
	MemChunk_t *p = self->globToSpTl;
	if (p != (MemChunk_t *)0) {
	    p->next = FromSpaceChunks;
	    FromSpaceChunks = p;
	}
    MutexUnlock (&GCLock);

  /* finish the GC setup for this vproc */
    for (MemChunk_t *p = self->globToSpHd;  p != (MemChunk_t *)0;  p = p->next) {
	p->sts = FROM_SP_CHUNK;
    }
    self->globToSpTl->usedTop = self->globNextW - WORD_SZB;
    self->globToSpTl = (MemChunk_t *)0;
    self->globToSpHd = (MemChunk_t *)0;

    if (leaderVProc) {
      /* reset the size of to-space */
	ToSpaceSz = 0;
      /* signal the vprocs that global GC is starting and then wait for them
       * to be ready.
       */
	MutexLock (&GCLock);
	    nParticipants = 1;
	    for (int i = 0;  i < NumVProcs;  i++) {
	      /* FIXME: this protocol would allow the following situation: just before
	       * reaching this point VProcs[i] is woken up (see vproc.c:247). Before the
	       * idle flag becomes false we reach the test below and identify the vproc
	       * as idle. Thus, this ith vproc could execute in parallel with the parallel
	       * GC, which would certainly result in memory corruption.
	       */
		if ((VProcs[i] != self) && (! VProcs[i]->idle)) {
		    nParticipants++;
		    VProcSignal (VProcs[i], GCSignal);
		}
	    }
	MutexUnlock (&GCLock);
	BarrierInit (&GCBarrier, nParticipants);
      /* wait for follower vprocs */
	if (nParticipants > 1) {
	  /* wait for the followers to be ready */
	    MutexLock (&GCLock);
		CondWait (&LeaderWait, &GCLock);
	    MutexUnlock (&GCLock);
	  /* release followers to start GC */
	    CondBroadcast (&FollowerWait);
	}
    }

  /* allocate the initial chunk for the vproc */
    AllocToSpaceChunk (self);

  /* start GC for this vproc */
    GlobalGC (self, roots);

  /* synchronize on every vproc finishing GC */
    BarrierWait (&GCBarrier);

  /* the leader reclaims the from-space pages */
    if (leaderVProc) {
	MutexLock (&HeapLock);
	    MemChunk_t *cp = FromSpaceChunks;
	    FromSpaceChunks = (MemChunk_t *)0;
	    while (cp != (MemChunk_t *)0) {
		cp->sts = FREE_CHUNK;
		cp->usedTop = cp->baseAddr;
		MemChunk_t *cq = cp->next;
		cp->next = FreeChunks;
		FreeChunks = cp;
		cp = cq;
	    }
/* NOTE: at some point we may want to release memory back to the OS */
	    GlobalGCInProgress = false;
	MutexUnlock (&HeapLock);

#ifndef NDEBUG
	if (DebugFlg)
	    SayDebug("[%2d] Completed global GC\n", self->id);
#endif
    }

    LogGlobalGCEnd (self);

} /* end of StartGlobalGC */

/* GlobalGC:
 */
static void GlobalGC (VProc_t *vp, Value_t **roots)
{
    LogGlobalGCVPStart (vp);

#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] Global GC starting\n", vp->id);
#endif

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
    if (DebugFlg)
	SayDebug("[%2d] Global GC finished\n", vp->id);
#endif

} /* end of GlobalGC */

/*! \brief Scan the vproc's local heap.  Since we have already done a major GC,
 *  all objects are known to be live.
 */
static void ScanVProcHeap (VProc_t *vp)
{
    Word_t *top = (Word_t *)(vp->oldTop);
    Word_t *scanPtr = (Word_t *)VProcHeap(vp);

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

} /* end of ScanVProcHeap */

/*! \brief Scan the to-space objects that have been copied by this vproc
 *  \param vp the vproc doing the scanning
 */
static void ScanGlobalToSpace (VProc_t *vp)
{
    MemChunk_t	*scanChunk = vp->globToSpHd;
    Word_t	*scanPtr = (Word_t *)(scanChunk->baseAddr);
    Word_t	*scanTop = ScanTop(vp, scanChunk);

    do {
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
	    scanTop = ScanTop(vp, scanChunk);
	}
	else
	    scanTop = (Word_t *)(scanChunk->usedTop);

    } while (scanPtr < scanTop);

}
