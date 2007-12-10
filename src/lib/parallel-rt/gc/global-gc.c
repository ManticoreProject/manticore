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

static Mutex_t		GCLock;
static Cond_t		LeaderWait;
static Cond_t		FollowerWait;	// followers block on this until the leader starts the GC
static int		nReadyForGC;	// number of vprocs that are ready for GC
static Barrier_t	GCBarrier;	// for synchronizing on GC completion

static void GlobalGC (VProc_t *vp, Value_t **roots);


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
	    GetChunkForVProc (vp);
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
	  // some other vprc forwarded the object, so return the forwarded
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

    MutexLock (&GCLock);
	if (GlobalGCInProgress) {
	  /* some other vproc has already started the GC */
	    leaderVProc = false;
	    if (++nReadyForGC == NumVProcs)
		CondSignal (&LeaderWait);
	    CondWait (&FollowerWait, &GCLock);
	}
	else {
#ifndef NDEBUG
	    if (DebugFlg)
		SayDebug("[%2d] Initiating global GC\n", self->id);
#endif
	    GlobalGCInProgress = true;
	    leaderVProc = true;
	    nReadyForGC = 0;
	}
    MutexUnlock (&GCLock);

    if (leaderVProc) {
      /* signal the vprocs that global GC is starting and then wait for them
       * to be ready.
       */
	for (int i = 0;  i < NumVProcs;  i++) {
	    if (VProcs[i] != self)
		VProcSignal (VProcs[i], GCSignal);
	}
	BarrierInit (&GCBarrier, NumVProcs);
      /* wait for the followers to be ready */
	MutexLock (&GCLock);
	    CondWait (&LeaderWait, &GCLock);
	MutexUnlock (&GCLock);
      /* initialize global heap for GC */ 
      /* release followers to start GC */
	CondBroadcast (&FollowerWait);
    }

  /* start GC for this vproc */
    GlobalGC (self, roots);

  /* synchronize on every vproc finishing GC */
    BarrierWait (&GCBarrier);

  /* thereclaim from-space pages */
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
/* NOTE: at come point we may want to release memory back to the OS */
	MutexUnlock (&HeapLock);

#ifndef NDEBUG
	if (DebugFlg)
	    SayDebug("[%2d] Initiating global GC\n", self->id);
#endif
    }

} /* end of StartGlobalGC */

/* GlobalGC:
 */
static void GlobalGC (VProc_t *vp, Value_t **roots)
{

#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] Global GC starting\n", vp->id);
#endif

  /* scan the vproc's roots */
    for (int i = 0;  roots[i] != 0;  i++) {
	Value_t p = *roots[i];
	if (isPtr(p)) {
	    if (AddrToChunk(ValueToAddr(p))->sts == FROM_SP_CHUNK) {
		*roots[i] = ForwardObj(vp, p);
	    }
	}
    }

    Die("GlobalGC unimplemented\n");

    /* scan to-space chunks */

    /* reclaim from-space chunks */

    /* assign a chunk to each vproc */

} /* end of GlobalGC */
