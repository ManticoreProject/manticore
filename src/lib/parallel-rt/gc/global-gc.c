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

static Mutex_t		GCLock;
static Cond_t		LeaderWait;
static Cond_t		FollowerWait;	// followers block on this until the leader starts the GC
static int		nReadyForGC;	// number of vprocs that are ready for GC
static Barrier_t	GCBarrier;	// for synchronizing on GC completion

static void GlobalGC (VProc_t *vp, Value_t **roots);


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

} /* end of StartGlobalGC */

/* GlobalGC:
 */
static void GlobalGC (VProc_t *vp, Value_t **roots)
{
    Die("GlobalGC unimplemented\n");

    /* for each vproc, scan its roots */

    /* scan to-space chunks */

    /* reclaim from-space chunks */

    /* assign a chunk to each vproc */

} /* end of GlobalGC */
