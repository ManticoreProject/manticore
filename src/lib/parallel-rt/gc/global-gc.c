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

static Barrier_t	GCBarrier;


/* GlobalGC:
 */
void GlobalGC ()
{
    Die("GlobalGC unimplemented\n");

    /* for each vproc, scan its roots */

    /* scan to-space chunks */

    /* reclaim from-space chunks */

    /* assign a chunk to each vproc */

} /* end of GlobalGC */


/* GCSync:
 *
 * Synchronize the VProcs for a GC of the global heap.
 */
void GCSync (VProc_t *self, bool isFirst)
{
    if (isFirst) {
	BarrierInit (&GCBarrier, NumVProcs);
	for (int i = 0;  i < NumVProcs;  i++) {
	    if (VProcs[i] != self)
		VProcSignal (VProcs[i], GCSignal);
	}
    }

    if (BarrierWait (&GCBarrier)) {
      /* this is the lead VProc, so do the garbage collection */
	GlobalGC ();
    }

  /* wait for the GC to complete */
    BarrierWait (&GCBarrier);

} /* VProcGCSync */
