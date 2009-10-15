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

extern Addr_t	MajorGCThreshold;	/* when the size of the nursery goes below */
					/* this limit it is time to do a GC. */

/* MinorGC:
 */
void MinorGC (VProc_t *vp)
{
#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_MINOR)
	SayDebug("[%2d] Get another memory chunk\n", vp->id);
#endif

#ifndef NO_GC_STATS
    vp->nMinorGCs++;
    vp->minorStats.nBytesAlloc += vp->allocPtr - vp->heapBase - WORD_SZB;/* FIXME */
//    vp->minorStats.nBytesCopied += (Addr_t)nextScan - vp->oldTop;
#endif

    if (vp->globalGCPending || (ToSpaceSz >= ToSpaceLimit)) {
      /* gather the roots.  The protocol is that the stdCont register holds
       * the return address (which is not in the heap) and that the stdEnvPtr
       * holds the GC root.
       */
	int nWorkStealingRoots = M_NumDequeRoots (vp);
	Value_t *roots[16 + nWorkStealingRoots], **rp;
	rp = roots;
	*rp++ = &(vp->currentFLS);
	*rp++ = &(vp->actionStk);
	*rp++ = &(vp->schedCont);
	*rp++ = &(vp->dummyK);
	*rp++ = &(vp->wakeupCont);
	*rp++ = &(vp->shutdownCont);
	*rp++ = &(vp->rdyQHd);
	*rp++ = &(vp->rdyQTl);
	*rp++ = &(vp->landingPad);
	*rp++ = &(vp->stdEnvPtr);
	rp = M_AddDequeEltsToLocalRoots(vp, rp);
	*rp++ = 0;

	StartGlobalGC (vp, roots);
    }
    else {
      // allocate another chunk
	AllocToSpaceChunk (vp);
    }

  /* reset the allocation and limit pointers */
    SetAllocPtr (vp);
    if (SetLimitPtr(vp, vp->allocTop - ALLOC_BUF_SZB) == 0)
	SetLimitPtr(vp, 0);  // pending preemption

}

