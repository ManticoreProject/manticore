/* heap.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _HEAP_H_
#define _HEAP_H_

#include "manticore-rt.h"
#include "vproc.h"
#include "atomic-ops.h"
#include "topology.h"

/* size of a heap chunk */
#define HEAP_CHUNK_SZB		((Addr_t)(4*ONE_MEG))

/********** VProc local heaps **********/

/* VP_HEAP_SZB */		/* defined in machine/sizes.h */
#define VP_HEAP_MASK		((Addr_t)(HEAP_CHUNK_SZB-1))

#define ALLOC_BUF_SZB		((Addr_t)(4*ONE_K))	/* slop at end of nursery */

/* set the allocation pointer for a vproc */
STATIC_INLINE void SetAllocPtr (VProc_t *vp)
{
    vp->allocPtr = vp->heapBase + WORD_SZB;
}

/* return the limit pointer value for the given vproc */
STATIC_INLINE Addr_t LimitPtr (VProc_t *vp)
{
    return vp->heapBase + HEAP_CHUNK_SZB - ALLOC_BUF_SZB;
}

STATIC_INLINE Addr_t SetLimitPtr (VProc_t *vp, Addr_t newLimitPtr)
{
    Value_t oldLimitPtr = AtomicExchangeValue((Value_t*)&(vp->limitPtr), AddrToValue(newLimitPtr));
    return ValueToAddr(oldLimitPtr);
}


/********** Exported functions **********/

extern void HeapInit (Options_t *opts);
extern void InitVProcHeap (VProc_t *vp);
extern void AllocToSpaceChunk (VProc_t *vp);
extern Addr_t AllocVProcMemory (int id, Location_t loc);

extern uint32_t	NumGlobalGCs;

#ifndef NO_GC_STATS
extern void ReportGCStats ();
#endif

#endif /* !_HEAP_H_ */
