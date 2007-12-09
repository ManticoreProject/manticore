/* heap.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _HEAP_H_
#define _HEAP_H_

#include "manticore-rt.h"
#include "vproc.h"

/********** VProc local heaps **********/

/* VP_HEAP_SZB */		/* defined in manticore-rt.h */
#define VP_HEAP_MASK		((Addr_t)(VP_HEAP_SZB-1))
#define VP_HEAP_DATA_SZB	((Addr_t)(VP_HEAP_SZB - sizeof(VProc_t)))

#define MAJOR_GC_THRESHOLD	((Addr_t)(VP_HEAP_DATA_SZB >> 1))

#define ALLOC_BUF_SZB		((Addr_t)(4*ONE_K))	/* slop at end of nursery */
#define MIN_NURSERY_SZB		((Addr_t)(16*ONE_K))	/* minimum nursery size */

/* set the allocation pointer for a vproc */
STATIC_INLINE void SetAllocPtr (VProc_t *vp)
{
    extern Addr_t MaxNurserySzB;
    Addr_t top = (Addr_t)vp + VP_HEAP_SZB;
    Addr_t szB = ROUNDDOWN((top - vp->oldTop) / 2, WORD_SZB);
    if (szB > MaxNurserySzB) szB = MaxNurserySzB;
    vp->nurseryBase = (top - szB);
    vp->allocPtr = vp->nurseryBase + WORD_SZB;
}


/********** Exported functions **********/

extern void HeapInit (Options_t *opts);
extern void InitVProcHeap (VProc_t *vp);

#endif /* !_HEAP_H_ */
