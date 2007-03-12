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
#define VP_HEAP_MASK		(VP_HEAP_SZB-1)
#define VP_HEAP_DATA_SZB	(VP_HEAP_SZB - sizeof(VProc_t))

#define MAJOR_GC_THRESHOLD	(VP_HEAP_DATA_SZB >> 1)

#define ALLOC_BUF_SZB		(4*ONE_K)	/* slop at end of nursery */
#define MIN_NURSERY_SZB		(16*ONE_K)	/* minimum nursery size */

/* set the allocation pointer for a vproc */
STATIC_INLINE void SetAllocPtr (VProc_t *vp)
{
    extern Addr_t MaxNurserySzB;
    Addr_t top = (Addr_t)vp + VP_HEAP_SZB;
    Addr_t szB = ROUNDDOWN((top - vp->oldTop) / 2, WORD_SZB);
    if (szB > MaxNurserySzB) szB = MaxNurserySzB;
    vp->allocPtr = top - MaxNurserySzB;
}


/********** Global heap **********/

#define HEAP_CHUNK_SZB		(4*ONE_MEG)

extern Addr_t	GlobalVM;	/* amount of memory allocated to Global heap (including */
				/* free chunks). */
extern Addr_t	FreeVM;		/* amount of free memory in free list */
extern Addr_t	TotalVM;	/* total memory used by heap (including vproc local heaps) */


/********** Exported functions **********/

extern void HeapInit (Options_t *opts);
extern void InitVProcHeap (VProc_t *vp);

#endif /* !_HEAP_H_ */
