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

/********** Global heap **********/

#endif /* !_HEAP_H_ */
