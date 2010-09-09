/* gc.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Public interface to the garbage collector.
 */

#ifndef _GC_H_
#define _GC_H_

#include "manticore-rt.h"
#include <stdlib.h>

extern void MinorGC (VProc_t *vp);
extern void MajorGC (VProc_t *vp, Value_t **roots, Addr_t top);
extern void StartGlobalGC (VProc_t *vp, Value_t **roots);
extern Value_t PromoteObj (VProc_t *vp, Value_t root);

#endif /* !_GC_H_ */
