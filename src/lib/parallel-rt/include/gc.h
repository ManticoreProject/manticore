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

extern Value_t NewStack (VProc_t *vp, Value_t funClos);
extern StackInfo_t* NewMainStack (VProc_t *vp, void** initialSP);
extern void FreeStackMem (VProc_t *vp, StackInfo_t* info);
extern void* GetStkLimit (StackInfo_t* info);
extern void WarmUpFreeList(VProc_t* vp, uint64_t numBytes);

extern void InvalidReturnAddr();
extern void EndOfStack();

#endif /* !_GC_H_ */
