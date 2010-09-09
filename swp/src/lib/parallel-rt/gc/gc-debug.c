/* gc-debug.c
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Debugging routines for GC.
 */

#include "manticore-rt.h"
#include "heap.h"
#include "gc.h"
#include "vproc.h"
#include "gc-inline.h"
#include "bibop.h"
#include "internal-heap.h"

bool isGlobalHeapPtr (Value_t v)
{
  assert(isPtr(v));
  assert(AddrToChunk(ValueToAddr(v)) != 0);
  assert(AddrToChunk(ValueToAddr(v))->sts == TO_SP_CHUNK);
}

/*! \brief check for obviously corrupt objects in the local heap
 *  \param vp host vproc
 *  \param beginHeap low address of the heap
 *  \param endHeap high address of the heap
 */
void LocalHeapConsistencyCheck (VProc_t *vp, Word_t *beginHeap, Word_t *endHeap)
{
    Word_t	*nextScan = beginHeap;
    Addr_t	nurseryBase = vp->nurseryBase;
    Addr_t	allocSzB = vp->allocPtr - nurseryBase - WORD_SZB;

    while (nextScan < endHeap-1) {
		Word_t hdr = *nextScan;	// get object header
		Word_t *scanptr = nextScan;
		if (isForwardPtr(hdr)) {
			// object in an older generation
		}
		else {
	  // we can just skip raw objects
	    tableDebug[getID(hdr)].gc_debug(scanptr,nurseryBase,allocSzB);
		}
		nextScan += GetLength(hdr);
	}

}

void DebugPromote (VProc_t *vp, char *loc, char *lhs, char *rhs)
{
#ifndef NDEBUG
    if (DebugFlg)
        SayDebug("[%2d] %s: let %s = promote(%s)\n", vp->id, loc, lhs, rhs);
#endif
}
