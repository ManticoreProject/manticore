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

static bool isGlobalHeapPtr (Value_t v)
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
	if (isMixedHdr(hdr)) {
	  // a record
	    Word_t tagBits = GetMixedBits(hdr);
	    assert ((uint64_t)tagBits < (1l << (uint64_t)GetMixedSizeW(hdr)));
	    Value_t *scanP = (Value_t *)nextScan;
	    while (tagBits != 0) {
		if (tagBits & 0x1) {
		    Value_t v = *scanP;
		    if (isPtr(v)) {
			if (inAddrRange(nurseryBase, allocSzB, ValueToAddr(v))) { // pointer into the nursery
			  // only young to old pointers allowed in the nursery
			    assert(ValueToAddr(v) < (Addr_t)nextScan);
			}
			else { // in the global heap
			  assert(isGlobalHeapPtr(v));
			}
		    }
		}
		tagBits >>= 1;
		scanP++;
	    }
	    nextScan += GetMixedSizeW(hdr);
	}
	else if (isVectorHdr(hdr)) {
	  // an array of pointers
	    int len = GetVectorLen(hdr);
	    for (int i = 0;  i < len;  i++, nextScan++) {
		Value_t v = *(Value_t *)nextScan;
		if (isPtr(v)) {
		    if (inAddrRange(nurseryBase, allocSzB, ValueToAddr(v))) {  // pointer into the nursery
		      // only young to old pointers allowed in the nursery
			assert(ValueToAddr(v) < (Addr_t)nextScan);
		    }
		    else {
		      // object in an older generation
		      assert(isGlobalHeapPtr(v));
		    }
		}
	    }
	    nextScan += len;
	}
	else if (isForwardPtr(hdr)) {
	  // object in an older generation
	}
	else {
	  // we can just skip raw objects
	    assert (isRawHdr(hdr));
	    nextScan += GetRawSizeW(hdr);
	}
    }

}

void DebugPromote (VProc_t *vp, char *loc, char *lhs, char *rhs)
{
#ifndef NDEBUG
    if (DebugFlg)
        SayDebug("[%2d] %s: let %s = promote(%s)\n", vp->id, loc, lhs, rhs);
#endif
}
