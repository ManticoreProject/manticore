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

/*! \brief check for obviously corrupt objects in a heap
 *  \param vp host vproc
 *  \param beginHeap low address of the heap
 *  \param endHeap high address of the heap
 */
void HeapConsistencyCheck (VProc_t *vp, Word_t *beginHeap, Word_t *endHeap)
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
			else {
			  // raw value or object in an older generation
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
		    }
		}
	    }
	    nextScan += len;
	}
	else if (isForwardPtr(hdr)) {
	  // object in an older generation
	  // TODO: check that the object in the older generation is consistent with this one
	}
	else {
	  // we can just skip raw objects
	    assert (isRawHdr(hdr));
	    nextScan += GetRawSizeW(hdr);
	}
    }

}
