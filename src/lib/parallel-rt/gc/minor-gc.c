/* minor-gc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Minor GCs are local collections of a vproc's allocation space.
 */

#include "manticore-rt.h"
#include "heap.h"
#include "gc.h"
#include "vproc.h"
#include "gc-inline.h"

/* Copy an object to the old region */
STATIC_INLINE Value_t ForwardObj (Value_t v, Word_t **nextW)
{
    Word_t	*p = (Word_t *)ValueToPtr(v);
    Word_t	hdr = p[-1];
    if (isForwardPtr(hdr))
	return PtrToValue(GetForwardPtr(hdr));
    else {
	int len = GetLength(hdr);
	Word_t *newObj = *nextW;
	newObj[-1] = hdr;
	for (int i = 0;  i < len;  i++) {
	    newObj[i] = p[i];
	}
	*nextW = newObj+len+1;
	*p = MakeForwardPtr(hdr, newObj);
	return PtrToValue(newObj);
    }

}

/* MinorGC:
 */
void MinorGC (VProc_t *vp, Value_t **roots)
{
    Addr_t	heapBase = VProcHeap(vp);
    Word_t	*nextW = (Word_t *)(vp->oldTop); /* next word in to space to copy to */
    Word_t	*nextScan = nextW;		/* current top of to space */

  /* process the roots */
    for (int i = 0;  roots[i] != 0;  i++) {
	Value_t p = *roots[i];
	if (isPtr(p)) {
	    if (inVPHeap(heapBase, (Addr_t)p)) {
		*roots[i] = ForwardObj(p, &nextW);
	    }
	}
    }

  /* scan to space */
    while (nextScan < nextW-1) {
	Word_t hdr = *nextScan++;	// get object header
	if (isMixedHdr(hdr)) {
	  // a record
	    Word_t tagBits = GetMixedBits(hdr);
	    Value_t *scanP = (Value_t *)nextScan;
	    while (tagBits != 0) {
		if (tagBits & 0x1) {
		    Value_t v = *scanP;
		    if (inVPHeap(heapBase, ValueToAddr(v))) {
			*scanP = ForwardObj(v, &nextW);
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
		if (isPtr(v) && inVPHeap(heapBase, ValueToAddr(v))) {
		    *nextScan = (Word_t)ForwardObj(v, &nextW);
		}
	    }
	}
	else {
	  // we can just skip raw objects
	    assert (isRawHdr(hdr));
	    nextScan += GetRawSizeW(hdr);
	}
    }

    Addr_t avail = VP_HEAP_SZB - ((Addr_t)nextScan - (Addr_t)vp);
    if (avail < MAJOR_GC_THRESHOLD) {
      /* time to do a major collection. */
	MajorGC (vp, roots, (Addr_t)nextScan);
    }
    else {
      /* remember information about the final state of the heap */
	vp->oldTop = (Addr_t)nextScan;
    }

  /* reset the allocation pointer */
    SetAllocPtr (vp);

}
