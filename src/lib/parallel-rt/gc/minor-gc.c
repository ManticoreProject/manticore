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
#include "inline-log.h"

extern Addr_t	MajorGCThreshold; /* when the size of the nursery goes below this limit */
				/* it is time to do a GC. */

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
	p[-1] = MakeForwardPtr(hdr, newObj);
	return PtrToValue(newObj);
    }

}

/* MinorGC:
 */
void MinorGC (VProc_t *vp, Value_t **roots)
{
    LogEvent0 (vp, MinorGCStartEvt);

    Addr_t	nurseryBase = vp->nurseryBase;
    Addr_t	allocSzB = vp->allocPtr - nurseryBase - WORD_SZB;
    Word_t	*nextScan = (Word_t *)(vp->oldTop); /* current top of to space */
    Word_t	*nextW = nextScan + 1; /* next word in to space to copy to */

    assert (VProcHeap(vp) <= (Addr_t)nextScan);
    assert ((Addr_t)nextScan < vp->nurseryBase);
    assert (vp->nurseryBase < vp->allocPtr);

#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] Minor GC starting\n", vp->id);
#endif

  /* process the roots */
    for (int i = 0;  roots[i] != 0;  i++) {
	Value_t p = *roots[i];
	if (isPtr(p)) {
	    if (inAddrRange(nurseryBase, allocSzB, ValueToAddr(p))) {
		*roots[i] = ForwardObj(p, &nextW);
	    }
	}
    }

  /* scan to space */
    while (nextScan < nextW-1) {
	assert ((Addr_t)(nextW-1) <= vp->nurseryBase);
	Word_t hdr = *nextScan++;	// get object header
	if (isMixedHdr(hdr)) {
	  // a record
	    Word_t tagBits = GetMixedBits(hdr);
	    assert (tagBits < (1 << GetMixedSizeW(hdr)));
	    Value_t *scanP = (Value_t *)nextScan;
	    while (tagBits != 0) {
		if (tagBits & 0x1) {
		    Value_t v = *scanP;
		    if (isPtr(v) && inAddrRange(nurseryBase, allocSzB, ValueToAddr(v))) {
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
		if (isPtr(v) && inAddrRange(nurseryBase, allocSzB, ValueToAddr(v))) {
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

    assert ((Addr_t)nextScan >= VProcHeap(vp));
    Addr_t avail = VP_HEAP_SZB - ((Addr_t)nextScan - VProcHeap(vp));
#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] Minor GC finished: %ld/%ld bytes live; %d available\n",
	    vp->id, (Addr_t)nextScan - vp->oldTop,
	    vp->allocPtr - vp->nurseryBase - WORD_SZB,
	    (int)avail);
#endif

    LogEvent0 (vp, MinorGCEndEvt);

    if (avail < MajorGCThreshold) {
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
