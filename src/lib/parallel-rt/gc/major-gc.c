/* major-gc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A major GC is the process of copying live data from a VProc's local heap
 * to the global heap.  It will occur immediately after a minor GC in the
 * case where the amount of free space falls below some threshold.  
 *
 * TODO:
 *	check heap limit when forwarding objects
 *	check for global GC.  How?
 */

#include "manticore-rt.h"
#include <string.h>
#include "heap.h"
#include "gc.h"
#include "vproc.h"
#include "gc-inline.h"

static void ScanGlobalToSpace (
	VProc_t *vp, Addr_t heapBase, Word_t *globScan, Word_t *globNext);

/* return true of the given address is within the old region of the heap */
STATIC_INLINE bool inOldHeap (Addr_t heapBase, Addr_t oldSzB, Addr_t p)
{
    return ((p - heapBase) < oldSzB);
}

/* Forward an object into the global-heap chunk reserved for the current VP */
STATIC_INLINE Value_t ForwardObj (Value_t v, Word_t **nextW)
{
    Word_t	*p = ValueToPtr(v)-1;  // address of object header
    Word_t	hdr = *p;
    if (isForwardPtr(hdr))
	return PtrToValue(GetForwardPtr(hdr));
    else {
	int len = GetLength(hdr);
/* What about the heap limit?? */
	Word_t *np = *nextW;
	for (int i = 0;  i <= len;  i++) {
	    np[i] = p[i];
	}
	Word_t *newObj = np+1;
	*nextW = newObj+len;
	*p = MakeForwardPtr(hdr, newObj);
	return PtrToValue(newObj);
    }

}

/* MajorGC:
 */
void MajorGC (VProc_t *vp, Value_t **roots, Addr_t top)
{
    Addr_t	heapBase = VProcHeap(vp);
    Addr_t	oldSzB = vp->oldTop - heapBase;
    Word_t	*globScan = (Word_t *)vp->globNextW;
    Word_t	*globNext = (Word_t *)vp->globNextW;

  /* process the roots */
    for (int i = 0;  roots[i] != 0;  i++) {
	Value_t p = *roots[i];
	if (isPtr(p)) {
	    if (inOldHeap(heapBase, oldSzB, (Addr_t)p)) {
		*roots[i] = ForwardObj(p, &globNext);
	    }
	    else if (inVPHeap(heapBase, (Addr_t)p)) {
	      // p points to another object in the "young" region,
	      // so adjust it.
		*roots[i] = AddrToValue(ValueToAddr(p) - oldSzB);
	    }
	}
    }

  /* we also treat the data between vproc->oldTop and top as roots, since
   * it is known to be both young and live.  While scanning it, we also
   * do pointer translation on the internal pointers in preparation for
   * copying it to the bottom of the heap after GC.
   */
    Word_t *nextScan = (Word_t *)(vp->oldTop);
    while (nextScan < (Word_t *)top) {
	Word_t hdr = *nextScan++;
	if (isMixedHdr(hdr)) {
	  // a record
	    Word_t tagBits = GetMixedBits(hdr);
	    Word_t *scanP = nextScan;
	    while (tagBits != 0) {
		if (tagBits & 0x1) {
		    Value_t p = *(Value_t *)scanP;
		    if (inOldHeap(heapBase, oldSzB, ValueToAddr(p))) {
			*scanP = (Word_t)ForwardObj(p, &globNext);
		    }
		    else if (inVPHeap(heapBase, ValueToAddr(p))) {
		      // p points to another object in the "young" region,
		      // so adjust it.
			*scanP = (Word_t)AddrToValue(ValueToAddr(p) - oldSzB);
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
	    for (int i = 0;  i < len;  i++) {
		Value_t v = (Value_t)*nextScan;
		if (isPtr(v)) {
		    if (inOldHeap(heapBase, oldSzB, ValueToAddr(v))) {
			*nextScan = (Word_t)ForwardObj(v, &globNext);
		    }
		    else if (inVPHeap(heapBase, (Addr_t)v)) {
		      // p points to another object in the "young" region,
		      // so adjust it.
			*nextScan = (Word_t)((Addr_t)v - oldSzB);
		    }
		    nextScan++;
		}
	    }
	}
	else {
	  // we can just skip raw objects; note that promoted objects
	  // are not possible here.
	    assert(isRawHdr(hdr));
	    nextScan += GetRawSizeW(hdr);
	}
    }

  /* scan to-space objects */
    ScanGlobalToSpace (vp, heapBase, globScan, globNext);

  /* copy the live data between vp->oldTop and top to the base of the heap */
    Addr_t youngSzB = top - vp->oldTop;
    memcpy ((void *)VProcHeap(vp), (void *)(vp->oldTop), youngSzB);
    vp->oldTop = VProcHeap(vp) + youngSzB;

} /* end of MajorGC */


/* PromoteObj:
 *
 * Promote an object and anything that it transitively refers to to the
 * global heap.
 */
Value_t PromoteObj (VProc_t *vp, Value_t root)
{
    Addr_t	heapBase = VProcHeap(vp);

  /* NOTE: the following test probably ought to happen before the runtime
   * system gets called.
   */
    if (isPtr(root) && inVPHeap(heapBase, ValueToAddr(root))) {
	Word_t	*globScan = (Word_t *)vp->globNextW;
	Word_t	*globNext = (Word_t *)vp->globNextW;

      /* promote the root to the global heap */
	root = ForwardObj (root, &globNext);

      /* promote any reachable values */
	ScanGlobalToSpace (vp, heapBase, globScan, globNext);
    }

    return root;

}

/* Scan the objects that have been copied to the global heap */
static void ScanGlobalToSpace (
    VProc_t *vp,
    Addr_t heapBase,
    Word_t *globScan,
    Word_t *globNext)
{
    while (globScan < globNext) {
	Word_t hdr = *globScan++;	// get object header
	if (isMixedHdr(hdr)) {
	  // a record
	    Word_t tagBits = GetMixedBits(hdr);
	    Word_t *scanP = globScan;
	    while (tagBits != 0) {
		if (tagBits & 0x1) {
		    Value_t p = *(Value_t *)scanP;
		    if (inVPHeap(heapBase, ValueToAddr(p))) {
			*scanP = (Word_t)ForwardObj(p, &globNext);
		    }
		}
		tagBits >>= 1;
		scanP++;
	    }
	    globScan += GetMixedSizeW(hdr);
	}
	else if (isVectorHdr(hdr)) {
	  // an array of pointers
	    int len = GetVectorLen(hdr);
	    for (int i = 0;  i < len;  i++) {
		Value_t v = (Value_t)*globScan;
		if (isPtr(v)) {
		    if (inVPHeap(heapBase, ValueToAddr(v))) {
			*globScan = (Word_t)ForwardObj(v, &globNext);
		    }
		    globScan++;
		}
	    }
	}
	else {
	    assert (isRawHdr(hdr));
	  // we can just skip raw objects
	    globScan += GetRawSizeW(hdr);
	}
    }
    vp->globNextW = (Addr_t)globNext;
}
