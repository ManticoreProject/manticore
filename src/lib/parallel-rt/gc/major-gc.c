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
 *	check for global GC.  How?
 */

#include "manticore-rt.h"
#include <string.h>
#include "heap.h"
#include "gc.h"
#include "vproc.h"
#include "gc-inline.h"
#include "internal-heap.h"

static void ScanGlobalToSpace (
	VProc_t *vp, Addr_t heapBase, MemChunk_t *scanChunk, Word_t *scanPtr);
static void GetGlobalChunk (VProc_t *vp);

/* Forward an object into the global-heap chunk reserved for the current VP */
STATIC_INLINE Value_t ForwardObj (VProc_t *vp, Value_t v)
{
    Word_t	*p = ((Word_t *)ValueToPtr(v));
    Word_t	hdr = p[-1];
    if (isForwardPtr(hdr))
	return PtrToValue(GetForwardPtr(hdr));
    else {
	Word_t *nextW = (Word_t *)vp->globNextW;
	int len = GetLength(hdr);
	if (nextW+len >= (Word_t *)(vp->globLimit)) {
	    GetGlobalChunk (vp);
	    nextW = (Word_t *)vp->globNextW;
	}
	Word_t *newObj = nextW;
	newObj[-1] = hdr;
	for (int i = 0;  i < len;  i++) {
	    newObj[i] = p[i];
	}
	vp->globNextW = (Addr_t)(newObj+len+1);
	p[-1] = MakeForwardPtr(hdr, newObj);
	return PtrToValue(newObj);
    }

}

/*! \brief Perform a major collection on a vproc's local heap.
 *  \param vp the vproc that is performing the collection.
 *  \param roots a null-terminated array of root addresses.
 *  \param top the address of the top of the old region in
 *         the local heap.
 */
void MajorGC (VProc_t *vp, Value_t **roots, Addr_t top)
{
    Addr_t	heapBase = (Addr_t)vp;
    Addr_t	oldBase = VProcHeap(vp);
    Addr_t	oldSzB = vp->oldTop - oldBase;
    Word_t	*globScan = (Word_t *)(vp->globNextW - WORD_SZB);
    MemChunk_t	*scanChunk = vp->globToSpace;

    assert (VProcHeap(vp) < vp->oldTop);
    assert (vp->oldTop < top);

#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] Major GC starting\n",vp->id);
#endif

  /* process the roots */
    for (int i = 0;  roots[i] != 0;  i++) {
	Value_t p = *roots[i];
	if (isPtr(p)) {
	    if (inAddrRange(oldBase, oldSzB, ValueToAddr(p))) {
		*roots[i] = ForwardObj(vp, p);
	    }
	    else if (inVPHeap(heapBase, ValueToAddr(p))) {
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
		    if (isPtr(p)) {
			if (inAddrRange(oldBase, oldSzB, ValueToAddr(p))) {
			    *scanP = (Word_t)ForwardObj(vp, p);
			}
			else if (inVPHeap(heapBase, ValueToAddr(p))) {
			  // p points to another object in the "young" region,
			  // so adjust it.
			    *scanP = (Word_t)AddrToValue(ValueToAddr(p) - oldSzB);
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
		Value_t v = *(Value_t*)nextScan;
		if (isPtr(v)) {
		    if (inAddrRange(oldBase, oldSzB, ValueToAddr(v))) {
			*nextScan = (Word_t)ForwardObj(vp, v);
		    }
		    else if (inVPHeap(heapBase, (Addr_t)v)) {
		      // p points to another object in the "young" region,
		      // so adjust it.
			*nextScan = (Word_t)((Addr_t)v - oldSzB);
		    }
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
    ScanGlobalToSpace (vp, heapBase, scanChunk, globScan);

#ifndef NDEBUG
    if (DebugFlg) {
	unsigned long nBytesCopied;
	if (vp->globToSpace == scanChunk)
	    nBytesCopied = (unsigned long)(vp->globNextW - (Addr_t)globScan - WORD_SZB);
	else
	    nBytesCopied = -1;  /* FIXME */
	SayDebug("[%2d] Major GC finished: %ld/%ld bytes live\n",
	    vp->id, nBytesCopied, oldSzB);
    }
#endif

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
    Addr_t	heapBase = (Addr_t)vp;
    MemChunk_t	*scanChunk = vp->globToSpace;

    assert ((vp->globNextW % WORD_SZB) == 0);

#ifndef NDEBUG
    /*    if (DebugFlg)
	  SayDebug("[%2d] PromoteObj(%p, %p)\n", vp->id, vp, root); */
#endif
  /* NOTE: the following test probably ought to happen before the runtime
   * system gets called.
   */
    if (isPtr(root) && inVPHeap(heapBase, ValueToAddr(root))) {
      /* promote the root to the global heap */
	root = ForwardObj (vp, root);
#ifndef NDEBUG
	/*    if (DebugFlg)
	      SayDebug("[%2d]  ==> %p\n", vp->id, root); */
#endif

      /* promote any reachable values */
	ScanGlobalToSpace (vp, heapBase, scanChunk, (Word_t *)(vp->globNextW));
    }

    return root;

}

/* Scan the objects that have been copied to the global heap */
static void ScanGlobalToSpace (
    VProc_t *vp,
    Addr_t heapBase,
    MemChunk_t *scanChunk,
    Word_t *scanPtr)
{
    Word_t	*scanTop;

    if (vp->globToSpace == scanChunk)
	scanTop = (Word_t *)(vp->globNextW - WORD_SZB);
    else
	scanTop = (Word_t *)(scanChunk->usedTop);

    do {
	while (scanPtr < scanTop) {
	    Word_t hdr = *scanPtr++;	// get object header
	    if (isMixedHdr(hdr)) {
	      // a record
		Word_t tagBits = GetMixedBits(hdr);
		Word_t *scanP = scanPtr;
		while (tagBits != 0) {
		    if (tagBits & 0x1) {
			Value_t p = *(Value_t *)scanP;
			if (isPtr(p) && inVPHeap(heapBase, ValueToAddr(p))) {
			    *scanP = (Word_t)ForwardObj(vp, p);
			}
		    }
		    tagBits >>= 1;
		    scanP++;
		}
		scanPtr += GetMixedSizeW(hdr);
	    }
	    else if (isVectorHdr(hdr)) {
	      // an array of pointers
		int len = GetVectorLen(hdr);
		for (int i = 0;  i < len;  i++, scanPtr++) {
		    Value_t v = (Value_t)*scanPtr;
		    if (isPtr(v) && inVPHeap(heapBase, ValueToAddr(v))) {
			*scanPtr = (Word_t)ForwardObj(vp, v);
		    }
		}
	    }
	    else {
		assert (isRawHdr(hdr));
	      // we can just skip raw objects
		scanPtr += GetRawSizeW(hdr);
	    }
	}

      /* recompute the scan top, switching chunks if necessary */
	if (vp->globToSpace == scanChunk)
	    scanTop = (Word_t *)(vp->globNextW - WORD_SZB);
	else if (scanPtr == (Word_t *)scanChunk->usedTop) {
	    scanChunk = vp->globToSpace;
	    assert ((scanChunk->baseAddr < vp->globNextW)
		&& (vp->globNextW < scanChunk->baseAddr+scanChunk->szB));
	    scanTop = (Word_t *)(vp->globNextW - WORD_SZB);
	    scanPtr = (Word_t *)(scanChunk->baseAddr);
	}
	else
	    scanTop = (Word_t *)(scanChunk->usedTop);

    } while (scanPtr < scanTop);

}

/* GetGlobalChunk:
 */
static void GetGlobalChunk (VProc_t *vp)
{
    Die ("GetGlobalChunk unimplemented\n");
}
