/* minor-gc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Minor GCs are local collections of a vproc's allocation space.
 */

#include <strings.h>
#include <stdio.h>

#include "manticore-rt.h"
#include "heap.h"
#include "gc.h"
#include "vproc.h"
#include "value.h"
#include "internal-heap.h"
#include "gc-inline.h"
#include "inline-log.h"
#include "bibop.h"

extern Addr_t	MajorGCThreshold;	/* when the size of the nursery goes below */
					/* this limit it is time to do a GC. */

#ifndef NDEBUG
static void CheckMinorGC (VProc_t *self, Value_t **roots);
#endif

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
void MinorGC (VProc_t *vp)
{
    Addr_t	nurseryBase = vp->nurseryBase;
    Addr_t	allocSzB = vp->allocPtr - nurseryBase - WORD_SZB;
    Word_t	*nextScan = (Word_t *)(vp->oldTop); /* current top of to-space */
    Word_t	*nextW = nextScan + 1;		/* next object address in to-space */

    LogMinorGCStart (vp, (uint32_t)allocSzB);

#ifndef NO_GC_STATS
    TIMER_Start(&(vp->minorStats.timer));
#endif

    assert (vp->heapBase <= (Addr_t)nextScan);
    assert ((Addr_t)nextScan < vp->nurseryBase);
    assert (vp->nurseryBase < vp->allocPtr);

#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_MINOR)
	SayDebug("[%2d] Minor GC starting\n", vp->id);
#endif

  /* gather the roots.  The protocol is that the stdCont register holds
   * the return address (which is not in the heap) and that the stdEnvPtr
   * holds the GC root.
   */
    Value_t *roots[16], **rp;
    rp = roots;
    *rp++ = &(vp->currentFLS);
    *rp++ = &(vp->actionStk);
    *rp++ = &(vp->schedCont);
    *rp++ = &(vp->dummyK);
    *rp++ = &(vp->wakeupCont);
    *rp++ = &(vp->shutdownCont);
    *rp++ = &(vp->rdyQHd);
    *rp++ = &(vp->rdyQTl);
    *rp++ = &(vp->landingPad);
    *rp++ = &(vp->stdEnvPtr);
    *rp++ = 0;
    assert (rp <= roots+(sizeof(roots)/sizeof(Value_t *)));

#ifndef NDEBUG
  /* nullify non-live registers */
    vp->stdArg = M_UNIT;
    vp->stdExnCont = M_UNIT;
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
	    assert ((uint64_t)tagBits < (1l << (uint64_t)GetMixedSizeW(hdr)));
	    Value_t *scanP = (Value_t *)nextScan;
	    while (tagBits != 0) {
		if (tagBits & 0x1) {
		    Value_t v = *scanP;
		    if (isPtr(v)) {
			if (inAddrRange(nurseryBase, allocSzB, ValueToAddr(v))) {
			    *scanP = ForwardObj(v, &nextW);
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
		    if (inAddrRange(nurseryBase, allocSzB, ValueToAddr(v))) {
			*nextScan = (Word_t)ForwardObj(v, &nextW);
		    }
		}
	    }
	}
	else {
	  // we can just skip raw objects
	    assert (isRawHdr(hdr));
	    nextScan += GetRawSizeW(hdr);
	}
    }

    assert ((Addr_t)nextScan >= vp->heapBase);
    Addr_t avail = VP_HEAP_SZB - ((Addr_t)nextScan - vp->heapBase);
#ifndef NO_GC_STATS
    vp->nMinorGCs++;
    vp->minorStats.nBytesAlloc += vp->allocPtr - vp->nurseryBase - WORD_SZB;
    vp->minorStats.nBytesCollected = vp->minorStats.nBytesAlloc;
    vp->minorStats.nBytesCopied += (Addr_t)nextScan - vp->oldTop;
    vp->majorStats.nBytesAlloc += (Addr_t)nextScan - vp->oldTop;
    TIMER_Stop(&(vp->minorStats.timer));
#endif
#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_MINOR) {
bzero(nextScan, avail); /* clear unused part of local heap */
	SayDebug("[%2d] Minor GC finished: %ld/%ld bytes live; %d available\n",
	    vp->id, (Addr_t)nextScan - vp->oldTop,
	    vp->allocPtr - vp->nurseryBase - WORD_SZB,
	    (int)avail);
    }
#endif /* !NDEBUG */

    LogMinorGCEnd (vp, (uint32_t)((Addr_t)nextScan - vp->oldTop), (uint32_t)avail);

    if ((avail < MajorGCThreshold) || vp->globalGCPending) {
      /* time to do a major collection. */
	MajorGC (vp, roots, (Addr_t)nextScan);
    }
    else {
      /* remember information about the final state of the heap */
	vp->oldTop = (Addr_t)nextScan;
    }

#ifndef NDEBUG
    if (HeapCheck >= GC_DEBUG_MINOR) {
	CheckMinorGC (vp, roots);
    }
#endif

  /* reset the allocation pointer */
    SetAllocPtr (vp);

}

#ifndef NDEBUG
static void CheckLocalPtr (VProc_t *self, void *addr, const char *where)
{
    Value_t v = *(Value_t *)addr;
    if (isPtr(v)) {
	MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
	if (cq->sts == TO_SP_CHUNK)
	    return;
	else if (cq->sts == FROM_SP_CHUNK)
	    SayDebug("CheckLocalPtr: unexpected from-space pointer %p at %p in %s\n",
		ValueToPtr(v), addr, where);
	else if (IS_VPROC_CHUNK(cq->sts)) {
	    if (cq->sts != VPROC_CHUNK(self->id)) {
		SayDebug("CheckLocalPtr: unexpected remote pointer %p at %p in %s\n",
		    ValueToPtr(v), addr, where);
	    }
	    else if (! inAddrRange(self->heapBase, self->oldTop - self->heapBase, ValueToAddr(v))) {
		SayDebug("CheckLocalPtr: local pointer %p at %p in %s is out of bounds\n",
		    ValueToPtr(v), addr, where);
	    }
	}
	else if (cq->sts == FREE_CHUNK) {
	    SayDebug("CheckLocalPtr: unexpected free-space pointer %p at %p in %s\n",
		ValueToPtr(v), addr, where);
	}
    }
}

static void CheckMinorGC (VProc_t *self, Value_t **roots)
{

    char buf[32];

  // check the roots
    for (int i = 0;  roots[i] != 0;  i++) {
	sprintf(buf, "root[%d]", i);
	Value_t v = *roots[i];
	CheckLocalPtr (self, roots[i], buf);
    }

  // check the local heap
    {
	Word_t *top = (Word_t *)(self->oldTop);
	Word_t *p = (Word_t *)self->heapBase;
	while (p < top) {
	    Word_t hdr = *p++;
	    if (isMixedHdr(hdr)) {
	      // a record
		Word_t tagBits = GetMixedBits(hdr);
		Word_t *scanP = p;
		while (tagBits != 0) {
		    if (tagBits & 0x1) {
			CheckLocalPtr (self, scanP, "local mixed object");
		    }
		    else {
		      /* check for possible pointers in non-pointer fields */
			Value_t v = *(Value_t *)scanP;
			if (isHeapPtr(v)) {
			    MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
			    switch (cq->sts) {
			      case FREE_CHUNK:
				SayDebug(" ** possible free-space pointer %p in mixed object %p+%d\n",
				    (void *)v, (void *)p, (int)(scanP-p));
				break;
			      case TO_SP_CHUNK:
				SayDebug(" ** possible to-space pointer %p in mixed object %p+%d\n",
				    (void *)v, (void *)p, (int)(scanP-p));
				break;
			      case FROM_SP_CHUNK:
				SayDebug(" ** possible from-space pointer %p in mixed object %p+%d\n",
				    (void *)v, (void *)p, (int)(scanP-p));
				break;
			      case UNMAPPED_CHUNK:
				break;
			      default:
				if (IS_VPROC_CHUNK(cq->sts)) {
				  /* the vproc pointer is pretty common, so filter it out */
				    if ((Addr_t)v & ~VP_HEAP_MASK != (Addr_t)v)
					SayDebug(" ** possible local pointer %p in mixed object %p+%d\n",
					    (void *)v, (void *)p, (int)(scanP-p));
				}
				else {
				    SayDebug(" ** strange pointer %p in mixed object %p+%d\n",
					(void *)v, (void *)p, (int)(scanP-p));
				}
				break;
			    }
			}
		    }
		    tagBits >>= 1;
		    scanP++;
		}
		p += GetMixedSizeW(hdr);
	    }
	    else if (isVectorHdr(hdr)) {
	      // an array of pointers
		int len = GetVectorLen(hdr);
		for (int i = 0;  i < len;  i++, p++) {
		    sprintf(buf, "local vector[%d/%d]", i, len);
		    CheckLocalPtr (self, p, buf);
		}
	    }
	    else if (isForwardPtr(hdr)) {
	      // forward pointer
		Word_t *forwardPtr = GetForwardPtr(hdr);
		CheckLocalPtr(self, forwardPtr, "forward pointer");
		Word_t hdr = forwardPtr[-1];
		if (isMixedHdr(hdr)) {
		    p += GetMixedSizeW(hdr);
		}
		else if (isVectorHdr(hdr)) {
		    p += GetVectorLen(hdr);
		}
		else {
		    assert (isRawHdr(hdr));
		    p += GetRawSizeW(hdr);
		}
	    }
	    else {
		assert (isRawHdr(hdr));
		int len = GetRawSizeW(hdr);
	      // look for raw values that might be pointers
		for (int i = 0; i < len; i++) {
		    Value_t v = (Value_t)p[i];
		    if (isPtr(v)) {
		        if (isHeapPtr(v)) {
			  MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
			  if (cq->sts != TO_SP_CHUNK) {
			     if (cq->sts == FROM_SP_CHUNK)
			       SayDebug("** suspicious looking from-space pointer %p at %p[%d] in raw object of length %d (in local heap)\n",
					ValueToPtr(v), (void *)p, i, len);
			     else if (IS_VPROC_CHUNK(cq->sts))
			       SayDebug("** suspicious looking local pointer %p at %p[%d] in raw object of length %d (in local heap)\n",
					ValueToPtr(v), (void *)p, i, len);
			     else if (cq->sts == FREE_CHUNK)
			       SayDebug("** suspicious looking free pointer %p at %p[%d] in raw object of length %d (in local heap)\n",
					ValueToPtr(v), (void *)p, i, len);
			  }
			} 
		    }
		}
		p += len;
	    }
	}
    }

  // check the global heap "to space"
    MemChunk_t	*cp = self->globToSpHd;
    while (cp != (MemChunk_t *)0) {
	assert (cp->sts = TO_SP_CHUNK);
	Word_t *p = (Word_t *)(cp->baseAddr);
	Word_t *top = UsedTopOfChunk(self, cp);
	while (p < top) {
	    Word_t hdr = *p++;
	    if (isMixedHdr(hdr)) {
	      // a record
		Word_t tagBits = GetMixedBits(hdr);
		Word_t *scanP = p;
		while (tagBits != 0) {
		    if (tagBits & 0x1) {
			Value_t v = *(Value_t *)scanP;
			if (isPtr(v)) {
			    MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
			    if (cq->sts != TO_SP_CHUNK) {
				if (cq->sts == FROM_SP_CHUNK)
				    SayDebug("** unexpected from-space pointer %p at %p in mixed object\n",
					ValueToPtr(v), (void *)p);
				else if (IS_VPROC_CHUNK(cq->sts))
				    SayDebug("** unexpected local pointer %p at %p in mixed object\n",
					ValueToPtr(v), (void *)p);
				else if (cq->sts == FREE_CHUNK)
				    SayDebug("** unexpected free pointer %p at %p in mixed object\n",
					ValueToPtr(v), (void *)p);
			    }
			}
		    }
		    else {
		      /* check for possible pointers in non-pointer fields */
			Value_t v = *(Value_t *)scanP;
			if (isHeapPtr(v)) {
			    MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
			    switch (cq->sts) {
			      case FREE_CHUNK:
				SayDebug(" ** possible free-space pointer %p in mixed object %p+%d\n",
				    (void *)v, (void *)p, (int)(scanP-p));
				break;
			      case TO_SP_CHUNK:
				SayDebug(" ** possible to-space pointer %p in mixed object %p+%d\n",
				    (void *)v, (void *)p, (int)(scanP-p));
				break;
			      case FROM_SP_CHUNK:
				SayDebug(" ** possible from-space pointer %p in mixed object %p+%d\n",
				    (void *)v, (void *)p, (int)(scanP-p));
				break;
			      case UNMAPPED_CHUNK:
				break;
			      default:
				if (IS_VPROC_CHUNK(cq->sts)) {
				  /* the vproc pointer is pretty common, so filter it out */
				    if ((Addr_t)v & ~VP_HEAP_MASK != (Addr_t)v)
					SayDebug(" ** possible local pointer %p in mixed object %p+%d\n",
					    (void *)v, (void *)p, (int)(scanP-p));
				}
				else {
				    SayDebug(" ** strange pointer %p in mixed object %p+%d\n",
					(void *)v, (void *)p, (int)(scanP-p));
				}
				break;
			    }
			}
		    }
		    tagBits >>= 1;
		    scanP++;
		}
		p += GetMixedSizeW(hdr);
	    }
	    else if (isVectorHdr(hdr)) {
	      // an array of pointers
		int len = GetVectorLen(hdr);
		for (int i = 0;  i < len;  i++, p++) {
		    Value_t v = (Value_t)*p;
		    if (isPtr(v)) {
			MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
			if (cq->sts != TO_SP_CHUNK) {
			    if (cq->sts == FROM_SP_CHUNK)
				SayDebug("** unexpected from-space pointer %p at %p in vector\n",
				    ValueToPtr(v), (void *)p);
			    else if (IS_VPROC_CHUNK(cq->sts))
				SayDebug("** unexpected local pointer %p at %p in vector\n",
				    ValueToPtr(v), (void *)p);
			    else if (cq->sts == FREE_CHUNK)
				SayDebug("** unexpected free pointer %p at %p in vector\n",
				    ValueToPtr(v), (void *)p);
			}
		    }
		}
	    }
	    else {
		assert (isRawHdr(hdr));
		int len = GetRawSizeW(hdr);
	      // look for raw values that might be pointers
		for (int i = 0; i < len; i++) {
		    Value_t v = (Value_t)p[i];
		    if (isPtr(v)) {
		        if (isHeapPtr(v)) {
			  MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
			  if (cq->sts != TO_SP_CHUNK) {
			     if (cq->sts == FROM_SP_CHUNK)
			       SayDebug("** suspicious looking from-space pointer %p at %p[%d] in raw object of length %d\n",
					ValueToPtr(v), (void *)p, i, len);
			     else if (IS_VPROC_CHUNK(cq->sts))
			       SayDebug("** suspicious looking local pointer %p at %p[%d] in raw object of length %d\n",
					ValueToPtr(v), (void *)p, i, len);
			     else if (cq->sts == FREE_CHUNK)
			       SayDebug("** suspicious looking free pointer %p at %p[%d] in raw object of length %d\n",
					ValueToPtr(v), (void *)p, i, len);
			  }
			} 
		    }
		}
		p += len;
	    }
	}
	cp = cp->next;
    }

}
#endif /* NDEBUG */
