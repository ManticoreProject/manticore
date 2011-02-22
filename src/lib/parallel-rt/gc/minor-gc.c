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
#include "work-stealing-deque.h"
#include "bibop.h"
#include "gc-scan.h"

extern Addr_t	MajorGCThreshold;	/* when the size of the nursery goes below */
					/* this limit it is time to do a GC. */

//ForwardObject of MinorGC
/* Copy an object to the old region */
Value_t ForwardObjMinor (Value_t v, Word_t **nextW)
{
	Word_t	*p = (Word_t *)ValueToPtr(v);
	Word_t	hdr = p[-1];
	
	if (isForwardPtr(hdr)) {
		return PtrToValue(GetForwardPtr(hdr));
	}
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

#ifndef NDEBUG
static void CheckMinorGC (VProc_t *self, Value_t **roots);
#endif

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
    int nWorkStealingRoots = M_NumDequeRoots (vp);
    Value_t *roots[16 + nWorkStealingRoots], **rp;
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
    rp = M_AddDequeEltsToLocalRoots(vp, rp);
    *rp++ = 0;

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
		*roots[i] = ForwardObjMinor(p, &nextW);
	    }
	}
    }
	
   /* process the proxy table */
    for (int i=0; i < vp->proxyTableentries;i++) {
		Value_t p = vp->proxyTable[i].localObj;
		if (inAddrRange(nurseryBase, allocSzB, ValueToAddr(p))) {
			vp->proxyTable[i].localObj = ForwardObjMinor(p, &nextW);
		}
    }
	
  /* scan to space */
    while (nextScan < nextW-1) {
		assert ((Addr_t)(nextW-1) <= vp->nurseryBase);
		
		Word_t hdr = *nextScan++;	// get object header
		
		if (isVectorHdr(hdr)) {
			//Word_t *nextScan = ptr;
			int len = GetLength(hdr);
			for (int i = 0;  i < len;  i++, nextScan++) {
				Value_t *scanP = (Value_t *)nextScan;
				Value_t v = *scanP;
				if (isPtr(v)) {
					if (inAddrRange(nurseryBase, allocSzB, ValueToAddr(v))) {
						*scanP = ForwardObjMinor(v, &nextW);
					}
				}
			}
			
			
		}else if (isRawHdr(hdr)) {
			assert (isRawHdr(hdr));
			nextScan += GetLength(hdr);
		}else {
			nextScan = table[getID(hdr)].minorGCscanfunction(nextScan,&nextW, allocSzB,nurseryBase);
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
void CheckLocalPtrMinor (VProc_t *self, void *addr, const char *where)
{
    Value_t v = *(Value_t *)addr;
    if (isPtr(v)) {
	MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
	if (cq->sts == TO_SP_CHUNK)
	    return;
	else if (cq->sts == FROM_SP_CHUNK)
	    SayDebug("CheckLocalPtrMinor: unexpected from-space pointer %p at %p in %s\n",
		ValueToPtr(v), addr, where);
	else if (IS_VPROC_CHUNK(cq->sts)) {
	    if (cq->sts != VPROC_CHUNK(self->id)) {
		SayDebug("CheckLocalPtrMinor: unexpected remote pointer %p at %p in %s\n",
		    ValueToPtr(v), addr, where);
	    }
	    else if (! inAddrRange(self->heapBase, self->oldTop - self->heapBase, ValueToAddr(v))) {
		SayDebug("CheckLocalPtrMinor: local pointer %p at %p in %s is out of bounds\n",
		    ValueToPtr(v), addr, where);
	    }
	}
	else if (cq->sts == FREE_CHUNK) {
	    SayDebug("CheckLocalPtrMinor: unexpected free-space pointer %p at %p in %s\n",
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
	CheckLocalPtrMinor (self, roots[i], buf);
    }

  // check the local heap
    {
	Word_t *top = (Word_t *)(self->oldTop);
	Word_t *p = (Word_t *)self->heapBase;
	while (p < top) {
		
	    Word_t hdr = *p++;
	    Word_t *scanptr = p;
		
	    if (isForwardPtr(hdr)) {
	      // forward pointer
			Word_t *forwardPtr = GetForwardPtr(hdr);
			CheckLocalPtrMinor(self, forwardPtr, "forward pointer");
			Word_t hdr = forwardPtr[-1];
			
			p += GetLength(hdr);
	    }
	    else {
			
			tableDebug[getID(hdr)].minorGCdebug(self,scanptr);
			p += GetLength(hdr);
	    }
	}
    }

  // check the global heap allocation space
    MemChunk_t	*cp = self->globAllocChunk;
	assert (cp->sts = TO_SP_CHUNK);
	Word_t *p = (Word_t *)(cp->baseAddr);
	Word_t *top = UsedTopOfChunk(self, cp);
	while (p < top) {
	    Word_t hdr = *p++;
	    Word_t *scanptr = p;
		
		tableDebug[getID(hdr)].minorGCdebugGlobal(self,scanptr);
		p += GetLength(hdr);
	}
}
#endif /* NDEBUG */
