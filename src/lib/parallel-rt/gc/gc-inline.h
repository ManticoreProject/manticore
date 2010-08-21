/* gc-inline.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Inline operations for the GC.  See ../include/header-bits.h for header layout.
 */

#ifndef _GC_INLINE_H_
#define _GC_INLINE_H_

#include "manticore-rt.h"
#include "header-bits.h"
#include "heap.h"
#include "bibop.h"
#include "internal-heap.h"
#include "gc-scan.h"
#include "vproc.h"

/*! \for new header structure */
STATIC_INLINE int getID (Word_t hdr)
{
    return ((hdr >> 1) & 0x3FFF);
}

/*! \brief is a header tagged as a forward pointer? */
STATIC_INLINE bool isForwardPtr (Word_t hdr)
{
    return ((hdr & FWDPTR_TAG_MASK) == FWDPTR_TAG);
}

/*! \brief extract a forward pointer from a header */
STATIC_INLINE Word_t *GetForwardPtr (Word_t hdr)
{
    return (Word_t *)(Addr_t)hdr;
}

STATIC_INLINE Word_t MakeForwardPtr (Word_t hdr, Word_t *fp)
{
    return (Word_t)((Addr_t)fp | FWDPTR_TAG);
}

/*! \brief return true if the value might be a pointer */
STATIC_INLINE bool isPtr (Value_t v)
{
    return (((Word_t)v & 0x3) == 0);
}

/*! \brief return true if the value is a pointer and is in the range covered
 * by the BIBOP.
 */
STATIC_INLINE bool isHeapPtr (Value_t v)
{
    return ((((Word_t)v & 0x3) == 0) && ((Addr_t)v < (1l << ADDR_BITS)));
}


STATIC_INLINE bool isMixedHdr (Word_t hdr)
{
   //NOTE: this code relies on the fact that the tag is one bit == 1 
    return ( (getID(hdr) != 0) && (getID(hdr) != 1) );
}

STATIC_INLINE bool isVectorHdr (Word_t hdr)
{
    return (getID(hdr) == 1);
}

STATIC_INLINE bool isRawHdr (Word_t hdr)
{
    return (getID(hdr) == 0);
}

/* Return the length field of a header */
STATIC_INLINE int GetLength (Word_t hdr)
{
   return (hdr >> 16);
}

/* return true if the given address is within the given address range */
STATIC_INLINE bool inAddrRange (Addr_t base, Addr_t szB, Addr_t p)
{
    return ((p - base) < szB);
}

/*! \brief return the top of the used space in a memory chunk.
 *  \param vp the vproc that owns the chunk.
 *  \param cp the memory chunk.
 */
STATIC_INLINE Word_t *UsedTopOfChunk (VProc_t *vp, MemChunk_t *cp)
{
    if (vp->globToSpTl == cp)
      /* NOTE: we must subtract WORD_SZB here because globNextW points to the first
       * data word of the next object (not the header word)!
       */
	return (Word_t *)(vp->globNextW - WORD_SZB);
    else
	return (Word_t *)(cp->usedTop);
}


//ForwardObject of MinorGC
/* Copy an object to the old region */
STATIC_INLINE Value_t ForwardObjMinor (Value_t v, Word_t **nextW)
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

//ForwardObject of MajorGC
/*! \brief Forward an object into the global-heap chunk reserved for the given vp.
 *  \param vp the vproc
 *  \param v  the heap object that is to be forwarded
 *  \return the forwarded value
 */
STATIC_INLINE Value_t ForwardObjMajor (VProc_t *vp, Value_t v)
{
    Word_t	*p = ((Word_t *)ValueToPtr(v));
    Word_t	hdr = p[-1];
    if (isForwardPtr(hdr))
		return PtrToValue(GetForwardPtr(hdr));
    else {
		/* forward object to global heap. */
		Word_t *nextW = (Word_t *)vp->globNextW;
		int len = GetLength(hdr);
		if (nextW+len >= (Word_t *)(vp->globLimit)) {
			AllocToSpaceChunk (vp);
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

//ForwardObject and isFromSpacePtr of GlobalGC
STATIC_INLINE bool isFromSpacePtr (Value_t p)
{
    return (isPtr(p) && (AddrToChunk(ValueToAddr(p))->sts == FROM_SP_CHUNK));
	
}

/* Forward an object into the global-heap chunk reserved for the current VP */
STATIC_INLINE Value_t ForwardObjGlobal (VProc_t *vp, Value_t v)
{
    Word_t	*p = ((Word_t *)ValueToPtr(v));
    Word_t	oldHdr = p[-1];
    if (isForwardPtr(oldHdr)) {
		Value_t v = PtrToValue(GetForwardPtr(oldHdr));
		assert (isPtr(v) && (AddrToChunk(ValueToAddr(v))->sts == TO_SP_CHUNK));
		return v;
    }
    else {
		// we need to atomically update the header to a forward pointer, so frst
		// we allocate space for the object and then we try to install the forward
		// pointer.
		Word_t *nextW = (Word_t *)vp->globNextW;
		int len = GetLength(oldHdr);
		if (nextW+len >= (Word_t *)(vp->globLimit)) {
			AllocToSpaceChunk (vp);
			nextW = (Word_t *)vp->globNextW;
		}
		// try to install the forward pointer
		Word_t fwdPtr = MakeForwardPtr(oldHdr, nextW);
		Word_t hdr = CompareAndSwapWord(p-1, oldHdr, fwdPtr);
		if (oldHdr == hdr) {
			Word_t *newObj = nextW;
			newObj[-1] = hdr;
			for (int i = 0;  i < len;  i++) {
				newObj[i] = p[i];
			}
			vp->globNextW = (Addr_t)(newObj+len+1);
			return PtrToValue(newObj);
		}
		else {
			// some other vproc forwarded the object, so return the forwarded
			// object.
			assert (isForwardPtr(hdr));
			return PtrToValue(GetForwardPtr(hdr));
		}
    }
	
}




#endif /* !_GC_INLINE_H_ */
