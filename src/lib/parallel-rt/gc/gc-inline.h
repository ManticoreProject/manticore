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

/* is a header tagged as a forward pointer? */
STATIC_INLINE bool isForwardPtr (Word_t hdr)
{
    return ((hdr & FWDPTR_TAG_MASK) == FWDPTR_TAG);
}

/* extract a forward pointer from a header */
STATIC_INLINE Word_t *GetForwardPtr (Word_t hdr)
{
    return (Word_t *)(Addr_t)hdr;
}

STATIC_INLINE Word_t MakeForwardPtr (Word_t hdr, Word_t *fp)
{
    return (Word_t)((Addr_t)fp | FWDPTR_TAG);
}

/* return true if the value is a pointer */
STATIC_INLINE bool isPtr (Value_t v)
{
    return (((Word_t)v & 0x3) == 0);
}

STATIC_INLINE bool isMixedHdr (Word_t hdr)
{
  /* NOTE: this code relies on the fact that the tag is one bit == 1 */
    return ((hdr & MIXED_TAG_MASK) /* == MIXED_TAG */);
}

STATIC_INLINE bool isVectorHdr (Word_t hdr)
{
    return ((hdr & VEC_TAG_MASK) == VEC_TAG);
}

STATIC_INLINE bool isRawHdr (Word_t hdr)
{
    return ((hdr & RAW_TAG_MASK) == RAW_TAG);
}

STATIC_INLINE int GetMixedSizeW (Word_t hdr)
{
    return (hdr >> MIXED_TAG_BITS) & ((1 << MIXED_LEN_BITS) - 1);
}

STATIC_INLINE Word_t GetMixedBits (Word_t hdr)
{
    return (hdr >> (MIXED_LEN_BITS+MIXED_TAG_BITS));
}

STATIC_INLINE int GetVectorLen (Word_t hdr)
{
    return (hdr >> VEC_TAG_BITS);
}

STATIC_INLINE int GetRawSizeW (Word_t hdr)
{
    return (hdr >> RAW_TAG_BITS);
}

/* Return the length field of a header */
STATIC_INLINE int GetLength (Word_t hdr)
{
    if (isMixedHdr(hdr))
	return GetMixedSizeW(hdr);
    else {
	assert (isRawHdr(hdr) || isVectorHdr(hdr));
	return GetVectorLen (hdr);  /* assuming RAW and VEC have same layout */
    }
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

#endif /* !_GC_INLINE_H_ */
