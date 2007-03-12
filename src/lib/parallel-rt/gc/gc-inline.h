/* gc-inline.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Inline operations for the GC.
 */

#ifndef _GC_INLINE_H_
#define _GC_INLINE_H_

#include "manticore-rt.h"
#include "heap.h"

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

STATIC_INLINE bool GetMixedBits (Word_t hdr)
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
    else
	return GetVectorLen (hdr);  /* assming RAW and VEC have same layout */
}

/* return true of the given address is within the given vproc heap */
STATIC_INLINE bool inVPHeap (Addr_t heapBase, Addr_t p)
{
    return (heapBase == (p & ~VP_HEAP_MASK));
}

#endif /* !_GC_INLINE_H_ */
