#ifndef _CHENEY_GC_H
#define _CHENEY_GC_H

#include <stdio.h>
#include <stdlib.h>
#include "manticore-rt.h"
#include "../../parallel-rt/include/header-bits.h"
#include "../../parallel-rt/include/vproc.h"
#include "../../parallel-rt/include/manticore-rt.h"
#include "gc-defs.h"

/* The GC has mixed-type objects (containing both pointer and
 * raw values), raw objects (containing only raw data like floats
 * and ints), and vectors (containing the rope representation of
 * data-parallel arrays).  Each of these types of objects has 
 * a distinct header configuration.  The rightmost header bits rely
 * on an 8-byte alignment for all pointers.
 * 
 *  Mixed-Type Object:
 *
 *                     header              
 *  ----------------------------------------------    ...
 *  | -- 56 bits -- | -- 6-bits -- | -- 1 bit -- |            |
 *  | pointer mask  |     length   |      1      |   data     |
 *
 *
 *  Raw:
 *
 *                     header              
 *  ----------------------------------------------    ...
 *  |               -- 61 bits --          |                  |
 *  |                  length              | 010 |   data     |
 *
 *  Vector:
 *
 *                     header              
 *  ----------------------------------------------    ...
 *  |               -- 61 bits --          |                  |
 *  |                  length              | 100 |   data     |
 * 
 * The GC also has a simple header for forward pointers:
 *
 *                     header              
 *  ----------------------------------------------    ...
 *  |               -- 61 bits --          |                  |
 *  |              forward pointer         | 000 |   data     |
 */

/* Use ALIGN_MASK constant to determine whether a pointer lies within
 * the heap.  If the heap is aligned on a 2^k boundary (2^(k+1) when
 * you account for both to- and from-space), the lower k+1 bits
 * are all zero, and the topmost bits are 1s.  Now heap membership
 * for ptr is simply ( heap_base == ( ptr & ALIGN_MASK ) ).
 */
#define ALIGN_MASK    (~((HEAP_ALIGN*2)-1l))

#define ALL_1S        (~0l)
#define BYTE_MASK     0xffl
#define HDR_ALIGN_MASK    7l
#define MIXED_LEN_MASK   ((1<<MIXED_LEN_BITS)-1)
#define MIXED_TY_MASK   1l
// number of slop words 
#define HEAP_SLOP      (1<<(12-3))

typedef Word_t Header_t;

#define TO_SPACE(vp) (vp->nurseryBase)
#define FROM_SPACE(vp) vp->oldTop
#define VP_HEAP_BASE(vp) vp->globLimit

// extract the header word from an object
STATIC_INLINE Word_t hdr_word (Mant_t *m) {
  return m[-1];
}

STATIC_INLINE Header_t hdr_type (Mant_t *m) {
  return hdr_word (m) & HDR_ALIGN_MASK;
} 

// extract the length of an object
STATIC_INLINE uint_t hdr_len (Mant_t *m) {
  switch (hdr_type (m)) {
  case FWDPTR_TAG:
	// chase down the forward pointer
	return hdr_len ((Mant_t*)*m);  
  case RAW_TAG:
	return hdr_word (m) >> RAW_TAG_BITS;
  case VEC_TAG:
	return hdr_word (m) >> VEC_TAG_BITS;
  default:
	// MIXED
	return (hdr_word (m) >> MIXED_TAG_BITS) & MIXED_LEN_MASK;
  }
}

STATIC_INLINE Bool_t is_mixed (Mant_t *m) {
  return (hdr_word (m) & MIXED_TAG_MASK) == MIXED_TAG;
}

STATIC_INLINE Bool_t in_heap (Mant_t *m) {
  return ((Word_t)m & ALIGN_MASK) == (Word_t)VP_HEAP_BASE(VProcSelf ());
}

STATIC_INLINE Bool_t is_ptr (Mant_t *m) {
  return (((Word_t)m & 0x3) == 0);
}

// is the ith element of m a pointer?
STATIC_INLINE Bool_t is_pointer (Mant_t *m, uint_t i) {
  Mant_t *mi = (Mant_t*)m[i];  
  return 
	is_mixed (m) &&
	(MIXED_TY_MASK & (hdr_word (m) >> (i + MIXED_LEN_BITS+1)))  && 
	// test whether a pointer is in the heap
	in_heap (mi);
	/*	( ((Word_t)mi & ALIGN_MASK) == (Word_t)base );  */
	/*( (mi < high) && (mi >= low) );*/
}

STATIC_INLINE Bool_t is_forwarded (Mant_t *m) {
  return hdr_type (m) == FWDPTR_TAG;
}

STATIC_INLINE Mant_t *get_forward_ptr (Mant_t *m) {
  return (Mant_t*)hdr_word (m);
}

STATIC_INLINE void set_forward_ptr (Mant_t *m_fs, Mant_t *m_ts) {
  m_fs[-1] = (Mant_t)m_ts;
}

STATIC_INLINE void set_to_space (VProc_t *vp, Addr_t addr) {
  vp->nurseryBase = addr;
}
STATIC_INLINE void set_from_space (VProc_t *vp, Addr_t addr) {
  vp->oldTop = addr;
}

STATIC_INLINE void swap_space (VProc_t *vp) {
  Addr_t temp;
  temp = TO_SPACE(vp);
  set_to_space (vp, FROM_SPACE(vp));
  set_from_space (vp, temp);
}

STATIC_INLINE void set_limit_ptr (VProc_t *vp) {
  vp->limitPtr = FROM_SPACE(vp) + (HEAP_SIZE_W-HEAP_SLOP);
}

#endif
