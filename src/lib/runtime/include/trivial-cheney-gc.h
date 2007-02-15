#ifndef _CHENEY_GC_H
#define _CHENEY_GC_H

#include "gc-defs.h"

/*
 * An object has simple a representation:
 * 
 *               header
 *  ----------------------------------           ...
 *  |  -- 7 bytes --  |  -- 1 byte --  |                     |
 *  |  pointer mask   |     length     |        data         |
 * 
 * Data are aligned on word boundaries. This representation of objects
 * allows for a max of 56 words in a data segment, much less than the 
 * 265 possible with the length byte.
 */

/* Use this constant to determine whether a pointer lies within the
 * heap.  If the heap is aligned on a 2^k boundary (2^(k+1) when
 * you account for both to- and from-space), the lower k+1 bits
 * are all zero, and the topmost bits are 1s.  Now heap membership
 * for ptr is simply ( heap_base == ( ptr & ALIGN_MASK ) ).
 */
#define ALIGN_MASK    (~((HEAP_ALIGN*2)-1l))

#define ALL_1S        (~0l)
#define BYTE_MASK     0xffl
#define BYTE_SZ_B     8l
#define WORD_SZ_B     (sizeof(Word_t))
#define PTR_MASK_SZ   ((WORD_SZ_B-1l)*BYTE_SZ_B)      // pointer mask bit length

Mant_t *to_space, *from_space;
Mant_t *high, *low, *base;

static inline Word_t hdr_word (Mant_t *m) {
  return m[-1];
}

static inline uint_t hdr_len (Mant_t *m) {
  return hdr_word (m) & BYTE_MASK;
}

static inline Bool_t is_pointer (Mant_t *m, uint_t i) {
  Mant_t *mi = (Mant_t*)m[i];  
  return (1l & (hdr_word (m) >> (i + BYTE_SZ_B)))  && 
	( ((Word_t)mi & ALIGN_MASK) == (Word_t)base );  // test whether a pointer is in the heap
	/*( (mi < high) && (mi >= low) );*/
}

/* Since LENGTH(obj) <= 56, we know that the length field never reaches
 * 0xFF.  I use this value to denote a forwarded object.
 */
static inline Bool_t is_forwarded (Mant_t *m) {
  return (hdr_word (m) & BYTE_MASK) == BYTE_MASK;
}

static inline Mant_t *get_forward_ptr (Mant_t *m) {
  return (Mant_t*)m[0];
}

static inline void set_forward_ptr (Mant_t *m_fs, Mant_t *m_ts) {
  m_fs[-1] = BYTE_MASK;    // mark the object as forwarded
  m_fs[0] = (Mant_t)m_ts;
}

typedef struct {
  Mant_t *root;
  Mant_t *ap;
  Mant_t *ra;
} GC_info_t;

GC_info_t *init_gc (Mant_t *, Mant_t *, Mant_t *);

#endif
