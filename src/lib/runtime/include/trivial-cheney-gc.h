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

#define ALL_1S        (~0l)
#define BYTE_MASK     0xffl
#define BYTE_SZ_B     8l
#define WORD_SZ_B     (sizeof(Word_t))
#define PTR_MASK_SZ   ((WORD_SZ_B-1l)*BYTE_SZ_B)      // pointer mask bit length

Mant_t *to_space, *from_space;

static inline Word_t hdr_word (Mant_t *m) {
  return m[-1];
}

static inline uint_t hdr_len (Mant_t *m) {
  return hdr_word (m) & BYTE_MASK;
}

static inline Bool_t is_pointer (Mant_t *m, uint_t i) {
  Mant_t *mi = (Mant_t*)m[i];  
  return (1l & (hdr_word (m) >> (i + BYTE_SZ_B))) &&
	( (mi < to_space && mi >= from_space) || (mi < from_space && mi >= to_space) );
	/*m[i] != 0l;*/
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
