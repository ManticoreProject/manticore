#ifndef _CHENEY_GC_H
#define _CHENEY_GC_H

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
#define WORD_SZ_B     (sizeof(Word_t))
#define HDR_ALIGN_MASK    7l
#define MIXED_LEN_BITS 6

enum { FORWARD=0, RAW=2, VECTOR=4 /*, MIXED=1,3,7 */};
typedef int Header_t;

Mant_t *to_space, *from_space;
Mant_t *high, *low, *base;

// extract the header word from an object
static inline Word_t hdr_word (Mant_t *m) {
  return m[-1];
}

static inline Header_t hdr_type (Mant_t *m) {
  return hdr_word (m) & HDR_ALIGN_MASK;
} 

// extract the length of an object
static /*inline*/ uint_t hdr_len (Mant_t *m) {
  switch (hdr_type (m)) {
  case FORWARD:
	exit(1);
	// chase down the forward pointer
	return hdr_len ((Mant_t*)*m);  
  case RAW:
  case VECTOR:
	exit (1);
	return hdr_word (m) >> 3;
  default:
	// MIXED
	return (hdr_word (m) >> 1) & 
	  ((1<<MIXED_LEN_BITS)-1);
  }
}

// is the ith element of m a pointer?
static inline Bool_t is_pointer (Mant_t *m, uint_t i) {
  Mant_t *mi = (Mant_t*)m[i];  
  return (1l & (hdr_word (m) >> (i + MIXED_LEN_BITS+1)))  && 
	( ((Word_t)mi & ALIGN_MASK) == (Word_t)base );  // test whether a pointer is in the heap
	/*( (mi < high) && (mi >= low) );*/
}

static inline Bool_t is_forwarded (Mant_t *m) {
  return hdr_type (m) == FORWARD;
}

static inline Mant_t *get_forward_ptr (Mant_t *m) {
  return (Mant_t*)hdr_word (m);
}

static inline void set_forward_ptr (Mant_t *m_fs, Mant_t *m_ts) {
  m_fs[-1] = (Mant_t)m_ts;
}

typedef struct {
  Mant_t *root;
  Mant_t *ap;
  Mant_t *ra;
} GC_info_t;

GC_info_t *init_gc (Mant_t *, Mant_t *, Mant_t *);

#endif
