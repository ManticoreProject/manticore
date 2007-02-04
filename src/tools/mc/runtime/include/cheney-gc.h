#ifndef _CHENEY_GC_H
#define _CHENEY_GC_H

#include "manticore-types.h"
#include "gc-defs.h"

/*
 * An object has simple a representation:
 * 
 *               header
 *  ___________________________________         ...
 *  | -- 1 byte -- |   -- 7 bytes --  |  -- length bytes -- |
 *  |   length     |   pointer mask   |        data         |
 * 
 * Data are aligned on word boundaries. This representation of objects
 * allows for a max of 56 words in an object, much less than the 265
 * possible with the length byte.
 */

#define ALL_1S (~0l)
#define BYTE_MASK 0xffffl
#define WORD_SZ_B  (sizeof(Word_t))
#define LEN_MASK (~ (BYTE_MASK<<((WORD_SZ_B-1l)*8l)))

typedef struct {
  unsigned char mask[WORD_SZ_B-1];
  unsigned char length;
} Hdr_Bytes_t;

typedef struct {
  union {
    Word_t hdr_word;
    Hdr_Bytes_t hdr_bytes;
  } hdr;
  void **data; // Word_t w[1];
} Object_t;

/* pointers only point to the first element of an object's data segment */
static inline Object_t *pointer_to_obj (void **p) {
  return (Object_t*)(p-1);
}
static inline void *obj_to_pointer (Object_t *obj) {
  return &obj->data;
}

static inline uint_t hdr_len (Object_t *obj) {
  return (uint_t)obj->hdr.hdr_bytes.length;
}

static inline Bool_t is_pointer (Object_t *obj, uint_t i) {
  return 1l & ((LEN_MASK & obj->hdr.hdr_word) >> i);
}

/*static inline Bool_t is_forwarded (void *to_space, void *p) {
  return p >= to_space;
  }*/

static inline Bool_t is_forwarded (Object_t *obj) {
  /* Since LENGTH(obj) <= 56, we know that the length field never reaches
   * 0xFF; thus obj->hdr should never normally contain all 1s.  An object
   * that does, however, contain all 1s denotes a forwarded object.
   */
  return obj->hdr.hdr_word == ALL_1S;
}

static inline void *get_forward_ptr (Object_t *obj) {
  /* Store the forward pointer at the beginning of the data segment. */
  return obj->data;
}

static inline void set_forward_ptr (Object_t *obj, void *p) {
  obj->hdr.hdr_word = ALL_1S;
  obj->data = p;
}

// keep a global copy of to- and from-space locally
void **to_space, **from_space;

void init_gc (void *);

#endif
