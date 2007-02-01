#ifndef _CHENEY_GC_H
#define _CHENEY_GC_H

#include "context.h"
#include "manticore-types.h"

/*
 * Objects have a simple storage format:
 * 
 *  | -- 1 byte -- |   -- 7 bytes --  |  length bytes |
 *  |   length     |   pointer mask   |     data      |
 * 
 * Data is aligned on 8-byte boundaries.
 * 
 * This representation allows for a max of 56 words in an object, much more
 * than the 265 possible with the length byte.
 */

typedef struct {
  unsigned char length;
  unsigned char mask[7];
} Hdr_Bytes_t;

typedef struct {
  union {
    Word_t hdr_word;
    Hdr_Bytes_t hdr_bytes;
  } hdr;
  void *data;
} Object_t;

/* pointers only point to the first element of an object's data segment */
static inline Object_t pointer_to_obj (void *p) {
  return (Object_t*)(p-1);
}

static inline uint_t hdr_len (Object_t *obj) {
  return (uint_t)obj->hdr.hdr_bytes.len;
}

static inline Bool_t is_pointer (Object_t *obj, uint_t i) {

}

static inline Bool_t is_forwarded (void *to_space, void *p) {
  return p >= to_space;
}

#define ALL_1s (~0l)

#endif
