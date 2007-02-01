#include "cheney-gc.h"

/* 
 * forward (p):
 *   if is_to_space (p[0]) { // p is a forwarding pointer
 *      return p[0];
 *   } else {
 *     for each i in the object that p points to
 *       to-space[next+i] := p[i]
 *     set_forward_pointer (p, next+WORD_SIZE);
 *     next += sizeof(p);
 *     return p;
 *   } 
 */

Bool_t is_forwarded (Object_t *obj) {
  /* Since LENGTH(obj) <= 56, we know that the length field never reaches
   * 0xFF; thus obj->hdr should never normally contain all 1s.  An object
   * that does, however, contain all 1s denotes a forwarded object.
   */
  return obj->hdr == ALL_1S;
}

void *get_forward_ptr (Object_t *obj) {
  return obj->data[0];  /* Store the forward pointer at the beginning of the 
			 * data segment. */
}

void set_forward_ptr (Object_t *obj, void *p) {
  obj->hdr = ALL_1S;
  obj->data[0] = p;
}

void *forward (void *to_space, void **next, void *p) {
  Object_t *obj = pointer_to_obj(p);

  if (is_forwarded (to_space, obj)) {
    return get_forward_ptr (obj);
  } else {
    uint_t len = hdr_len (obj) + 1; // one extra word for the header

    for (uint_t i = 0; i < len; i++) {
      *next[i] = p[i]
    }
    set_forward_ptr (obj, *next);
    *next += len;
    return obj->data[0];
  }
}

void do_gc (void *to_space, void *from_space, Context_t *ctx) {
  Object_t *obj = pointer_to_obj (ctx); // the context is heap allocated
  void *next = to_space;
  void *scan = next;

  // forward the roots
  for (int i = 0; i < NUM_GPRS; i++) {
    if (ctx->gprs[i] != NULL) {
      ctx->gprs[i] = forward (to_space, &next, ctx->gprs[i]);
    }
  }

  while (scan < next) {
    Object_t *obj = pointer_to_obj (scan);
    uint_t len = hdr_len (obj);
    for (uint_t i = 0; i < len; i++) {
      if (is_pointer (obj, i)) {
	scan[i] = forward (to_space, &next, scan[i]);
      }
    }
    scan += len;
  }
}

void init_gc (Context_t *ctx) {
  
}
