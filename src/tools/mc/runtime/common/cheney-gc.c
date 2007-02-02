/* cheney-gc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Basic implementation of Cheney's algorithm (see ch16 of Compiling with
 * Continuations for details).
 *
 * Assumptions:
 *   -- there are only pointers to the first word of the data segment
 *   -- the bitmask identifies all pointers to trace in an object
 */

#include "cheney-gc.h"

void *forward (void ***next, void **data) {
  Object_t *obj = pointer_to_obj (data);
  void **heap = *next;

  if (is_forwarded (obj)) {
    return get_forward_ptr (obj);
  } else {
    uint_t len = hdr_len (obj) + 1; // add one extra word for the header
    for (int i = 0; i < len; i++) {
      heap[i] = data[i-1];
    }
    set_forward_ptr (obj, data);
    *next += len;
    return obj->data;
  }
}

void do_gc (Object_t *root_obj) {
  void **next = to_space;
  void **scan = next + 1;

  // forward the roots
  uint_t len = hdr_len (root_obj);
  void ***roots = &root_obj->data;
  for (uint_t i = 0; i < len; i++) {
    if (is_pointer (root_obj, i)) {
      roots[i] = forward (&next, roots[i]);
    }
  }
  // forward each live object
  while (scan < next) {
    Object_t *obj = pointer_to_obj (scan);
    uint_t len = hdr_len (obj);
    for (uint_t i = 0; i < len; i++) {
      if (is_pointer (obj, i)) {
		scan[i] = forward (&next, scan[i]);
      }
    }
    scan += len;
  }
}

void init_gc (void *root) {
  Object_t *root_obj = pointer_to_obj (root);

  do_gc (root_obj);

  // swap to- and from-space
  void *temp = to_space;
  to_space = from_space; from_space = temp;  
}
