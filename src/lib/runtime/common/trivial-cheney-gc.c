#include "trivial-cheney-gc.h"

#include "trivial-cheney-gc-info.c"

Mant_t *forward (Mant_t **next, Mant_t *data) {
  Mant_t *heap = *next;
  
  if (is_forwarded (data)) {
    return get_forward_ptr (data);
  } else {
    uint_t len = hdr_len (data) + 1;
    for (int i = 0; i < len; i++) {
      heap[i-1] = data[i-1];
    }
    set_forward_ptr (data, heap);
    *next += len;
    return heap;
  }
}

void do_gc (GC_info_t *info) {
  Mant_t *next = to_space;   // expect that &to_space == &heap[1]
  Mant_t *scan = next;
  
  Mant_t *root_ts = forward (&next, info->root);

  while (scan < next) {
    uint_t len = hdr_len (scan);
    for (uint_t i = 0; i < len; i++) {
      if (is_pointer (scan, i)) {
	scan[i] = (Mant_t)forward (&next, (Mant_t*)scan[i]);
      }
    }
    scan += len + 1;
  }
  // refresh heap information
  info->root = root_ts;
  info->ap = next;
}

GC_info_t *init_gc (Mant_t *ra, Mant_t *ap, Mant_t *root) {
  GC_info_t info;
  info.root = root; info.ap = ap; info.ra = ra;

  do_gc (&info);
  
  Mant_t *temp = to_space;
  to_space   = from_space; 
  from_space = temp;

  dump_heap (info.ap, from_space);

  //printf ("\nGC: %d\n", info.ap-from_space);
  return &info;  /* this is safe because the caller assembly stub does not
		  * use the stack */
}
