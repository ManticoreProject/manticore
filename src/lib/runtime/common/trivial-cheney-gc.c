#include "trivial-cheney-gc.h"
#include "trivial-cheney-gc-info.c"

Mant_t *limit_ptr () {
  return from_space + (HEAP_SIZE_W-HEAP_SLOP);
}

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

	switch (hdr_type (scan)) {
	case FORWARD:
	  printf ("unexpected forward pointer\n");
	  exit (1);
	case RAW:
	case VECTOR:
	  break;
	default: 
	  // MIXED
	  for (uint_t i = 0; i < len; i++) {
		if (is_pointer (scan, i)) {
		  scan[i] = (Mant_t)forward (&next, (Mant_t*)scan[i]);
		}
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
  int reclaimed_space = ap-from_space;

  info.root = root; info.ap = ap; info.ra = ra;

  dump_heap (info.ap, from_space);

  do_gc (&info);
  
  Mant_t *temp = to_space;
  to_space   = from_space; 
  from_space = temp;

  dump_heap (info.ap, from_space);

  if (reclaimed_space <= (info.ap-from_space)) {
	printf ("failed to reclaim space, exiting\n");
	exit(1);
  }

  return &info;  /* this is safe because the caller assembly stub does not
		  * use the stack */
}
