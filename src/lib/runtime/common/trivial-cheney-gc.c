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

Addr_t do_gc (VProc_t *vp, Value_t **roots) {
  Mant_t *next = TO_SPACE(vp);   // expect that to_space == &nursery[1]
  Mant_t *scan = next;
  
  for (int i = 0; roots[i] != 0l; i++) {
	Mant_t *p = *roots[i];
	if (is_ptr (p)) {
	  if (in_heap (p)) {
		*roots[i] = (Value_t)forward (&next, p);
	  }
	}
  }
  
  while (scan < next) {
    uint_t len = hdr_len (scan);

	switch (hdr_type (scan)) {
	case FWDPTR_TAG:
	  Die ("unexpected forward pointer");
	case RAW_TAG:
	case VEC_TAG:
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

  return (Addr_t)next;
}

void MinorGC (VProc_t *vp, Value_t **roots) {
  Word_t initWords =  (Word_t)(vp->allocPtr-vp->oldTop);
  vp->allocPtr = do_gc (vp, roots);
  // swap to- and from-space
  swap_space (vp);
  set_limit_ptr (vp);
  numGCs++;
  Word_t heapSz = (Word_t)(vp->allocPtr-vp->oldTop);
  printf ("heap size: %ld\tbytes collected: %ld\n", 
		  8*heapSz, 8*(initWords-heapSz));
}
