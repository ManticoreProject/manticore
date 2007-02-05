#include "trivial-cheney-gc.h"

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

Mant_t *do_gc (Mant_t *root_fs) {
  Mant_t *next = to_space;   // expect that &to_space == &heap[1]
  Mant_t *scan = next;

  Mant_t *root_ts = forward (&next, root_fs);

  while (scan < next) {
	uint_t len = hdr_len (scan);
	for (uint_t i = 0; i < len; i++) {
	  if (is_pointer (scan, i)) {
		scan[i] = (Mant_t)forward (&next, (Mant_t*)scan[i]);
	  }
	}
	scan += len + 1;
  }
  return root_ts;
}

Mant_t *init_gc (Mant_t *root_fs) {
  Mant_t *root_ts = do_gc (root_fs);
  
  Mant_t *temp;
  to_space = from_space; from_space = temp;

  return root_ts;
}
