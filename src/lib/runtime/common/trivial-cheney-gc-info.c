#include <stdio.h>

char *get_hdr_type (Mant_t *m) {
  switch (hdr_type (m)) {
  case FORWARD: 
	return "forward";
  case RAW: 
	return "raw";
  case VECTOR: 
	return "vector";
  default: 
	return "mixed";
  }
}

void print_obj (Mant_t *heap, uint_t *i) {
  Mant_t *obj = heap + *i;
  uint_t len = hdr_len (obj);
  printf ("\t- object 0x%lX #%d len=%d type=%s-\n", 
		  obj, *i, len, get_hdr_type (obj));
  for (uint_t j = 0; j < len; j++) {
	if (is_pointer (obj, j)) {
	  printf ("\t\t #%d 0x%lX \n", j, obj[j]);
	} else {
	  printf ("\t\t #%d %ld 0x%lX \n", j, obj[j], obj[j]);
	}
  } 
  *i += len+1;
}

void dump_heap (Mant_t *ap, Mant_t *heap) {
#if defined(DEBUG)
  printf ("----*heap dump*----\n");
  Word_t heap_sz = (((Word_t)ap-1)-((Word_t)heap))>>3;
  printf ("----base: %lX range: %lX-%lX: len=%ld words limit_ptr=%lX----\n",
		  base, heap, ap, heap_sz, limit_ptr ());
  for (uint_t i = 0; i < (uint_t)heap_sz; ) {
	print_obj (heap, &i);
  }
  printf ("------------------\n");
  printf ("\n");
#endif
}
