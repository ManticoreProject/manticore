#include <stdio.h>

void print_obj (Mant_t *heap, uint_t *i) {
  Mant_t *obj = heap + *i;
  uint_t len = hdr_len (obj);
  printf ("\t- object #%d len=%d -\n", *i, len);
  for (uint_t j = 0; j < len; j++) {
	if (is_pointer (obj, j)) {
	  printf ("\t\t #%d 0x%lX \n", j, obj[j]);
	} else {
	  printf ("\t\t #%d %ld \n", j, obj[j]);
	}
  } 
  *i += len+1;
}

void dump_heap (Mant_t *ap, Mant_t *heap) {
#if defined(DEBUG)
  printf ("----*heap dump*----\n");
  Word_t heap_sz = (((Word_t)ap-1)-((Word_t)heap))>>3;
  printf ("----len=%ld words----\n",heap_sz);
  for (uint_t i = 0; i < (uint_t)heap_sz; ) {
	print_obj (heap, &i);
  }
  printf ("----*heap dump*----\n");
  printf ("\n");
#endif
}
