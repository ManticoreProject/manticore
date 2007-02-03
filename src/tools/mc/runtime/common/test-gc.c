#include "cheney-gc.h"
#include <stdio.h>
#define __USE_XOPEN2k
#include <stdlib.h>

typedef struct {
  Bool_t is_pointer;
  void *data;
} Alloc_t;

void ***alloc (void ***ap, uint_t length, Alloc_t *alloc_arr) {
  Word_t hdr = 0;
  void **heap = *ap;

  for (uint_t i = 1; i <= length; i++) {
    heap[i] = alloc_arr[i-1].data;
    hdr = (hdr << 1) | (Word_t)alloc_arr[i-1].is_pointer;
  }
  *ap += length+1;
  Object_t *obj = (Object_t*)heap;
  obj->hdr.hdr_word = ((Word_t)length<<56)|hdr;
  return &obj->data;
}

void init_raw_obj (Alloc_t *a, Word_t data) {
  a->is_pointer = FALSE;
  a->data = (void*)data;
}
void init_ptr_obj (Alloc_t *a, void *ptr) {
  a->is_pointer = TRUE;
  a->data = ptr;
}

void init_heap () {
  posix_memalign (&from_space, HEAP_ALIGN, HEAP_SIZE*2);
  to_space = from_space + HEAP_SIZE;
}

void *data_value (void **data, uint_t i) {
  if (i > hdr_len (pointer_to_obj (data))) {
	printf ("index too large\n");
	exit (1);
  } else {
	return data[i];
  }
}

int main () {
  init_heap ();
  void **heap = from_space;

  Alloc_t arr1[1];
  init_raw_obj (&arr1[0], 1023);
  void ***obj1 = alloc (&heap, 1, arr1); 

  Alloc_t arr[3];
  init_raw_obj (&arr[0], 1024);
  //init_raw_obj (&arr[1], 1025);
  init_ptr_obj (&arr[1], obj1);
  init_raw_obj (&arr[2], 1026);
  void ***obj = alloc (&heap, 3, arr);

  Alloc_t arr_root[1];
  init_ptr_obj (&arr_root[0], obj);

  void ***root_obj = alloc (&heap, 1, arr_root);

  void *p1 = data_value (root_obj, 0);
  printf ("%ld\n",(Word_t)data_value (p1, 2));

  init_gc (root_obj);

  void *obj3 = root_obj[0];
  printf ("%ld\n",(Word_t)data_value (obj3, 2)); 

  return 0;
}
