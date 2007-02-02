#include "cheney-gc.h"
#include <stdio.h>
#define __USE_XOPEN2k
#include <stdlib.h>

typedef struct {
  Bool_t is_pointer;
  void *data;
} Alloc_t;

Object_t *alloc (void ***ap, uint_t length, Alloc_t *alloc_arr) {
  Word_t hdr = 0;
  void **heap = *ap;

  for (uint_t i = 1; i <= length; i++) {
    heap[i] = alloc_arr[i-1].data;
    hdr = (hdr << 1) | (Word_t)alloc_arr[i-1].is_pointer;
  }
  *ap += length+1;
  Object_t *obj = (Object_t*)heap;
  obj->hdr.hdr_word = ((Word_t)length<<56)|hdr;
  return obj;
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

void *data_value (Object_t *obj, uint_t i) {
  if (i > hdr_len (obj)) {
	printf ("index too large\n");
	exit (1);
  } else {
	void **data = &obj->data;
	return data[i];
  }
}

int main () {
  init_heap ();
  void **heap = from_space;

  Alloc_t arr1[1];
  init_raw_obj (&arr1[0], 1023);
  Object_t *obj1 = alloc (&heap, 1, arr1);

  Alloc_t arr[3];
  init_raw_obj (&arr[0], 1024);
  init_ptr_obj (&arr[1], obj_to_pointer (obj1));
  init_raw_obj (&arr[2], 1026);
  Object_t *obj = alloc (&heap, 3, arr);

  Alloc_t arr_root[1];
  init_ptr_obj (&arr_root[0], obj_to_pointer (obj));

  Object_t *root_obj = alloc (&heap, 1, arr_root);

  Object_t *obj_1 = pointer_to_obj (data_value (root_obj, 0));
  printf ("%ld\n",(Word_t)data_value (obj_1, 2));

  init_gc (&root_obj->data);

  Object_t *obj_2 = pointer_to_obj (data_value (root_obj, 0));
  printf ("%ld\n",(Word_t)data_value (obj_2, 2));

  return 0;
}
