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
  obj->hdr.hdr_word = hdr;
  obj->hdr.hdr_bytes.length = (unsigned char)length;
  return (Object_t*)obj;
}

int main () {
  // allocate the heap
  void **heap;
  posix_memalign (&heap, HEAP_ALIGN, HEAP_SIZE);

  Alloc_t arr[3];
  arr[0].is_pointer = TRUE;
  arr[0].data = (void*)1023;

  arr[1].is_pointer = FALSE;
  arr[1].data = (void*)1024;

  arr[2].is_pointer = TRUE;
  arr[3].data = (void*)1025;

  Object_t *obj = alloc (&heap, 3, arr);
  return 0;
}
