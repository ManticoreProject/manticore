#include "trivial-cheney-gc.h"
#include <stdio.h>
#define __USE_XOPEN2k
#include <stdlib.h>

typedef struct {
  Bool_t is_pointer;
  Mant_t data;
} Alloc_t;

GC_info_t info;

Mant_t *alloc (uint_t len, Alloc_t *alloc_arr) {
  Word_t hdr = 0;
  Mant_t *heap = info.ap;

  for (int i = len-1; i >= 0; i--) {
	heap[i] = (Mant_t)alloc_arr[i].data;
	hdr = (hdr << 1l) | (Word_t)alloc_arr[i].is_pointer;
  }
  info.ap += len + 1;
  heap[-1] = ((Word_t)len)|(hdr<<8l);
  return heap;
}

void init_raw_obj (Alloc_t *a, Mant_t data) {
  a->is_pointer = FALSE;
  a->data = data;
}
void init_ptr_obj (Alloc_t *a, Mant_t *ptr) {
  a->is_pointer = TRUE;
  a->data = (Mant_t)ptr;
}

void init_heap () {
  posix_memalign (&from_space, HEAP_ALIGN, HEAP_SIZE*2);
  from_space++;
  to_space = from_space + HEAP_SIZE;
  info.ap = from_space;
}

void gc (Mant_t *root_fs) {
  info.root = root_fs;
  init_gc (&info);
}

Mant_t data_value (Mant_t *obj, uint_t i) {
  if (i < hdr_len (obj)) {
	return obj[i];
  } else {
	printf ("index too large\n");
	exit (1);
  }
}

#include "list.c"

int main () {
  init_heap ();

  Cons_cell_t *ls_0 = tabulate (3);
  Cons_cell_t *ls_1 = tabulate (4);
  Cons_cell_t *lss = cons_ptr (ls_0, cons_ptr (ls_1, NULL));
  print_ls (lss);

  Alloc_t arr_root[1];
  init_ptr_obj (arr_root, lss);
  Mant_t *root_obj = alloc (1, arr_root);

  gc (root_obj);

  print_ls (data_value (info.root, 0));

  Cons_cell_t *ls_2 = tabulate (4);

  gc (info.root);

  print_ls (data_value (info.root, 0)); 

  /*  Alloc_t arr0[1];
  init_ptr_obj (arr0, 10);
  Mant_t *obj0 = alloc (&heap, 1, arr0);

  Alloc_t arr[3];
  init_raw_obj (&arr[0], 1024);
  init_raw_obj (&arr[1], 1025);
  init_ptr_obj (&arr[1], obj0);
  init_raw_obj (&arr[2], 1026);
  Mant_t *obj1 = alloc (&heap, 3, arr);

  obj0[0] = (Mant_t)obj1;

  Alloc_t arr_root[1];
  init_ptr_obj (arr_root, obj1);

  Mant_t *root_obj = alloc (&heap, 1, arr_root);

  Mant_t *p1 = (Mant_t*)data_value (root_obj, 0);
  printf ("%ld\n", data_value (p1, 1));
  
  Mant_t *root_obj_1 = gc (root_obj);

  Mant_t *p2 = (Mant_t*)data_value (root_obj_1, 0);
  printf ("%ld %ld\n", p2, data_value (data_value (p2, 1),0));
  */
  return 0;
}
