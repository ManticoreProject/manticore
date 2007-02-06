/* init-rts.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Glue code for executing CFG code via C.
 */

#include <stdio.h>
#include <stdlib.h>

#include "gc-defs.h"

// find me in mantentry.s
int mantentryglue (void *, void *, void *, void *);

int main () {
  // allocate the heap
  void *heap;
  posix_memalign (&heap, HEAP_ALIGN, HEAP_SIZE);
  heap++;

  // call the manticore entry function using the "mantentryglue" wrapper
  int ans = mantentryglue (NULL, NULL, NULL, heap);
  printf ("ans: %d\n", ans);

  return 0;
}
