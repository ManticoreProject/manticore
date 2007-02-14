/* init-rts.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Glue code for executing CFG code via C.
 */

#include <stdio.h>
#include <stdlib.h>

#include "trivial-cheney-gc.h"

GC_info_t info;

// find me in mantentry.s
Word_t *mantentryglue (void *, void *, void *, void *);

void init_heap () {
  posix_memalign (&from_space, HEAP_ALIGN, HEAP_SIZE*2);
  from_space++;
  to_space = from_space + HEAP_SIZE;
  info.ap = from_space;
}

int main () {
  init_heap ();

  // call the manticore entry function using the "mantentryglue" wrapper
  Word_t *ans = mantentryglue (3, NULL, NULL, info.ap);
  printf ("ans: %ld\n", *ans);

  return 0;
}
