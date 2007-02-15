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

#define HEAP_SIZE_W (HEAP_SIZE>>3)

// find me in mantentry.s
Word_t *mantentryglue (void *, void *, void *, void *);

void init_heap () {
  posix_memalign (&from_space, HEAP_ALIGN*2, HEAP_SIZE*2);
  base = from_space;
  from_space++;
  to_space = from_space + HEAP_SIZE_W;
  info.ap = from_space;
  high = from_space + (HEAP_SIZE_W*2);
  low = from_space;
}

int main (int argc, char *argv[]) {
  if (argc <= 1) {
	printf ("usage: ./rts <int>\n");
	exit(1);
  }

  int arg = atoi (argv[1]);
  init_heap ();

  // call the manticore entry function using the "mantentryglue" wrapper
  Word_t *ans = mantentryglue ((Word_t)arg, NULL, NULL, info.ap);
  printf ("ans: %ld\n", *ans);

  return 0;
}
