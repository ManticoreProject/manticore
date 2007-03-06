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

typedef union {
  int int_v;
  Word_t word_v;
  float float_v;
} Ret_t;

// find me in mantentry.s
Ret_t *mantentryglue (
		Word_t arg,           // command line argument
		Mant_t *limit_ptr,    // initial limit pointer
		Mant_t *junk,         // placeholder
		Mant_t *ap);          // allocation pointer

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
  Ret_t *ans = mantentryglue ((Word_t)arg, limit_ptr (), NULL, info.ap);
  printf ("ans->int=%d ans->word=%ld ans->float=%f\n", 
		  ans->int_v, ans->word_v, ans->float_v);

  return 0;
}
