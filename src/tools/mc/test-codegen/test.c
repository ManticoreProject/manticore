/*  test.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *  Glue code for executing CFG code via C.
 */

#include <stdio.h>
#include <stdlib.h>

// find me in mantentry.s
int mantentryglue (void *, void *, void *, void *);

int main () {
  void *heap = malloc (1024);
  int ans = mantentryglue (NULL, NULL, NULL, heap);
  printf ("ans: %d\n", ans);
  return 0;
}
