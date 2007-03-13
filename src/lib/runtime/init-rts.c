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
#include "../parallel-rt/include/value.h"

extern Value_t RunManticore (VProc_t *vp, Value_t f, Value_t arg);
int TotalVM;
int MaxNurserySzB;

//GC_info_t info;
static pthread_key_t	VProcInfoKey;

typedef union {
  int int_v;
  Word_t word_v;
  float float_v;
} Ret_t;

void Die (const char *fmt, ...) {
  printf ("die\n");
  exit (1);
}

void Error (const char *fmt, ...) {
  printf ("error\n");
  exit (1);
}

void Warning (const char *fmt, ...) {
  
}

// dummy slot for the manticore entry function
extern int mantentry ();

VProc_t *InitVProcHeap () {
  VProc_t *vp;
  Addr_t from_space;

  posix_memalign (&from_space, HEAP_ALIGN*2, HEAP_SIZE*2);
  /* the vproc structure is at the bottom of its heap */
  vp = from_space;
  /* use globLimit as a pointer to the bottom of the heap */
  vp->globLimit = from_space;
  /* allocate space for the vproc structure */
  from_space += sizeof(VProc_t);
  /* Keep the invariant that the allocation pointer always points one
   * word past the first object.
   */
  from_space += 8;
  FROM_SPACE(vp) = from_space;

  set_to_space (vp, FROM_SPACE(vp) + HEAP_SIZE_W);
  vp->allocPtr = FROM_SPACE(vp);
  set_limit_ptr (vp);

  numGCs = 0;
  
  return vp;
}

Options_t *opts;
int arg;

static void VProcMainSEQ (void *_data) {
  VProc_t *vp = (VProc_t*)_data;

  vp->stdArg = 0;
  vp->stdEnvPtr = 0;
  vp->stdCont = 0;
  vp->stdExnCont = 0;
  
  /* store a pointer to the VProc info as thread-specific data */
  pthread_setspecific (VProcInfoKey, vp);
  
  int argVal = GetIntOpt (opts, "-a", arg);

  Value_t entry = AllocUniform (vp, 2, PtrToValue(&mantentry), NULL);
  Ret_t *ans = RunManticore (vp, entry, argVal);

  printf ("ans->int=%d ans->word=%ld ans->float=%f\tnumGCs=%d\n", 
		  ans->int_v, ans->word_v, ans->float_v,numGCs);
  
  FREE (vp);
}

VProc_t *VProcCreateSEQ () {
  VProc_t *vp = InitVProcHeap (vp);

  ThreadCreate (&(vp->hostID), VProcMainSEQ, vp);

  return vp;
}

int main (int argc, char *argv[]) {

  printf ("Running manticore from the sequential runtime system\n");
  opts = InitOptions (argc, argv);
  VProcInit (opts);
  // create a single vproc
  VProcCreateSEQ ();
  while (1) {}

  return 0;
}
