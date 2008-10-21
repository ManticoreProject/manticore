/* work-stealing-local-deques.c
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Local deques for the work stealing scheduler. 
 * 
 * To support in-place updates, we allocate these deques outside of the heap. Thus
 * we make the elements of the deque GC roots for local collections.
 */

#include "manticore-rt.h"
#include <stdio.h>
#include <string.h>
#include "vproc.h"
#include "value.h"
#include "heap.h"
#include "../gc/gc-inline.h"

#define DEQUE_LEN 1024

/* local work-stealing deque */
typedef struct {
  int hd;                      /* pointer to the head of the deque */
  int tl;                      /* pointer to the tail of the deque */
  Value_t elts[DEQUE_LEN];     /* memory for the deque */
} WSLocalDeque_t;

/*! \brief add the deque elements to the root set.
 *  \param vp the host vproc
 *  \param rp the root-set pointer
 */
Value_t** M_WSAddLocalDequeRoots (VProc_t *vp, Value_t **rp)
{
  /* TODO: find a way to access the active local deques */
  /*
  WSLocalDeque_t* deque = &(deques[vp->id]);
  for (int i = deque->hd; i < deque->tl; i++) {
    *rp++ = &(deque->elts[i]);
  }
  */
  return rp;
}

/* \brief initialize the local deques
   \param vp the host vproc
   \param n the number of local deques to initialize
 */
Value_t M_WSAllocLocalDeques (VProc_t *vp, int n)
{
  WSLocalDeque_t* deques = (WSLocalDeque_t*)ValueToPtr(AllocUniform(vp, n * sizeof(WSLocalDeque_t)));

  for (int i = 0; i < n; i++) {
    deques[i].hd = 0;
    deques[i].tl = 0;
    for (int j = 0; j < DEQUE_LEN; j++) {
      deques[i].elts[j] = M_NIL;
    }
  }

  return deques;
}
