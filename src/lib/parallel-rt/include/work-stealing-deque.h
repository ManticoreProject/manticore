/* work-stealing-deque.h
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Deque structure used by the Work Stealing scheduler.
 */

#ifndef _WORK_STEALING_DEQUE_H_
#define _WORK_STEALING_DEQUE_H_

#include <stdio.h>
#include <string.h>
#include "manticore-rt.h"
#include "vproc.h"
#include "value.h"

/* deque structure */
struct Deque_s {
  int32_t       old;           // pointer to the oldest element in the deque
  int32_t       new;           // pointer to the address immediately to the right of the newest element
  int32_t       maxSz;         // max number of elements
  bool          live;          // true, if the deque is being used by a scheduler
  Value_t       elts[];        // elements of the deque
};
typedef struct Deque_s Deque_t;

/* linked list of all deques local to a particular vproc */
struct DequeList_s {
  Deque_t                *hd;
  struct DequeList_s     *tl;  
};
typedef struct DequeList_s DequeList_t;

/* must call this function once at startup */
void M_InitDequeList ();

Value_t M_DequeAlloc (VProc_t *self, int size);
void M_DequeFree (Deque_t *deque);

/* number of roots needed for deques on the given vproc */
int M_NumDequeRoots (VProc_t *self);
/* add the deque elements to the root set */
Value_t **M_AddDequeEltsToRoots (VProc_t *self, Value_t **rootPtr);

#endif /*! _WORK_STEALING_DEQUE_H_ */
