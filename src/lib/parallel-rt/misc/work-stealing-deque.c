/* work-stealing-deque.c
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Deque structure used by the Work Stealing scheduler.
 *
 * NOTES:
 *   - The deques are allocated in the C heap.
 */

#include "work-stealing-deque.h"

static DequeList_t **PerVProcDequeLists;

void M_InitDequeList ()
{
  PerVProcDequeLists = NEWVEC(DequeList_t*, NumVProcs);
  for (int i = 0; i < NumVProcs; i++)
    PerVProcDequeLists[i] = ValueToPtr(M_NIL);
}

static DequeList_t *ConsDeque (Deque_t *deque, DequeList_t *deques)
{
  DequeList_t *new = NEW(DequeList_t);
  new->hd = deque;
  new->tl = deques;
  return new;
}

Value_t M_DequeAlloc (VProc_t *self, int size)
{
  Deque_t *deque = NEW(Deque_t);
  deque->new = 0;
  deque->old = 0;
  deque->live = true;
  deque->maxSz = size;
  for (int i = 0; i < size; i++)
    deque->elts[i] = M_NIL;
  /* add the new deque to the vproc's list of deques */
  PerVProcDequeLists[self->id] = ConsDeque (deque, PerVProcDequeLists[self->id]);
  return (PtrToValue (deque));
}

void M_DequeFree (Deque_t *deque)
{
  deque->live = false;
}

static int MoveRight (int i, int sz)
{
  if (i >= (sz - 1))
    return 0;
  else
    return i + 1;
}

/* \brief number of roots needed for deques on the given vproc 
 * \param self the host vproc
 * \return number of roots
*/
int M_NumDequeRoots (VProc_t *self)
{
  int numRoots = 0;
  DequeList_t *deques = PerVProcDequeLists[self->id];
  while (deques != ValueToPtr(M_NIL)) {
    Deque_t *deque = deques->hd;
    for (int i = deque->old; i != deque->new; i = MoveRight (i, deque->maxSz))
      numRoots++;
    deques = deques->tl;
  }
  return numRoots;
}

/* \brief free any deques that have been marked as free since the last GC
 * \param self the host vproc
 */
static void Prune (VProc_t *self)
{
  DequeList_t *deques = PerVProcDequeLists[self->id];
  DequeList_t *new = ValueToPtr(M_NIL);
  while (deques != ValueToPtr(M_NIL)) {
    Deque_t *deque = deques->hd;
    DequeList_t *next = deques->tl;
    if (deque->live) {
      deques->tl = new;
      new = deques;
    }
    else {
	  FREE(deque);
      FREE(deques);
    }
    deques = next;
  }
  PerVProcDequeLists[self->id] = new;
}

/* \brief add the deque elements to the root set 
 * \param self the host vproc
 * \param rootPtr pointer to the root set
 * \return the updated root set
 */
Value_t **M_AddDequeEltsToRoots (VProc_t *self, Value_t **rootPtr)
{
  Prune (self);
  DequeList_t *deques = PerVProcDequeLists[self->id];
  while (deques != ValueToPtr(M_NIL)) {
    Deque_t *deque = deques->hd;
    for (int i = deque->old; i != deque->new; i = MoveRight (i, deque->maxSz)) {
	  assert (deque->elts[i] != M_NIL);
	  *rootPtr++ = &(deque->elts[i]);
	}
    deques = deques->tl;
  }
  return rootPtr;
}
