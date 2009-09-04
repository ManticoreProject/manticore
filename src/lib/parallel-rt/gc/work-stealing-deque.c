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
#include "internal-heap.h"
#include "bibop.h"
#include <stdio.h>
#include <string.h>

struct DequeList_s {
  Deque_t               *deque;
  struct DequeList_s    *next;
};
typedef struct DequeList_s DequeList_t;

struct WorkGroupList_s {
  uint64_t                 workGroupId;
  DequeList_t              *deques;
  struct WorkGroupList_s   *next;
};
typedef struct WorkGroupList_s WorkGroupList_t;

static WorkGroupList_t **PerVProcLists;          

/* \brief call this function once during runtime initialization to initialize
 *     gc state */
void M_InitWorkGroupList ()
{
  PerVProcLists = NEWVEC(WorkGroupList_t*, NumVProcs);
  for (int i = 0; i < NumVProcs; i++)
    PerVProcLists[i] = NULL;
}

static WorkGroupList_t *FindWorkGroup (VProc_t *self, uint64_t workGroupId)
{
  for (WorkGroupList_t *wgList = PerVProcLists[self->id]; wgList != NULL; wgList = wgList->next)
    if (wgList->workGroupId == workGroupId)
      return wgList;        // found an entry for the work group
  // found no entry for the given work group, so create such an entry and return it
  WorkGroupList_t *new = NEW(WorkGroupList_t);
  new->workGroupId = workGroupId;
  new->deques = NULL;
  new->next = PerVProcLists[self->id];
  PerVProcLists[self->id] = new;
  return new;
}

static DequeList_t *ConsDeque (Deque_t *deque, DequeList_t *deques)
{
  DequeList_t *new = NEW(DequeList_t);
  new->deque = deque;
  new->next = deques;
  return new;
}

/* \brief allocate a deque on the given vproc to by used by the given group
 * \param self the host vproc
 * \param workGroupId the work group allocating the deque
 * \param size the max number of elements in the deque
 * \return a pointer to the freshly allocated deque
 */
Value_t M_DequeAlloc (VProc_t *self, uint64_t workGroupId, int32_t size)
{
  Deque_t *deque = (Deque_t*)malloc (sizeof(Deque_t) + sizeof(Value_t) * (size - 1));
  deque->new = 0;
  deque->old = 0;
  deque->maxSz = size;
  deque->nClaimed = 1;         // implicitly claim the deque for the allocating process
  for (int i = 0; i < size; i++)
    deque->elts[i] = M_NIL;
  // add the deque to the list of deques owned by the work group
  WorkGroupList_t *workGroup = FindWorkGroup (self, workGroupId);
  workGroup->deques = ConsDeque (deque, workGroup->deques);
  return (PtrToValue (deque));
}

/* \brief return the number of elements in the given deque
 */
static int DequeNumElts (Deque_t *deque)
{
  if (deque->old <= deque->new)
    return deque->new - deque->old;
  else
    return deque->maxSz - deque->old - deque->new;
}

static DequeList_t *PruneDequeList (DequeList_t *deques)
{
  DequeList_t *old = deques;
  DequeList_t *new = NULL;
  DequeList_t *next;
  for (; old != NULL; old = next) {
    next = old->next;
    if (DequeNumElts (old->deque) == 0 && old->deque->nClaimed == 0) {
      FREE(old->deque);
      FREE(old);
    }
    else {
      old->next = new;
      new = old;
    }    
  }
  return new;
}

/* \brief free any deques that have been marked as free since the preceding GC
 * \param self the host vproc
 */
static void Prune (VProc_t *self)
{
  WorkGroupList_t *old = PerVProcLists[self->id];
  WorkGroupList_t *new = NULL;
  WorkGroupList_t *next;
  for (; old != NULL; old = next) {
    next = old->next;
    old->deques = PruneDequeList (old->deques);
    if (old->deques == NULL) {
      FREE(old);
    }
    else {
      old->next = new;
      new = old;
    }    
  }
  PerVProcLists[self->id] = new;
}

/* \brief number of roots needed for deques on the given vproc 
 * \param self the host vproc
 * \return number of roots
*/
int M_NumDequeRoots (VProc_t *self)
{
  int numRoots = 0;
  //  Prune (self);
  for (WorkGroupList_t *wgList = PerVProcLists[self->id]; wgList != NULL; wgList = wgList->next)
    for (DequeList_t *deques = wgList->deques; deques != NULL; deques = deques->next)
      numRoots += DequeNumElts (deques->deque);
  return numRoots;
}

/* \brief move left one position in the deque
 */
static int MoveLeft (int i, int sz)
{
  if (i <= 0)
    return sz - 1;
  else
    return i - 1;
}

#ifdef ROOT_SET_OPTIMIZATION

/* The root-set-partitioning optimization partitions the root set into the subset
 * needed by minor collections only and the subset needed by global collections. 
 *
 * FIXME: this code is broken
 */

// returns true if the ith element of the deque points into the local heap
#define ELT_POINTS_TO_LOCAL_HEAP(deque, i) (IS_VPROC_CHUNK(AddrToChunk(ValueToAddr(deque->elts[i]))->sts))

/* \brief add the deque elements to the root set to be used by a minor collection
 * \param self the host vproc
 * \param rootPtr pointer to the root set
 * \return the updated root set
 */
Value_t **M_AddDequeEltsToLocalRoots (VProc_t *self, Value_t **rootPtr)
{
  for (WorkGroupList_t *wgList = PerVProcLists[self->id]; wgList != NULL; wgList = wgList->next) {
    for (DequeList_t *deques = wgList->deques; deques != NULL; deques = deques->next) {
      Deque_t *deque = deques->deque;
      // iterate through the deque in the direction going from the new to the old end
      for (int i = deque->new; i != deque->old; i = MoveLeft (i, deque->maxSz)) {
	int j = MoveLeft (i, deque->maxSz);
                  // i points one element to right of the element we want to scan
	if (deque->elts[j] != M_NIL)
	  if (ELT_POINTS_TO_LOCAL_HEAP(deque, j)) {
	    // the jth element is in the local heap
	    *rootPtr++ = &(deque->elts[j]);
	  } else {
	    /* the jth element points to the global heap, so we do not need to add it
	     * to the root set. elements to the right of the jth position must also 
	     * point to the global heap, so it is safe to return the current root set. this
	     * property always holds for two reasons:
	     *   1. new elements can only be inserted at the new (rightmost) end of the deque
	     *   2. elements are not explicitly promoted when inserted into the deque
	     */
#ifndef NDEBUG
	    // check that none of the elements to the right of the jth element point to the local heap
      	    for (int i = j; i != deque->old; i = MoveLeft (i, deque->maxSz)) {
	      int j = MoveLeft (i, deque->maxSz);	   
	      if (deque->elts[j] != M_NIL)
		assert (!ELT_POINTS_TO_LOCAL_HEAP(deque, j));
	    }
#endif
	    return rootPtr;
	  }
      }	    
    }
  }
  return rootPtr;
}

/* \brief add the deque elements to the root set to be used by a global collection
 * \param self the host vproc
 * \param rootPtr pointer to the root set
 * \return the updated root set
 */
Value_t **M_AddDequeEltsToGlobalRoots (VProc_t *self, Value_t **rootPtr)
{
  for (WorkGroupList_t *wgList = PerVProcLists[self->id]; wgList != NULL; wgList = wgList->next) {
    for (DequeList_t *deques = wgList->deques; deques != NULL; deques = deques->next) {
      Deque_t *deque = deques->deque;
      bool inGlobalHeap = false;    // true, if all elements remaining must be in the global heap
      // iterate through the deque in the direction going from the new to the old end
      for (int i = deque->new; i != deque->old; i = MoveLeft (i, deque->maxSz)) {
	int j = MoveLeft (i, deque->maxSz); 
                  // i points one element to right of the element we want to scan
	if (deque->elts[j] != M_NIL)
	  if (inGlobalHeap) {
	    // the jth element points to the global heap
	    *rootPtr++ = &(deque->elts[j]);
	  } else if (!ELT_POINTS_TO_LOCAL_HEAP(deque, j)) {
	    *rootPtr++ = &(deque->elts[j]);     // j points to the youngest element in the global heap
	    inGlobalHeap = true;
	  } else {}
      }	    
    }
  }
  return rootPtr;
}

#else /* no root-set optimization: instead we just add the entire deque to each local 
       * collection */

/* \brief add the deque elements to the root set to be used by a minor collection
 * \param self the host vproc
 * \param rootPtr pointer to the root set
 * \return the updated root set
 */
Value_t **M_AddDequeEltsToLocalRoots (VProc_t *self, Value_t **rootPtr)
{
  for (WorkGroupList_t *wgList = PerVProcLists[self->id]; wgList != NULL; wgList = wgList->next) {
    for (DequeList_t *deques = wgList->deques; deques != NULL; deques = deques->next) {
      Deque_t *deque = deques->deque;
      // iterate through the deque in the direction going from the new to the old end
      for (int i = deque->new; i != deque->old; i = MoveLeft (i, deque->maxSz)) {
	int j = MoveLeft (i, deque->maxSz); 
                  // i points one element to right of the element we want to scan
	if (deque->elts[j] != M_NIL)
	  *rootPtr++ = &(deque->elts[j]);
      }	    
    }
  }
  return rootPtr;
}

/* \brief add the deque elements to the root set to be used by a global collection
 * \param self the host vproc
 * \param rootPtr pointer to the root set
 * \return the updated root set
 */
Value_t **M_AddDequeEltsToGlobalRoots (VProc_t *self, Value_t **rootPtr)
{
  return rootPtr;
}

#endif /*! ROOT_SET_OPTIMIZATION */

/* \brief returns a list of all deques on the host vproc corresponding to the given work group
 * \param self the host vproc
 * \param the work group id
 * \return pointer to a linked list of the deques
 */
Value_t M_LocalDeques (VProc_t *self, uint64_t workGroupId)
{
  Value_t l = M_NIL;
  for (DequeList_t *deques = FindWorkGroup (self, workGroupId)->deques; deques != NULL; deques = deques->next) {
    deques->deque->nClaimed++;    // claim the deque for the calling process
    Value_t deque = AllocUniform (self, 1, PtrToValue(deques->deque));
    l = Cons (self, deque, l);    
  }
  return l;
}

void M_AssertDequeAddr (Deque_t *d, int i, void *p)
{
  assert (p == (& (d->elts[i])));
}
