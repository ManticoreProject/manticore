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

/* local, work-stealing deque */
typedef struct {
  int hd;                             /* pointer to the head of the deque */
  int tl;                             /* pointer to the tail of the deque */
  Value_t elts[DEQUE_LEN];            /* memory for the deque */
} WSLocalDeque_t;

/* list of worker deques */
struct WSLocalDeques_s {
  WSLocalDeque_t* hd;                 /* local deque */
  struct WSLocalDeques_s* tl;         /* rest of the list */
  Value_t live;                       /* if this field is false, then the local deque is garbage */
};

typedef struct WSLocalDeque_s WSLocalDeques_t;

/* one list of local deques per vproc */
WSLocalDeques_t* globalLists[];

/* \brief prune the dead local deques
 * \param localDeques list of local deques
 * \param pruned list of already pruned local deques
 * \return pruned list of local deques
 */
static WSLocalDeques_t* PruneLocalDequesLoop (WSLocalDeques_t* localDeques, WSLocalDeques_t* pruned)
{
  if (PtrToValue(localDeques) == M_NIL) {
    return pruned;
  } else if (localDeques->live == M_TRUE) {
    WSLocalDeques_t* tl = localDeques->tl;

    localDeques->tl = pruned;

    return PruneLocalDequesLoop(tl, localDeques);
  } else {
    WSLocalDeques_t* tl = localDeques->tl;
    
    FREE(localDeques);

    return PruneLocalDeques(tl, pruned);
  }
}

/* \brief prune dead local deques from the global list
 * \param vprocId unique id of the host vproc
 * \return pruned pruned list of local deques
 */
static WSLocalDeques_t* PruneLocalDeques (int vprocId)
{
  /* TODO */  
}

/*! \brief add the deque elements to the root set; at this point we prune out deques that are no longer live.
 *  \param vp the host vproc
 *  \param rp the root-set pointer
 */
Value_t** M_WSAddLocalDequeRoots (VProc_t* vp, Value_t** rp)
{
  WSLocalDeques_t* globalList = PruneLocalDeques(vp->id);

  while (globalList != M_NIL) {
    WSLocalDeque_t* localDeque = globalList->hd;

    for(int i = localDeque->hd; i < localDeque->tl; i++) {
      *rp++ = &(localDeque->elts[i]);
    }

    globalList = globalList->tl;

  }

  return rp;
}

/* \brief add the local deque to the globally-maintained list of local deques
 * \param vprocId unique id of the host vproc
 * \param localDeque the local deque
 * \return entry in the global list
 */
static WSLocalDeques* AddToGlobalList (int vprocId, WSLocalDeque_t* localDeque)
{
  WSLocalDeques_t* globalList = NEW(WS_LocalDeques_t);

  globalList->hd = localDeque;
  globalList->tl = globalLists[vprocId];
  globalList->live = M_TRUE;

  globalLists[vprocId] = globalList;

  return globalList;
}

/* \brief allocate and initialize a local deque
 * \return new local deque
 */
static WSLocalDeque_t* AllocLocalDeque ()
{
  WSLocalDeque_t* deque = NEW(WSLocalDeque_t);

  deque->tl = deque->hd = 0;

  for (int i = 0; i < DEQUE_LEN; i++) {
    deque->elts[i] = M_NIL;
  }

  return deque;
}

/* \brief allocate a local deque
 * \param the host vproc id
 * \return allocated and initialized local deque
 */
Value_t M_WSAllocLocalDeque (int vprocId)
{
  WSLocalDeque_t* localDeque = AllocLocalDeque();
  WSLocalDeques_t* localDeques = AddToGlobalList(vprocId, localDeque);

  return PtrToValue(localDeques);
}
