/* work-stealing-local-deques.c
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Local deques for the work stealing scheduler. 
 * 
 * NOTES:
 *   - The local deque structures actually exist outside of the heap, but they contain
 *     pointers to fibers in the local or global heaps. For this reason, we must treat
 *     these elements are GC roots.
 *   - We support multiple instances of the work stealing scheduler at run time. In particular, 
 *     multiple workers may multiplex on a single vproc.
 */

#include "work-stealing-local-deques.h"

/* one list of local deques per vproc */
WSLocalDeques_t** globalLists;


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

    return PruneLocalDequesLoop(tl, pruned);
  }
}

/* \brief prune dead local deques from the global list
 * \param vprocId unique id of the host vproc
 * \return pruned pruned list of local deques
 */
static WSLocalDeques_t* PruneLocalDeques (int vprocId)
{
  return (globalLists[vprocId] = PruneLocalDequesLoop(globalLists[vprocId], ValueToPtr(M_NIL)));
}

/*! \brief add the deque elements to the root set; at this point we prune out deques that are no longer live.
 *  \param vp the host vproc
 *  \param rp the root-set pointer
 */
Value_t** M_WSAddLocalDequesToRoots (VProc_t* vp, Value_t** rp)
{
  WSLocalDeques_t* globalList = PruneLocalDeques(vp->id);

  while (PtrToValue(globalList) != M_NIL) {
    WSLocalDeque_t* localDeque = globalList->hd;

    //    printf("hd=%d tl=%d\n", localDeque->hd, localDeque->tl);
    for(Word_t i = localDeque->hd; i < localDeque->tl; i++) {
      //printf("localDeque=%p elt=%p i=%d root=%p\n", localDeque, localDeque->elts[i], i, rp);
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
static WSLocalDeques_t* AddToGlobalList (int vprocId, WSLocalDeque_t* localDeque)
{
  WSLocalDeques_t* globalList = NEW(WSLocalDeques_t);

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

  deque->tl = 0;
  deque->hd = 0;

  //  printf("tl==%p elts=%p\n", &(deque->tl), &(deque->elts));

  for (int i = 0; i < WORK_STEALING_LOCAL_DEQUE_LEN; i++) {
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

/* \brief obtain the local deque at the head of the list
 * \param localDeques list of local deques
 * \return the local deque at the head of the list
 */
Value_t M_WSGetLocalDeque (WSLocalDeques_t* localDeques)
{
  assert(localDeques->live == M_TRUE);
  return PtrToValue(localDeques->hd);
}

/* \brief free the local deque at the head of the list
 * \param localDeques list of local deques
 */
void M_WSFreeLocalDeque (WSLocalDeques_t* localDeques)
{
  localDeques->live = M_FALSE;
}

/* \brief initialize the work-stealing infrastructure
 * \param nVProcs the number of vprocs
 */
void M_WSInit (int nVProcs)
{
  /* allocate and initialize the global list of local deques */
  globalLists = NEWVEC(WSLocalDeques_t*, nVProcs);

  for (int i = 0; i < nVProcs; i++) {
    globalLists[i] = ValueToPtr(M_NIL);
  }
}
