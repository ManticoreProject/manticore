/* work-stealing-local-deques.h
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Local deques for the work stealing scheduler. 
 * 
 */

#ifndef _WORK_STEALING_LOCAL_DEQUES_H_
#define _WORK_STEALING_LOCAL_DEQUES_H_

#include "manticore-rt.h"
#include <stdio.h>
#include <string.h>
#include "vproc.h"
#include "value.h"

/* max size for the local deque */
#define WORK_STEALING_LOCAL_DEQUE_LEN               1024
/* maximum number of elements that can exist in the deque at one time, i.e., at all times it must be the case that
 *   hd - tl <= WORK_STEALING_LOCAL_DEQUE_MAX_ELTS
 */
#define WORK_STEALING_LOCAL_DEQUE_MAX_ELTS          128

/* maximum number of workers that can be simultaneuously active on a vproc.
 */
#define WORK_STEALING_MAX_NUM_LOCAL_DEQUES          64

/* upper bound on the number of roots needed for local deques on a given vproc.
 */
#define WORK_STEALING_LOCAL_DEQUE_MAX_ROOTS         (WORK_STEALING_LOCAL_DEQUE_MAX_ELTS * WORK_STEALING_MAX_NUM_LOCAL_DEQUES)

/* local, work-stealing deque */
typedef struct {
  Word_t hd;                                           /* pointer to the head of the deque */
  Word_t tl;                                           /* pointer to the tail of the deque */
  Value_t elts[WORK_STEALING_LOCAL_DEQUE_LEN];         /* memory for the deque */
} WSLocalDeque_t;

/* list of worker deques */
struct WSLocalDeques_s {
  WSLocalDeque_t* hd;                 /* local deque */
  struct WSLocalDeques_s* tl;         /* rest of the list */
  Value_t live;                       /* if this field is false, then the local deque is garbage */
};

typedef struct WSLocalDeques_s WSLocalDeques_t;

Value_t M_WSAllocLocalDeque (int vprocId);

Value_t** M_WSAddLocalDequesToRoots (VProc_t* vp, Value_t** rp);
void M_WSInit (int nVProcs);

#endif /* _WORK_STEALING_LOCAL_DEQUES_H_ */
