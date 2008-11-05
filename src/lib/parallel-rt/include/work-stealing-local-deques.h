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

typedef struct WSLocalDeques_s WSLocalDeques_t;

Value_t** M_WSAddLocalDequesToRoots (VProc_t* vp, Value_t** rp);
Value_t M_WSAllocLocalDeque (int vprocId);
Value_t M_WSGetLocalDeque (WSLocalDeques_t* localDeques);
void M_WSFreeLocalDeque (WSLocalDeques_t* localDeques);
void M_WSInit (int nVProcs);

#endif /* _WORK_STEALING_LOCAL_DEQUES_H_ */
