/* work-queue.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A simple implementation of work queues to support futures, etc.
 */

#include "manticore-rt.h"
#include "value.h"
#include "os-threads.h"

/* An item in the queue */
typedef struct struct_qitem WorkQItem_t;

/* A work queue; note that thses are allocated in the global heap */
typedef struct {
    Mutex_t	*lock;	// pointer to malloc'd lock for the queue */
    Value_t	front;	// front of queue
    Value_t	rear;	// rear of queue
} WorkQueue_t;

struct struct_qitem {
    Value_t	item;
    Value_t	next;
};


/* M_NewWorkQueue:
 */
Value_t M_NewWorkQueue (VProc_t *vp)
{
    Mutex_t	*lock = NEW(Mutex_t);
    MutexInit (lock);

  // allocate dummy queue item
    Value_t qitem = GlobalAllocUniform (vp, 2, M_UNIT, M_NIL);

  // allocate queue header
    Value_t workQ = GlobalAllocNonUniform (vp, 3, INT(lock), PTR(qitem), PTR(qitem));

    return workQ;
}

/* M_WorkDequeue:
 */
Value_t M_WorkDequeue (VProc_t *vp, Value_t q)
{
    WorkQueue_t		*workQ = (WorkQueue_t *)ValueToPtr(q);

    MutexLock (workQ->lock);

    if (workQ->front == workQ->rear) {
	MutexUnlock (workQ->lock);
	return M_NONE;
    }
    else {
	WorkQItem_t *f = (WorkQItem_t *)ValueToPtr(workQ->front);
	workQ->front = f->next;
	MutexUnlock (workQ->lock);
	f->next = M_NIL;
	return Some(vp, f->item);
    }

}

/* M_WorkEnqueue:
 */
void M_WorkEnqueue (VProc_t *vp, Value_t q, Value_t item)
{
    WorkQueue_t		*workQ = (WorkQueue_t *)ValueToPtr(q);

    MutexLock (workQ->lock);
    Value_t newItem = GlobalAllocUniform(vp, 2, M_UNIT, M_NIL);
    WorkQItem_t *r = (WorkQItem_t *)ValueToPtr(workQ->rear);
    r->next = newItem;
    r->item = item;
    workQ->rear = newItem;
    MutexUnlock (workQ->lock);

} /* M_WorkEnqueue */
