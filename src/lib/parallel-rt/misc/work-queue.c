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
#include "heap.h"

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

    Say ("Allocated work queue %p\n");

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
	f->next = M_NIL;
	MutexUnlock (workQ->lock);
	return Some(vp, f->item);
    }

}


/* return true of the given address is within the given vproc heap */
STATIC_INLINE bool inVPHeap (Addr_t heapBase, Addr_t p)
{
    return (heapBase == (p & ~VP_HEAP_MASK));
}


/* M_WorkEnqueue:
 */
void M_WorkEnqueue (VProc_t *vp, Value_t q, Value_t item)
{
    WorkQueue_t		*workQ = (WorkQueue_t *)ValueToPtr(q);
    /* item must be a pointer in the global queue, and thus we can
     * at least be sure that it is not in the local queue
     */
    assert (!(inVPHeap ((Addr_t)vp, (Addr_t)item)));

    MutexLock (workQ->lock);
    Value_t newItem = GlobalAllocUniform(vp, 2, M_UNIT, M_NIL);
    WorkQItem_t *r = (WorkQItem_t *)ValueToPtr(workQ->rear);
    r->next = newItem;
    r->item = item;
    workQ->rear = newItem;
    MutexUnlock (workQ->lock);

} /* M_WorkEnqueue */
