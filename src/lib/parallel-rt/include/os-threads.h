/* os-threads.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A thin abstraction layer of the OS provided threading mechanism.
 */

#ifndef _OS_THREADS_H_
#define _OS_THREADS_H_

#include "manticore-rt.h"
#include <pthread.h>
#include <signal.h>

/********** Threads **********/

typedef pthread_t OSThread_t;

STATIC_INLINE bool ThreadCreate (OSThread_t *tidOut, void * (*f)(void *), void *arg)
{
    if (pthread_create(tidOut, (pthread_attr_t *)0, f, arg) != 0)
	return false;
    else
	return true;

} /* end of NewThread */

STATIC_INLINE OSThread_t ThreadSelf ()
{
    return pthread_self();
}

/* signalling a thread */
STATIC_INLINE void ThreadKill (OSThread_t tid, int sig)
{
    pthread_kill (tid, sig);
} /* end of ThreadKill */


/********** Mutual exclusion synchronization **********/

typedef pthread_mutex_t Mutex_t;

STATIC_INLINE void MutexInit (Mutex_t *mu)
{
    pthread_mutex_init (mu, (pthread_mutexattr_t *)0);
} /* end of NewMutex */

STATIC_INLINE void DestroyMutex (Mutex_t *mu)
{
    pthread_mutex_destroy (mu);
} /* end of DestroyMultex */

STATIC_INLINE void MutexLock (Mutex_t *mu)
{
    pthread_mutex_lock (mu);
}

STATIC_INLINE void MutexUnlock (Mutex_t *mu)
{
    pthread_mutex_unlock (mu);
}

STATIC_INLINE void MutexDestroy (Mutex_t *mu)
{
    pthread_mutex_destroy (mu);
}


/********** Condition synchronization **********/

typedef pthread_cond_t Cond_t;

STATIC_INLINE void CondInit (Cond_t *cond)
{
    pthread_cond_init (cond, (pthread_condattr_t *)0);
} /* end of CondInit */

STATIC_INLINE void DestroyCond (Cond_t *cond)
{
    pthread_cond_destroy (cond);
} /* end of DestroyCond */

STATIC_INLINE void CondWait (Cond_t *cond, Mutex_t *mu)
{
    pthread_cond_wait (cond, mu);
}

STATIC_INLINE void CondSignal (Cond_t *cond)
{
    pthread_cond_signal (cond);
}

STATIC_INLINE void CondBroadcast (Cond_t *cond)
{
    pthread_cond_broadcast (cond);
}

STATIC_INLINE void CondDestroy (Cond_t *cond)
{
    pthread_cond_destroy (cond);
}

/********** Barrier synchronization **********/

#ifdef HAVE_PTHREAD_BARRIER

typedef pthread_barrier_t Barrier_t;

STATIC_INLINE void BarrierInit (Barrier_t *b, int nProcs)
{
    pthread_barrier_init (b, 0, nProcs);
}

STATIC_INLINE bool BarrierWait (Barrier_t *b)
{
    return (pthread_barrier_wait (b) != 0);
}

STATIC_INLINE void BarrierDestroy (Barrier_t *b)
{
    pthread_barrier_destroy (b);
}

#else /* !HAVE_PTHREAD_BARRIER */

typedef struct {
    int		nProcs;		/*!< number of processes involved in the sync */
    int		nWaiting;	/*!< number of processes currently blocked */
    Mutex_t	lock;		/*!< lock to control access to barrier rep. */
    Cond_t	wait;		/*!< condition for waiting */
} Barrier_t;

STATIC_INLINE void BarrierInit (Barrier_t *b, int nProcs)
{
    b->nProcs = nProcs;
    b->nWaiting = 0;
    MutexInit (&(b->lock));
    CondInit (&(b->wait));
}

STATIC_INLINE bool BarrierWait (Barrier_t *b)
{
    bool	result = false;
    MutexLock (&(b->lock));
	if (++(b->nWaiting) < b->nProcs)
	    CondWait (&(b->wait), &(b->lock));
	else {
	    CondBroadcast (&(b->wait));
	    result = true;
	}
    MutexUnlock (&(b->lock));

    return result;
}

STATIC_INLINE void BarrierDestroy (Barrier_t *b)
{
    assert (b->nWaiting == 0);
    b->nProcs = 0;
    MutexDestroy (&(b->lock));
    CondDestroy (&(b->wait));
}

#endif /* HAVE_PTHREAD_BARRIER */

#endif /* !_OS_THREADS_H_ */
