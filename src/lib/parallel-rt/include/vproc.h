/* vproc.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _VPROC_H_
#define _VPROC_H_

#include <pthread.h>
#include "manticore-rt.h"
#include "os-threads.h"

struct struct_vproc {
    Value_t	inManticore;	/*!< true, when executing Manticore code */
    Value_t	atomic;		/*!< true, when in a vproc-atomic region */
    Value_t	sigPending;	/*!< true, when there is a pending signal */
    Value_t	actionStk;	/*!< the top of the signal-action stack */
    Value_t	rdyQHd;		/*!< the head of the primary ready queue */
    Value_t	rdyQTl;		/*!< the head of the primary ready queue */
			      /* VProc registers */
    Value_t	stdArg;		/*!< holds value of standard argument reg. */
    Value_t	stdEnvPtr;	/*!< holds value of standard environment-pointer reg. */
    Value_t	stdCont;	/*!< holds value of standard return-cont. reg. */
    Value_t	stdExnCont;	/*!< holds value of standard exception-cont. reg. */
    Addr_t	allocPtr;	/*!< allocation pointer */
    Addr_t	limitPtr;	/*!< heap-limit pointer */
			      /* GC parameters */
    Addr_t	nurseryBase;	/*!< Base address of current nursery area */
    Addr_t	oldTop;		/*!< Old objects live in the space from the */
				/* heap base to the oldTop. */
    MemChunk_t	*globToSpace;	/*!< a to-space chunk in the global heap to promote */
				/* objects into. */
    Addr_t	globNextW;	/*!< pointer to next word to allocate in */
				/* global heap */
    Addr_t	globLimit;	/*!< limit pointer for global heap */
    int		id;	      /*!< index of this vproc in VProcs[] array */
    OSThread_t	hostID;	      /*!< PThread ID of host */
    Mutex_t	lock;	      /*!< lock for VProc state */
    Cond_t	wait;	      /*!< for waiting when idle */
    bool	idle;	      /*!< true when the VProc is idle */
};

typedef enum {
    GCSignal,
    PreemptSignal
} VPSignal_t;

/* the type of the initial function to run in a vproc */
typedef void (*VProcFn_t) (VProc_t *vp, void *arg);

/* Return the base address of the VProc's heap */
STATIC_INLINE Addr_t VProcHeap (VProc_t *vp)
{
    return ((Addr_t)vp) + sizeof(VProc_t);
}

/* the array of vprocs */
extern int		NumHardwareProcs;	// number of hardware processors to use.
extern int		NumVProcs;
extern VProc_t		*VProcs[MAX_NUM_VPROCS];

extern void VProcInit (Options_t *opts);
extern VProc_t *VProcCreate (VProcFn_t f, void *arg);
extern VProc_t *VProcSelf ();
extern void VProcSignal (VProc_t *vp, VPSignal_t sig);
extern void VProcSleep (VProc_t *vp);

#endif /* !_VPROC_H_ */
