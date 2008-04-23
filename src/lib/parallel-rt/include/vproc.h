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

/* WARNING:
 * Changing the vproc struct might require modifying ../config/vproc-offsets-ins.c.
 */
struct struct_vproc {
    Value_t	inManticore;	/*!< true, when executing Manticore code */
    Value_t	atomic;		/*!< true, when in a vproc-atomic region */
    Value_t	sigPending;	/*!< true, when there is a pending signal */
    Value_t	currentFG;	/*!< fiber group of the current thread */
    Value_t	actionStk;	/*!< the top of the signal-action stack */
    Value_t     schedCont;      /*!< continuation that invokes the current scheduler */
    Value_t	rdyQHd;		/*!< the head of the primary ready queue */
    Value_t	rdyQTl;		/*!< the tail of the primary ready queue */
    Value_t     entryQ;         /*!< the head of the entry queue (stack) for the vproc */
    Value_t	secondaryQHd;	/*!< the head of the secondary ready queue */
    Value_t	secondaryQTl;	/*!< the tail of the secondary ready queue */
			      /* VProc registers */
    Value_t	stdArg;		/*!< holds value of standard argument reg. */
    Value_t	stdEnvPtr;	/*!< holds value of standard environment-pointer reg. */
    Value_t	stdCont;	/*!< holds value of standard return-cont. reg. */
    Value_t	stdExnCont;	/*!< holds value of standard exception-cont. reg. */
    Addr_t	allocPtr;	/*!< allocation pointer */
    Addr_t	limitPtr;	/*!< heap-limit pointer */
			      /* logging support */
    LogBuffer_t	*log;	        /*!< current buffer for logging events */
    LogBuffer_t	*prevLog;       /*!< previous buffer for logging events */
			      /* GC parameters */
    Addr_t	nurseryBase;	/*!< Base address of current nursery area */
    Addr_t	oldTop;		/*!< Old objects live in the space from the */
				/* heap base to the oldTop. */
    MemChunk_t	*globToSpHd;	/*!< pointer to the head of the list of global-heap */
				/* to-space memory chunks allocated by this vproc. */
    MemChunk_t	*globToSpTl;	/*!< pointer to the tail of the list of global-heap */
				/* to-space memory chunks allocated by this vproc. */
				/* This chunk is the current allocation chunk for */
				/* the vproc. */
    Addr_t	globNextW;	/*!< pointer to next word to allocate in */
				/* global heap */
    Addr_t	globLimit;	/*!< limit pointer for to-space chunk */
    bool	globalGCPending; /*!< true when the vproc has been signaled that */
				/* global GC has started, but this vproc has not */
				/* started yet. */
    bool	globalGCInProgress; /*!< true when this vproc has started global collection */
    int		id;	      /*!< index of this vproc in VProcs[] array */
    OSThread_t	hostID;	      /*!< PThread ID of host */
    Mutex_t	lock;	      /*!< lock for VProc state */
    Cond_t	wait;	      /*!< for waiting when idle */
    bool	idle;	      /*!< true when the VProc is idle */
			      /* GC stats */
#ifndef NO_GC_STATS
    int32_t	nLocalPtrs;
    int32_t	nGlobPtrs;
#endif
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
extern void VProcPushEntries (VProc_t *self, Value_t entries);
extern Value_t VProcGetEntryQ (VProc_t *vp);

#endif /* !_VPROC_H_ */
