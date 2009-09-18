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
  /* the following fields are only accessed by the local vproc */
    Value_t	inManticore;	//!< true, when executing Manticore code
    Value_t	atomic;		//!< true, when in a vproc-atomic region
    Value_t	sigPending;	//!< true, when there is a pending signal
    Value_t	sleeping;       //!< true, when the vproc is sleeping
    Value_t	currentFLS;	//!< the current fiber's local storage
    Value_t	actionStk;	//!< the top of the signal-action stack
    Value_t     schedCont;      //!< continuation that invokes the current scheduler
    Value_t     dummyK;         //!< trivial fiber that immediate terminates
    Value_t     wakeupCont;     //!< continuation that wakes the vproc
    Value_t	rdyQHd;		//!< the head of the primary ready queue
    Value_t	rdyQTl;		//!< the tail of the primary ready queue
			      /* VProc registers */
    Value_t	stdArg;		//!< holds value of standard argument reg.
    Value_t	stdEnvPtr;	//!< holds value of standard environment-pointer reg.
    Value_t	stdCont;	//!< holds value of standard return-cont. reg.
    Value_t	stdExnCont;	//!< holds value of standard exception-cont. reg.
    Addr_t	allocPtr;	//!< allocation pointer
			      /* logging support */
/* NOTE: these volatile annotations are not required for the SWP branch */
    volatile uint64_t
		eventId;	//!< counter for generating event IDs; the
				//!  top 8 bits of the counter hold the VProc ID.
    volatile LogBuffer_t
		*log;		//!< current buffer for logging events
    LogBuffer_t	*prevLog;       //!< previous buffer for logging events
#ifdef HAVE_AIO_RETURN
    struct aiocb *logCB;	//!< AIO control buffer for log file
#endif
			      /* GC parameters */
    Addr_t	nurseryBase;	//!< Base address of current nursery area
    Addr_t	oldTop;		//!< Old objects live in the space from the
				//! heap base to the oldTop.
    MemChunk_t	*globToSpHd;	//!< pointer to the head of the list of global-heap
				//! to-space memory chunks allocated by this vproc.
    MemChunk_t	*globToSpTl;	//!< pointer to the tail of the list of global-heap
				//! to-space memory chunks allocated by this vproc.
				//! This chunk is the current allocation chunk for
				//! the vproc.
    Addr_t	globNextW;	//!< pointer to next word to allocate in
				//! global heap
    Addr_t	globLimit;	//!< limit pointer for to-space chunk
    int		id;		//!< index of this vproc in VProcs[] array
    OSThread_t	hostID;		//!< PThread ID of host
    Location_t	location;	//!< the physical location that hosts this vproc.

  /* the following fields may be changed by remote vprocs */
    Mutex_t	lock;		//!< lock for VProc state
    Cond_t	wait;		//!< for waiting when idle
    Value_t     landingPad __attribute__((aligned(64)));
                                //!< the head of the landing pad (stack)
    Addr_t	limitPtr __attribute__((aligned(64)));	
                                 //!< heap-limit pointer
    bool	globalGCPending __attribute__((aligned(64))); 
                                //!< true when this vproc has been signaled that
				//! global GC has started, but it has not
				//! started yet.

  /* additional optional fields used for stats etc. */
			      /* GC stats */
#ifndef NO_GC_STATS
    int32_t	nLocalPtrs;	//!< counter of pointers into local heap that
				//!  are scanned in minor GC
    int32_t	nGlobPtrs;	//!< counter of pointers into global heap that
				//!  are scanned in minor GC
    uint64_t	nWordsScanned;	//!< counter of words scanned by this vpro
				//! c during global GC
    uint64_t	nBytesCopied;	//!< counter of bytes copied by this vproc
				//!  during global GC
#endif
#ifndef ENABLE_LOGGING	      /* GC counters for logging info */

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
extern int		NumVProcs;
extern VProc_t		*VProcs[MAX_NUM_VPROCS];

extern void VProcInit (bool isSequential, Options_t *opts);
extern VProc_t *VProcCreate (VProcFn_t f, void *arg);
extern VProc_t *VProcSelf ();
void VProcSendUnixSignal (VProc_t *vp, VPSignal_t sig);
void VProcPreempt (VProc_t *self, VProc_t *vp);
void VProcSendSignal (VProc_t *self, VProc_t *vp, Value_t k, Value_t fls);
void VProcSleep (VProc_t *vp);
void VProcGlobalGCInterrupt (VProc_t *self, VProc_t *vp);

#endif /* !_VPROC_H_ */
