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
#include "timer.h"

#ifndef NO_GC_STATS
typedef struct {	    //!< counters for a GC
    uint64_t	nBytesAlloc;	//!< number of bytes allocated in the region being
				//!  collected; this region is defined as follows:
				//!    - minorGC --- nursery part of local heap
				//!    - majorGC --- old part of local heap
				//!    - globalGC --- global heap
    uint64_t	nBytesCollected;//!< the number of byte (either allocated or
				//!  copied by a previous GC) in the region
				//!  being collected.
    uint64_t	nBytesCopied;	//!< number of live bytes copied by the GC from
				//!  the region being collected.
    Timer_t	timer;		//!< used to track time spent in GC
} GCCntrs_t;
#endif

#ifdef ENABLE_PERF_COUNTERS
typedef struct {    //!< a perf counter
    uint64_t    nonGC;//!< nonGC perf counter value
    uint64_t    GC;    //!< perf counter value during gc
    uint64_t    last;   //!< previous value of the counter at last state change
    bool        inGC;   //!< true if we're currently recording GC-specific data
    int         fd;     //!< file descriptor associated with perf counter
    bool        enabled;//!< true if perf counters are enabled for this vproc
} PerfCntrs_t;
#endif

/* WARNING:
 * Changing the vproc struct might require modifying ../config/vproc-offsets-ins.c.
 */
struct struct_vproc {
  /* the following fields are only accessed by the local vproc */
    Value_t	atomic;		//!< true, when in a vproc-atomic region
    Value_t	sigPending;	//!< true, when there is a pending signal
    Value_t	sleeping;       //!< true, when the vproc is sleeping
    Value_t	currentFLS;	//!< the current fiber's local storage
    Value_t	actionStk;	//!< the top of the signal-action stack
    Value_t	schedCont;      //!< continuation that invokes the current scheduler
    Value_t	dummyK;         //!< trivial fiber that immediate terminates
    Value_t	wakeupCont;     //!< continuation that wakes the vproc
    Value_t	shutdownCont;   //!< continuation used by vprocs to shutdown the runtime
    Value_t	shutdownPending; //!< true, when runtime shutdown is pending
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
    Addr_t      heapBase;       //!< Base address of the vproc heap
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
                                //!< heap-limit pointer. this field plays the
                                //!< additional role of signaling asynchronous events,
                                //!< which is the reason why the field is shared.
    bool	globalGCPending __attribute__((aligned(64)));
                                //!< true when this vproc has been signaled that
				//! global GC has started, but it has not
				//! started yet.

  /* additional optional fields used for stats etc. */
    Timer_t	timer;		//!< tracks the execution time of this vproc
			      /* GC stats */
#ifndef NO_GC_STATS
    uint32_t	nPromotes;	//!< number of promotions
    uint32_t	nMinorGCs;	//!< number of minor GCs by this vproc
    uint32_t	nMajorGCs;	//!< number of major GCs by this vproc
    GCCntrs_t	minorStats;	//!< information about minor GCs on this vproc
    GCCntrs_t	majorStats;	//!< information about major GCs on this vproc
    GCCntrs_t	globalStats;	//!< information about this vproc's part in
				//!  global GCs.
    uint64_t	nBytesPromoted;	//!< the number of bytes promoted on this vproc
    Timer_t	promoteTimer;	//!< used to track time taken by promotions
#endif
#ifndef ENABLE_LOGGING	      /* GC counters for logging info */

#endif
#ifdef ENABLE_PERF_COUNTERS
    PerfCntrs_t misses;//!< L3 read miss perf counter
    PerfCntrs_t reads;//!< L3 read perf counter
#endif
	
    int proxyTableentries;
    int maxProxy;
    Value_t * proxyTable;
};

/* the type of the initial function to run in a vproc */
typedef void (*VProcFn_t) (VProc_t *vp, void *arg);

/* the array of vprocs */
extern int		NumVProcs;
extern VProc_t		*VProcs[MAX_NUM_VPROCS];
extern bool		ShutdownFlg;

extern void VProcInit (bool isSequential, Options_t *opts);
extern VProc_t *VProcCreate (VProcFn_t f, void *arg);
extern VProc_t *VProcSelf ();
extern void VProcPreempt (VProc_t *self, VProc_t *vp);
extern void VProcSendSignal (VProc_t *self, VProc_t *vp, Value_t k, Value_t fls);
extern void VProcSleep (VProc_t *vp);
extern void VProcGlobalGCInterrupt (VProc_t *self, VProc_t *vp);
extern Value_t VProcNanosleep (VProc_t *vp, Time_t nsec);

#endif /* !_VPROC_H_ */
