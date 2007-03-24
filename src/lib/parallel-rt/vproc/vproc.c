/* vproc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include <stdio.h>
#include <signal.h>
#include <ucontext.h>
#if defined (OPSYS_DARWIN)
#  include <CoreServices/CoreServices.h>	/* for MPProcessors */
#endif
#include "os-memory.h"
#include "os-threads.h"
#include "vproc.h"
#include "heap.h"
#include "gc.h"
#include "options.h"
#include "value.h"

typedef struct {		/* data passed to VProcMain */
    VProc_t	*vp;		/* the host vproc */
    VProcFn_t	initFn;	/* the initial function to run */
    void	*arg;		/* an additional argument to initFn */
    bool	started;	/* used to signal that the vproc has started */
    Mutex_t	lock;		/* lock to protect wait and started */
    Cond_t	wait;		/* use to wait for the vproc to start */
} InitData_t;

static void *VProcMain (void *_data);
static void IdleVProc (VProc_t *vp, void *arg);
static void SigHandler (int sig, siginfo_t *si, void *_sc);
static int GetNumCPUs ();

static pthread_key_t	VProcInfoKey;

/********** Globals **********/
int			NumHardwareProcs;
int			NumVProcs;
VProc_t			*VProcs[MAX_NUM_VPROCS];


/* VProcInit:
 *
 * Initialization for the VProc management system.
 */
void VProcInit (Options_t *opts)
{
    NumHardwareProcs = GetNumCPUs();

  /* get command-line options */
    int nProcs = ((NumHardwareProcs == 0) ? DFLT_NUM_VPROCS : NumHardwareProcs);
    nProcs = GetIntOpt (opts, "-p", nProcs);
    if ((NumHardwareProcs > 0) && (nProcs > NumHardwareProcs))
	Warning ("%d processors requested on a %d processor machine\n",
	    nProcs, NumHardwareProcs);

#ifndef NDEBUG
    SayDebug("%d/%d processors allocated to vprocs\n", nProcs, NumHardwareProcs);
#endif

    if (pthread_key_create (&VProcInfoKey, 0) != 0) {
	Die ("unable to create VProcInfoKey");
    }

  /* allocate nProcs-1 idle vprocs; the last vproc will be created to run
   * the initial Manticore thread.
   */
    for (int i = 1;  i < nProcs;  i++) {
	VProcCreate (IdleVProc, 0);
    }

} /* end of VProcInit */


/* VProcCreate:
 *
 * Create the data structures and underlying system thread to
 * implement a vproc.
 */
VProc_t *VProcCreate (VProcFn_t f, void *arg)
{
    if (NumVProcs >= MAX_NUM_VPROCS)
	Die ("too many vprocs\n");

#ifdef HAVE_PTHREAD_SETAFFINITY_NP
    cpu_set_t	cpus;
    CPU_ZERO(&cpus);
    CPU_SET(NumVProcs, &cpus);
    if (pthread_setaffinity_np(pthread_self(), sizeof(cpu_set_t), &cpus) == -1) {
	Warning("[%2d] unable to set affinity\n", NumVProcs);
    }
#endif

  /* allocate the VProc heap; we store the VProc representation in the base
   * of the heap area.
   */
    int nBlocks = 1;
    VProc_t *vproc = (VProc_t *)AllocMemory (&nBlocks, VP_HEAP_SZB);
    if ((vproc == 0) || (nBlocks != 1))
	Die ("unable to allocate vproc heap");

  /* initialize the vproc structure */
    vproc->inManticore = M_FALSE;
    vproc->atomic = M_FALSE;
    vproc->sigPending = M_FALSE;
    vproc->actionStk = M_NIL;	    /* FIXME: install default action? */
    vproc->rdyQHd = M_NIL;
    vproc->rdyQTl = M_NIL;
    vproc->stdArg = M_UNIT;
    vproc->stdEnvPtr = M_UNIT;
    vproc->stdCont = M_UNIT;
    vproc->stdExnCont = M_UNIT;
    vproc->limitPtr = (Addr_t)vproc + VP_HEAP_SZB - ALLOC_BUF_SZB;
    vproc->oldTop = VProcHeap(vproc);
    SetAllocPtr (vproc);
    MutexInit (&(vproc->lock));
    CondInit (&(vproc->wait));
    vproc->idle = true;

    vproc->id = NumVProcs;
    VProcs[NumVProcs++] = vproc;

    InitVProcHeap (vproc);

  /* start the vproc's pthread */
    InitData_t data;
    data.vp = vproc;
    data.initFn = f;
    data.arg = arg;
    data.started = false;
    MutexInit(&(data.lock));
    CondInit(&(data.wait));
    ThreadCreate (&(vproc->hostID), VProcMain, &data);

  /* wait until the vproc has started, since data is in use until
   * then.
   */
    MutexLock (&(data.lock));
	while (! data.started)
	    CondWait (&(data.wait), &(data.lock));
    MutexUnlock (&(data.lock));

    return vproc;

} /* VProcCreate */


/*! \brief return a pointer to the VProc that the caller is running on.
 *  \return the VProc that the caller is running on.
 */
VProc_t *VProcSelf ()
{
    return (VProc_t *)pthread_getspecific (VProcInfoKey);

} /* VProcSelf */


/*! \brief send an asynchronous signal to another VProc.
 *  \param vp the target VProc.
 *  \param sig the signal.
 */
void VProcSignal (VProc_t *vp, VPSignal_t sig)
{
    if (sig == GCSignal) pthread_kill (vp->hostID, SIGUSR1);
    else if (sig == PreemptSignal) pthread_kill (vp->hostID, SIGUSR2);
    else Die("bogus signal");

} /* end of VProcSignal */

/*! \brief put an idle vproc to sleep.
 *  \param vp the vproc that is being put to sleep.
 */
void VProcSleep (VProc_t *vp)
{
    assert (vp == VProcSelf());

    MutexLock (&(vp->lock));
	vp->idle = true;
	while (vp->idle) {
	    CondWait (&(vp->wait), &(vp->lock));
	}
    MutexUnlock (&(vp->lock));

}

/* VProcMain:
 */
static void *VProcMain (void *_data)
{
    InitData_t		*data = (InitData_t *)_data;
    VProc_t		*self = data->vp;
    VProcFn_t		init = data->initFn;
    void		*arg = data->arg;
    struct sigaction	sa;

#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] VProcMain: initializing ...\n", self->id);
#endif

  /* store a pointer to the VProc info as thread-specific data */
    pthread_setspecific (VProcInfoKey, self);

  /* initialize this pthread's handler. */
    sa.sa_sigaction = SigHandler;
    sa.sa_flags = SA_SIGINFO | SA_RESTART;
    sigfillset (&(sa.sa_mask));
    sigaction (SIGUSR1, &sa, 0);
    sigaction (SIGUSR2, &sa, 0);

  /* signal that we have started */
    MutexLock (&(data->lock));
	data->started = true;
	CondSignal (&(data->wait));
    MutexUnlock (&data->lock);

    self->idle = false;

    init (self, arg);

} /* VProcMain */

/*! \brief enqueue a (fiber, thread ID) pair on another vproc's ready queue
 *  \param self the calling vproc
 *  \param vp the vproc to enqueue the thread on.
 *  \param fiber the fiber to be enqueued.
 *  \param 
 */
void EnqueueOnVProc (VProc_t *self, VProc_t *vp, Value_t *fiber, Value_t *tid)
{
    MutexLock (&(vp->lock));
/***** where do we put the object? *****/
        if (vp->idle) {
	    vp->idle = false;
	    CondSignal (&(vp->wait));
	}
    MutexUnlock (&(vp->lock));

} /* end of EnqueueOnVProc */

/*! \brief dequeue a fiber from the secondary scheduling queue or else go idle.
 *  \param self the calling vproc
 */
Value_t VProcDequeue (VProc_t *self)
{
#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] VProcDequeue called\n", self->id);
#endif

    MutexLock (&(self->lock));
	self->idle = false;
	while (self->idle)
	    CondWait (&(self->wait), &(self->lock));
    MutexUnlock (&(self->lock));

} /* end of VProcDequeue */

/* IdleVProc:
 */
static void IdleVProc (VProc_t *vp, void *arg)
{
#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] IdleVProc starting\n", vp->id);
#endif

    MutexLock (&(vp->lock));
	while (vp->idle)
	    CondWait (&(vp->wait), &(vp->lock));
    MutexUnlock (&(vp->lock));

  /* Here, we expect that there will be a thread to run on the vproc's
   * ready queue soon.
   */
    /* ??? */

}

/* SigHandler:
 *
 * A per-vproc handler for SIGUSR1 and SIGUSR2 signals.
 */
static void SigHandler (int sig, siginfo_t *si, void *_sc)
{
    ucontext_t	*uc = (ucontext_t *)_sc;
    VProc_t	*self = VProcSelf();

SayDebug("[%2d] inManticore = %p, atomic = %p\n", self->id, self->inManticore, self->atomic);
    self->sigPending = M_TRUE;
    if ((self->inManticore == M_TRUE) && (self->atomic == M_FALSE)) {
      /* set the limit pointer to zero to force a context switch on
       * the next GC test.
       */
	uc->uc_mcontext.gregs[REG_R11] = 0;
    }

} /* SignHandler */

static int GetNumCPUs ()
{
#if defined(HAVE__PROC_CPUINFO)
  /* Get the number of hardware processors on systems that have /proc/cpuinfo */
    FILE *cpuinfo = fopen("/proc/cpuinfo", "r");
    char buf[1024];
    if (cpuinfo != NULL) {
	int n = 0;
	while (fgets(buf, sizeof(buf), cpuinfo) != 0) {
	    int id;
	    if (sscanf(buf, "processor : %d", &id) == 1)
		n++;
	}
	fclose (cpuinfo);
	return n;
    }
#elif defined(OPSYS_DARWIN)
    return MPProcessors ();
#else
    return 0;
#endif

} /* end of GetNumCPUs */
