/* vproc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include <stdio.h>
#include <signal.h>
#include <ucontext.h>
#if defined (TARGET_DARWIN)
#  include <sys/sysctl.h>
#endif
#include "os-memory.h"
#include "os-threads.h"
#include "atomic-ops.h"
#include "vproc.h"
#include "heap.h"
#include "gc.h"
#include "options.h"
#include "value.h"
#include "scheduler.h"
#include "inline-log.h"

typedef struct {		/* data passed to VProcMain */
    VProc_t	*vp;		/* the host vproc */
    VProcFn_t	initFn;		/* the initial function to run */
    void	*arg;		/* an additional argument to initFn */
    bool	started;	/* used to signal that the vproc has started */
    Mutex_t	lock;		/* lock to protect wait and started */
    Cond_t	wait;		/* use to wait for the vproc to start */
} InitData_t;

static void *VProcMain (void *_data);
static void IdleVProc (VProc_t *vp, void *arg);
static void SigHandler (int sig, siginfo_t *si, void *_sc);
static int GetNumCPUs ();
static Value_t Dequeue2 (VProc_t *self);

static pthread_key_t	VProcInfoKey;

/********** Globals **********/
int			NumHardwareProcs;
int			NumVProcs;
int			NumIdleVProcs;
VProc_t			*VProcs[MAX_NUM_VPROCS];
int			NextVProc;			/* index of next slot in VProcs */
int                     FiberIdCounter = 0;                   /* fiber id counter */

extern int ASM_VProcSleep;

/* Macros for accessing the register fields in the ucontext */
#if defined(TARGET_LINUX)
#  define UC_R11(uc)	((uc)->uc_mcontext.gregs[REG_R11])
#  define UC_RIP(uc)	((uc)->uc_mcontext.gregs[REG_RIP])
#elif defined(TARGET_DARWIN)
#  if defined(__DARWIN_UNIX03) && defined(_DARWIN_FEATURE_UNIX_CONFORMANCE)
#    define UC_R11(uc)	((uc)->uc_mcontext->__ss.__r11)
#    define UC_RIP(uc)	((uc)->uc_mcontext->__ss.__rip)
#  else /* Tiger */
#    define UC_R11(uc)	((uc)->uc_mcontext->ss.r11)
#    define UC_RIP(uc)	((uc)->uc_mcontext->ss.rip)
#  endif
#else
#  error unsupported OS
#endif

/* VProcInit:
 *
 * Initialization for the VProc management system.
 */
void VProcInit (Options_t *opts)
{
    NumHardwareProcs = GetNumCPUs();
    NumIdleVProcs = 0;

  /* get command-line options */
    NumVProcs = ((NumHardwareProcs == 0) ? DFLT_NUM_VPROCS : NumHardwareProcs);
    NumVProcs = GetIntOpt (opts, "-p", NumVProcs);
    if ((NumHardwareProcs > 0) && (NumVProcs > NumHardwareProcs))
	Warning ("%d processors requested on a %d processor machine\n",
	    NumVProcs, NumHardwareProcs);

#ifndef NDEBUG
    SayDebug("%d/%d processors allocated to vprocs\n", NumVProcs, NumHardwareProcs);
#endif

#ifdef ENABLE_LOGGING
  /* initialize the log file */
    const char *logFile = GetStringOpt(opts, "-log", DFLT_LOG_FILE);
    InitLogFile (logFile, NumVProcs, NumHardwareProcs);
#endif

    if (pthread_key_create (&VProcInfoKey, 0) != 0) {
	Die ("unable to create VProcInfoKey");
    }

/* FIXME: we should allocate and initialize the vproc objects in the pthreads so
 * that we get the right memory affinity on NUMA machines.
 */
    /* allocate memory and initialize locks and condition variables for all vprocs.
     */
    for (int i = 0;  i < NumVProcs;  i++) {
      /* allocate the VProc heap; we store the VProc representation in the base
       * of the heap area.
       */
	int nBlocks = 1;
	VProc_t *vproc = (VProc_t *)AllocMemory (&nBlocks, VP_HEAP_SZB);
	if ((vproc == 0) || (nBlocks != 1))
	    Die ("unable to allocate vproc heap");
	
	VProcs[i] = vproc;
	MutexInit (&(vproc->lock));
	CondInit (&(vproc->wait));
    }

  /* create nProcs-1 idle vprocs; the other vproc will run the initial Manticore 
   * thread.
   */
    for (int i = 1;  i < NumVProcs;  i++) {
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

    if (NextVProc > MAX_NUM_VPROCS) {
	Die ("Allocated too many VProcs");
    }
  
    VProc_t* vproc = VProcs[NextVProc];

  /* initialize the vproc structure */
    vproc->id = NextVProc++;

    vproc->oldTop = VProcHeap(vproc);
    InitVProcHeap (vproc);

    vproc->inManticore = M_FALSE;
    vproc->atomic = M_TRUE;
    vproc->sigPending = M_FALSE;
    vproc->actionStk = M_NIL;
    vproc->schedCont = M_NIL;
    vproc->rdyQHd = M_NIL;
    vproc->rdyQTl = M_NIL;
    vproc->entryQ = M_NIL;
    vproc->secondaryQHd = M_NIL;
    vproc->secondaryQTl = M_NIL;
    vproc->stdArg = M_UNIT;
    vproc->stdEnvPtr = M_UNIT;
    vproc->stdCont = M_NIL;
    vproc->stdExnCont = M_UNIT;
    vproc->limitPtr = (Addr_t)vproc + VP_HEAP_SZB - ALLOC_BUF_SZB;
    SetAllocPtr (vproc);
    vproc->idle = true;

#ifdef ENABLE_LOGGING
    InitLog (vproc);
#endif

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

void VProcPreempt (VProc_t *vp)
{
    VProcSignal (vp, PreemptSignal);
}

/*! \brief send an asynchronous signal to another VProc.
 *  \param vp the target VProc.
 *  \param sig the signal.
 */
void VProcSignal (VProc_t *vp, VPSignal_t sig)
{
#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] VProcSignal: sig = %d\n", vp->id, sig);
#endif

    if (sig == PreemptSignal) pthread_kill (vp->hostID, SIGUSR1);
    else if (sig == GCSignal) pthread_kill (vp->hostID, SIGUSR2);
    else Die("bogus signal");

} /* end of VProcSignal */


/*! \brief put the vproc to sleep until a signal arrives
 *  \param vp the vproc that is being put to sleep.
 */
void VProcWaitForSignal (VProc_t *vp)
{
   assert (vp == VProcSelf());

    LogEvent0 (vp, VProcSleepEvt);

#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] VProcWaitForSignal called\n", vp->id);
#endif

    if (FetchAndInc(&NumIdleVProcs) == NumVProcs-1) {
      /* all VProcs are idle, so shutdown */
#ifndef NDEBUG
	if (DebugFlg)
	    SayDebug("[%2d] shuting down\n", vp->id);
#endif
	exit (0);
    }
    else {
        vp->idle = true;
	MutexLock (&(vp->lock));
	    while (vp->entryQ == M_NIL) {
		CondWait (&(vp->wait), &(vp->lock));
	    }
	vp->idle = false;

#ifndef NDEBUG
        if (DebugFlg)
            SayDebug("[%2d] VProcWaitForSignal: waking up; entryQ = %p\n", vp->id, ValueToRdyQItem(vp->entryQ));
#endif
	MutexUnlock (&(vp->lock));
	FetchAndDec(&NumIdleVProcs);
    }

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

#ifdef HAVE_PTHREAD_SETAFFINITY_NP 
    cpu_set_t	cpus;
    CPU_ZERO(&cpus);
    CPU_SET(self->id, &cpus);
    if (pthread_setaffinity_np(pthread_self(), sizeof(cpu_set_t), &cpus) == -1) {
	Warning("[%2d] unable to set affinity\n", NumVProcs);
    }
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

    Die("VProcMain returning.\n");

} /* VProcMain */

/*! \brief return a list of the vprocs in the system.
 *  \param self the host vproc
 */
Value_t ListVProcs (VProc_t *self)
{
    Value_t	l = M_NIL;

    for (int i = 0;  i < NumVProcs;  i++) {
	l = Cons(self, PtrToValue(VProcs[i]), l);
    }

    return l;

}

/*! \brief return a pointer to the nth vproc
 *  \param n vproc id
 */
VProc_t* GetNthVProc (int n)
{
    assert(n >= 0 && n < NumVProcs);
    return VProcs[n];
}

/*! \brief Wake a vproc.
 *  \param the vproc to wake
 */
void WakeVProc (VProc_t *vp)
{
#ifndef NDEBUG
        if (DebugFlg)
	  SayDebug("[%2d] WakeVProc: waking up vp %d\n", VProcSelf()->id, vp->id);
#endif
  CondSignal (&(vp->wait));
}

/*! \brief create a fiber that puts the vproc to sleep
 *  \param self the calling vproc
 *  \return fiber that when run puts the vproc to sleep
 */
Value_t SleepCont (VProc_t *self)
{
    return AllocUniform(self, 1, PtrToValue(&ASM_VProcSleep));
}

/* IdleVProc:
 */
static void IdleVProc (VProc_t *vp, void *arg)
{
    LogEvent0 (vp, VProcStartIdleEvt);

#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] IdleVProc starting\n", vp->id);
#endif

  /* Run code that will immediately put this VProc to sleep. */
    RunManticore (vp, (Addr_t)&ASM_VProcSleep, M_UNIT, M_UNIT);
  /* Run code that will activate the VProc. */
    Value_t envP = vp->schedCont;
    Addr_t codeP = ValueToAddr(ValueToCont(envP)->cp);
    RunManticore (vp, codeP, M_NIL, envP);

#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] return from RunManticore in idle vproc\n", vp->id);
#endif
    exit (0);

} /* end of IdleVProc */

/* SigHandler:
 *
 * A per-vproc handler for SIGUSR1 and SIGUSR2 signals.
 */
static void SigHandler (int sig, siginfo_t *si, void *_sc)
{
    ucontext_t	*uc = (ucontext_t *)_sc;
    VProc_t	*self = VProcSelf();

  /* WARNING:
   * Enabling the following SayDebug can cause deadlock;
   * if the signal arrives while the VProc/pthread is in the runtime,
   * the PrintLock may already be acquired by this thread for debugging. 
   * Attempting to re-acquire the PrintLock in the signal handler leads to deadlock
   * (technically, undefined behavior, but deadlock in practice).
   */
/*
#ifndef NDEBUG
    if (DebugFlg)
        SayDebug("[%2d] SigHandler; inManticore = %p, atomic = %p, pc = %p\n",
                 self->id, self->inManticore, self->atomic, UC_RIP(uc));
#endif
*/

    switch (sig) {
      case SIGUSR1: /* Preemption signal */
	LogEvent0 (self, PreemptSignalEvt);
	self->sigPending = M_TRUE;
	break;
      case SIGUSR2: /* Global GC signal */
	LogEvent0 (self, GCSignalEvt);
	self->sigPending = M_TRUE;
	self->globalGCPending = true;
	break;
      default:
	Die ("bogus signal %d\n", sig);
	break;
    }

    if ((self->inManticore == M_TRUE) && (self->atomic == M_FALSE)) {
      /* set the limit pointer to zero to force a context switch on
       * the next GC test.
       */
	UC_R11(uc) = 0;
    }

} /* SigHandler */


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
    else {
	Warning("unable to determine the number of processors\n");
	return 0;
    }
#elif defined(TARGET_DARWIN)
    int		numCPUs;
    size_t	len = sizeof(int);
    if (sysctlbyname("hw.activecpu", &numCPUs, &len, 0, 0) < 0) {
	Warning("unable to determine the number of processors\n");
	return 0;
    }
    else
	return numCPUs;
#else
    return 0;
#endif

} /* end of GetNumCPUs */


int GetNumVProcs ()
{
  return NumVProcs;
}

