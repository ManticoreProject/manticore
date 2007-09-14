/* main.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include <stdio.h>
#include <time.h>
#include <signal.h>
#include <stdarg.h>
#include "options.h"
#include "value.h"
#include "vproc.h"
#include "heap.h"
#include "os-threads.h"
#include "asm-offsets.h"

static void MainVProc (VProc_t *vp, void *arg);
static void PingLoop ();
static void Ping (int n);
#ifndef HAVE_SIGTIMEDWAIT
static void SigHandler (int sig, siginfo_t *si, void *uc);
#endif

#define MIN_TIMEQ_NS	1000000		/* minimum timeq in nanoseconds (== 1ms) */

static int	TimeQ;			/* time quantum in milliseconds */
#ifndef NDEBUG
static FILE	*DebugF = NULL;
bool		DebugFlg = false;
#endif
static Mutex_t	PrintLock;		/* lock for output routines */

extern int mantEntry;			/* the entry-point of the Manticore code */
extern Int32_t mantMagic;


int main (int argc, const char **argv)
{
    Options_t *opts = InitOptions (argc, argv);

    MutexInit (&PrintLock);

#ifndef NDEBUG
  /* initialize debug output */
    DebugF = stdout;
    DebugFlg = GetFlagOpt (opts, "-d");
#endif

    if (mantMagic != RUNTIME_MAGIC) {
	Die("runtime/compiler inconsistency\n");
    }

    HeapInit (opts);
    VProcInit (opts);

  /* get the time quantum in milliseconds */
    TimeQ = GetIntOpt(opts, "-q", DFLT_TIME_Q_MS);

/* FIXME: for testing purposes, we pass an integer argument to the Manticore code */
    int arg = GetIntOpt(opts, "-a", 1);

  /* create the main vproc */
    VProcCreate (MainVProc, (void *)(Addr_t)arg);

    PingLoop();

} /* end of main */


/* MainVProc:
 *
 * The main vproc is responsible for running the Manticore code.  The
 * argument is the address of the initial entry-point in Manticore program.
 *
 * FIXME: right now, the argument is an integer argument to the program.
 */ 
static void MainVProc (VProc_t *vp, void *arg)
{
#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] MainVProc starting\n", vp->id);
#endif

    Value_t argV = WrapInt(vp, (int)(Addr_t)arg); /* FIXME: for testing purposes */

    Say("arg = ");
    SayValue (argV);
    Say("\n");

    FunClosure_t fn = {.cp = PtrToValue(&mantEntry), .ep = M_UNIT};
    Value_t resV = ApplyFun (vp, PtrToValue(&fn), argV);

    Say("res = ");
    SayValue (resV);
    Say("\n");

    exit (0);

}


/* PingLoop:
 */
static void PingLoop ()
{
    struct timespec	tq;

  /* compute interval for preempting the vprocs */
    long ns = 1000000 * (long)TimeQ;
    int nPings = 1;
    while (((nPings * ns) / NumVProcs < MIN_TIMEQ_NS) && (nPings < NumVProcs)) {
	nPings++;
    }

    long nsec = ns / NumVProcs;
    tq.tv_sec = 0;
    while (nsec >= 1000000000) {
	nsec -= 1000000000;
	tq.tv_sec++;
    }
    tq.tv_nsec = nsec / NumVProcs;

#if defined(HAVE_SIGTIMEDWAIT)
    sigset_t		sigs;
    siginfo_t		info;

    sigemptyset (&sigs);
    sigaddset (&sigs, SIGHUP);
    sigaddset (&sigs, SIGINT);
    sigaddset (&sigs, SIGQUIT);
#else
    // setup signal handler
    struct sigaction sa;
    sa.sa_sigaction = SigHandler;
    sa.sa_flags = SA_SIGINFO;
    sigfillset(&sa.sa_mask);
    sigaction (SIGHUP, &sa, 0);
    sigaction (SIGINT, &sa, 0);
    sigaction (SIGQUIT, &sa, 0);
#endif

    while (true) {
#if defined(HAVE_SIGTIMEDWAIT)
	int sigNum = sigtimedwait (&sigs, &info, &tq);
	if (sigNum < 0) {
	  // timeout
	    Ping (nPings);
	}
	else {
	  // signal
	    Error("Received signal %d\n", info.si_signo);
	    exit (0);
	}
#elif defined(HAVE_NANOSLEEP)
	if (nanosleep(&tq, 0) == -1) {
	  // we were interrupted
	}
	else {
	  // timeout
	    Ping (nPings);
	}
#endif
    }

} /* end of PingLoop */

static void Ping (int n)
{
    static int	nextPing = 0;

    for (int i = 0;  i < n;  i++) {
	if (! VProcs[nextPing]->idle)
	    VProcSignal (VProcs[nextPing], PreemptSignal);
	if (++nextPing == NumVProcs)
	    nextPing = 0;
    }

} /* end of Ping */

#ifndef HAVE_SIGTIMEDWAIT
static void SigHandler (int sig, siginfo_t *si, void *_uc)
{
    Error("Received signal %d\n", sig);
    exit (0);
}
#endif


/***** Output and error routines *****/

/* Say:
 * Print a message to the standard output.
 */
void Say (const char *fmt, ...)
{
    va_list	ap;

    va_start (ap, fmt);
    MutexLock (&PrintLock);
      vfprintf (stdout, fmt, ap);
      fflush (stdout);
    MutexUnlock (&PrintLock);
    va_end(ap);

} /* end of Say */

#ifndef NDEBUG
/* SayDebug:
 * Print a message to the debug output stream.
 */
void SayDebug (const char *fmt, ...)
{
    va_list	ap;

    va_start (ap, fmt);
    MutexLock (&PrintLock);
      vfprintf (DebugF, fmt, ap);
      fflush (DebugF);
    MutexUnlock (&PrintLock);
    va_end(ap);

} /* end of SayDebug */
#endif

/* Error:
 * Print an error message.
 */
void Error (const char *fmt, ...)
{
    va_list	ap;
    VProc_t	*vp = VProcSelf();

    va_start (ap, fmt);
    MutexLock (&PrintLock);
	if (vp != 0)
	    fprintf (stderr, "[%2d] Error -- ", VProcSelf()->id);
	else
	    fprintf (stderr, "Error -- ");
	vfprintf (stderr, fmt, ap);
    MutexUnlock (&PrintLock);
    va_end(ap);

} /* end of Error */

/* Warning:
 * Print a warning message.
 */
void Warning (const char *fmt, ...)
{
    va_list	ap;
    VProc_t	*vp = VProcSelf();

    va_start (ap, fmt);
    MutexLock (&PrintLock);
	if (vp != 0)
	    fprintf (stderr, "[%2d] Warning -- ", VProcSelf()->id);
	else
	    fprintf (stderr, "Warning -- ");
	vfprintf (stderr, fmt, ap);
    MutexUnlock (&PrintLock);
    va_end(ap);

} /* end of Warning */


/* Die:
 * Print an error message and then exit.
 */
void Die (const char *fmt, ...)
{
    va_list	ap;
    VProc_t	*vp = VProcSelf();

    va_start (ap, fmt);
    MutexLock (&PrintLock);
	if (vp != 0)
	    fprintf (stderr, "[%2d] Fatal error -- ", VProcSelf()->id);
	else
	    fprintf (stderr, "Fatal error -- ");
	vfprintf (stderr, fmt, ap);
	fprintf (stderr, "\n");
    MutexUnlock(&PrintLock);
    va_end(ap);

    exit (1);

} /* end of Die */
