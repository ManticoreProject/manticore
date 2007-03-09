/* main.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include <stdio.h>
#include <time.h>
#include <signal.h>
#include "options.h"

static void PingLoop ();

static int	TimeQ = DFLT_TIME_Q;

int main (int argc, char **argv)
{
    Options_t *opts = InitOptions (argc, argv);

    VProcInit (opts);
    HeapInit (opts);

  /* create the initial vproc */

    PingLoop();

} /* end of main */


/* PingLoop:
 */
static void PingLoop ()
{
    int			nPings;		/* number of vprocs to ping each iteration */
    sigset_t		sigs;
    struct timespec	tq;

    tq.tv_sec = 0;
    tq.tv_nsec = ??;

#if defined(HAVE_SIGTIMEDWAIT)
    siginfo_t	info;
#endif

    while (true) {
#if defined(HAVE_SIGTIMEDWAIT)
	int sigNum = sigtimedwait (&sigs, &info, &tq);
	if (sigNum < 0) {
	  // timeout
	}
	else {
	  // signal
	}
#elif defined(HAVE_NANOSLEEP)
	if (nanosleep(&ts, 0) == -1) {
	  // we were interrupted
	}
#endif
    }

} /* end of Ping */
