/* log.c
 *
 * COPYRIGHT (c) 2016 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 */

#include "manticore-rt.h"
#include <unistd.h>
#include <fcntl.h>
#include <strings.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#ifdef HAVE_MACH_ABSOLUTE_TIME
#  include <mach/mach_time.h>
#endif
#include "inline-event-log.h"
#include "os-threads.h"
#include "vproc.h"

static int	LogFD = -1;
uint64_t start_ts;

#ifdef HAVE_MACH_ABSOLUTE_TIME
uint64_t timer_scaling_factor_numer;
uint64_t timer_scaling_factor_denom;
#endif



//Taken from GHC: https://github.com/ml9951/ghc/blob/pastm/rts/posix/GetTime.c#L86-L119
uint64_t getProcessElapsedTime(){
#if defined(HAVE_CLOCK_GETTIME)
    struct timespec ts;
    int res;

    res = clock_gettime(CLOCK_ID, &ts);
    if (res != 0) {
        Die("clock_gettime\n");
    }
    return (uint64_t)ts.tv_sec * 1000000000 + (uint64_t)ts.tv_nsec;

#elif defined(HAVE_MACH_ABSOLUTE_TIME)

    uint64_t time = mach_absolute_time();
    return (time * timer_scaling_factor_numer) / timer_scaling_factor_denom;

#else // use gettimeofday()

    struct timeval tv;
    gettimeofday(&tv, (struct timezone *) NULL);
    return (uint64_t)tv.tv_sec * 1000000000 +
           (uint64_t)tv.tv_usec * 1000;

#endif
}

uint64_t get_elapsed_time(){
    return getProcessElapsedTime() - start_ts;
}

void initElapsedTS(){
#ifdef HAVE_MACH_ABSOLUTE_TIME
    mach_timebase_info_data_t info;
    (void) mach_timebase_info(&info);
    timer_scaling_factor_numer = (uint64_t)info.numer;
    timer_scaling_factor_denom = (uint64_t)info.denom;
#endif
    start_ts = getProcessElapsedTime();
}

void printAndClearEventBuf (VProc_t * vp)
{
    EventsBuf * ebuf = vp->event_log;
    uint64_t numBytes = 0, written = 0;

    closeBlockMarker(vp->event_log);
    
    if (ebuf->begin != NULL && ebuf->pos != ebuf->begin)
    {
        numBytes = ebuf->pos - ebuf->begin;

	if(write(LogFD, ebuf->begin, numBytes) < 0){
	    Die("error in printAndClearEventBuf\n");
	}
	ebuf->pos = ebuf->begin;
	ebuf->marker = NULL;

	postBlockMarker(vp);
	
    }
}

/* InitLogFile:
 *
 * Initialize the log file.
 */
void InitEventLogFile (const char *name, int nvps, int ncpus)
{
    if ((LogFD = open(name, O_TRUNC|O_CREAT|O_WRONLY|O_APPEND, 0664)) < 0) {
	Die ("unable to open log file: %s", strerror(errno));
    }    
}

/* InitLog:
 *
 * Initialize the log buffers for the given vproc.
 */
void InitEventLog (VProc_t *vp)
{
    vp->event_log = NEW(EventsBuf);
    vp->event_log->pos = vp->event_log->begin;
    vp->event_log->size = LOGBLOCK_SZB;
    vp->event_log->marker = NULL;
    vp->event_log->vpNum = vp->id;

    if(vp->id == 0){
	initElapsedTS();
	postEventTypes(vp->event_log);
	printAndClearEventBuf(vp);
    }
    
}

/* FinishLog:
 */
void FinishEventLog ()
{
    if (LogFD < 0)
	return;

    
    
  /* flush out any remaining vproc buffers. */
    for (int i = 0;  i < NumVProcs;  i++) {
	VProc_t *vp = VProcs[i];
	printAndClearEventBuf (vp);
	if(i == 0){

	    vp->event_log->pos = vp->event_log->begin;
	    vp->event_log->marker = NULL;
	    
	    postWord16(vp->event_log, EVENT_DATA_END);
	    printAndClearEventBuf(vp);
	}
    }

  /* close the file */
    close (LogFD);
    LogFD = -1;

} /* end of FinishLog */

