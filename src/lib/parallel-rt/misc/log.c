/* log.c
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
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
#include "inline-log.h"
#include "os-threads.h"
#include "vproc.h"

static int	LogFD = -1;


/* InitLogFile:
 *
 * Initialize the log file.
 */
void InitLogFile (const char *name, int nvps, int ncpus)
{
    union {
	LogFileHeader_t	_hdr;
        char		_pad[LOGBLOCK_SZB];
    } hdrBuf;
    LogFileHeader_t *hdr = &hdrBuf._hdr;

    if ((LogFD = open(name, O_TRUNC|O_CREAT|O_WRONLY|O_APPEND, 0664)) < 0) {
	Die ("unable to open log file: %s", strerror(errno));
    }

  /* initialize the header */
    bzero(&hdrBuf, LOGBLOCK_SZB);
    hdr->magic		= LOG_MAGIC;
    hdr->majorVersion	= LOG_VERSION_MAJOR;
    hdr->minorVersion	= LOG_VERSION_MINOR;
    hdr->patchVersion	= LOG_VERSION_PATCH;
    hdr->hdrSzB		= sizeof(LogFileHeader_t);
    hdr->bufSzB		= LOGBLOCK_SZB;
    time_t tim = time(0);
    ctime_r (&tim, hdr->date);
    hdr->date[24]	= '\0';  /* zero out '\n' */
    LogTimestamp (&(hdr->startTime));
#if HAVE_MACH_ABSOLUTE_TIME
    hdr->tsKind		= LOGTS_MACH_ABSOLUTE;
    strncpy(hdr->clockName, "mach_absolute_time", sizeof(hdr->clockName)-1);
    hdr->resolution	= 1;
#elif HAVE_CLOCK_GETTIME
    hdr->tsKind		= LOGTS_TIMESPEC;
    strncpy(hdr->clockName, "clock_gettime(CLOCK_REALTIME)", 23);
    struct timespec res;
    clock_getres(CLOCK_REALTIME, &res);
    hdr->resolution	= res.tv_nsec;
#else
    hdr->tsKind		= LOGTS_TIMEVAL;
    strncpy(hdr->clockName, "gettimeofday", sizeof(hdr->clockName)-1);
    hdr->resolution	= 1000;
#endif
    hdr->nVProcs	= nvps;
    hdr->nCPUs		= ncpus;

  /* write the header block */
    if (write(LogFD, hdr, LOGBLOCK_SZB) < 0)
	Die("Error writing logfile header\n");

}

/* InitLog:
 *
 * Initialize the log buffers for the given vproc.
 */
void InitLog (VProc_t *vp)
{
    vp->log = NEW(LogBuffer_t);
    vp->log->vpId = vp->id;
    vp->log->next = 0;
    vp->prevLog = NEW(LogBuffer_t);
    vp->prevLog->vpId = vp->id;
    vp->prevLog->next = 0;
    vp->eventId = ((uint64_t)vp->id & 0xff) << 56;  // high 8 bits have vproc ID
}


/* SwapLogBuffers:
 *
 * Swap the current log buffer with the previous one after writing the previous
 * buffer to disk.
 */
void SwapLogBuffers (VProc_t *vp)
{
    LogBuffer_t	*nextBuf;

    nextBuf = vp->prevLog;
    if (nextBuf->next != 0) {
	ssize_t nb;
	do {
	    nb = write (LogFD, nextBuf, LOGBLOCK_SZB);
	    if ((nb < 0) && (errno != EINTR)) {
		Error("Failure writing log data; errno = %d\n", errno);
		break;
	    }
	} while (nb < 0);
    }
    vp->prevLog = vp->log;
    vp->log = nextBuf;

    nextBuf->next = 0;

}

/* FinishLog:
 */
void FinishLog ()
{
    if (LogFD < 0)
	return;

  /* first flush out any remaining vproc buffers. */
    for (int i = 0;  i < NumVProcs;  i++) {
	VProc_t *vp = VProcs[i];
	if (vp->prevLog->next != 0) {
	    write (LogFD, vp->prevLog, LOGBLOCK_SZB);
	    vp->prevLog->next = 0;
	}
	if (vp->log->next != 0) {
	    write (LogFD, vp->log, LOGBLOCK_SZB);
	    vp->log->next = 0;
	}
    }

  /* close the file */
    close (LogFD);
    LogFD = -1;

} /* end of FinishLog */
