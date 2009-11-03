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
#ifdef HAVE_AIO_RETURN
#  include <aio.h>
#endif
#include "inline-log.h"
#include "os-threads.h"
#include "vproc.h"
#include "atomic-ops.h"

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
    strncpy(hdr->clockName, "clock_gettime(CLOCK_REALTIME)", sizeof(hdr->clockName)-1);
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
#ifdef HAVE_AIO_RETURN
  // allocate the AIO control buffer
    vp->logCB = NEW(struct aiocb);
    vp->logCB->aio_buf = 0;  // when the buffer is non-zero, we know that there is a pending write
#endif
}


/* SwapLogBuffers:
 *
 * Swap the current log buffer with the previous one and then write it to
 * disk.
 */
void SwapLogBuffers (VProc_t *vp, LogBuffer_t *curBuf)
{
    LogBuffer_t	*nextBuf;

#ifdef HAVE_AIO_RETURN
  /* wait for any pending pending I/O operations */
    if (vp->logCB->aio_buf != 0) {
	const struct aiocb *const list[1] = { vp->logCB->aio_buf };
	int sts;
	do {
	    sts = aio_suspend (list, 1, 0);
	    if ((sts != 0) && (errno != EINTR)) {
		Error("Failure writing log data; errno = %d\n", errno);
		break;
	    }
	} while (sts != 0);
    }
#endif

    nextBuf = vp->prevLog;

/* FIXME: in the software-polling version, we don't need the CAS, since there
 * is no preemption.
 */

  // atomically set the current log buffer to be nextBuf; if this operation
  // fails, then we must have been preempted and the signal handler did the swap.
    if (CompareAndSwapPtr(&(vp->log), curBuf, nextBuf) == curBuf) {
	vp->prevLog = curBuf;
#ifdef HAVE_AIO_RETURN
      // schedule a write of the buffer to the log file
	bzero (vp->logCB, sizeof(struct aiocb));
	vp->logCB->aio_fildes = LogFD;
	vp->logCB->aio_buf = curBuf;
	vp->logCB->aio_nbytes = LOGBLOCK_SZB;
	if (aio_write(vp->logCB) < 0) {
	    Error("Failure writing log data; errno = %d\n", errno);
	    vp->logCB->aio_buf = 0;  // no pending write
	}
#else
      // write the buffer to a file
	ssize_t nb;
	do {
	    nb = write (LogFD, curBuf, LOGBLOCK_SZB);
	    if ((nb < 0) && (errno != EINTR)) {
		Error("Failure writing log data; errno = %d\n", errno);
		break;
	    }
	} while (nb < 0);
#endif
      // reset curBuf's next pointer for its next use
	curBuf->next = 0;
    }

}

/* FinishLog:
 */
void FinishLog ()
{
    if (LogFD < 0)
	return;

#ifdef HAVE_AIO_RETURN
  /* first wait for any pending pending I/O operations */
    for (int i = 0;  i < NumVProcs;  i++) {
	VProc_t *vp = VProcs[i];
	if (vp->logCB->aio_buf != 0) {
	    const struct aiocb *const list[1] = { vp->logCB->aio_buf };
	    int sts;
	    do {
		sts = aio_suspend (list, 1, 0);
		if ((sts != 0) && (errno != EINTR)) {
		    Error("Failure writing log data; errno = %d\n", errno);
		    break;
		}
	    } while (sts != 0);
	}
    }
#endif

  /* flush out any remaining vproc buffers. */
    for (int i = 0;  i < NumVProcs;  i++) {
	VProc_t *vp = VProcs[i];
	if (vp->log->next != 0) {
	    write (LogFD, vp->log, LOGBLOCK_SZB);
	    vp->log->next = 0;
	}
    }

  /* close the file */
    close (LogFD);
    LogFD = -1;

} /* end of FinishLog */
