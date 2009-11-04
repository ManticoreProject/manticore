/* log-dump.cxx
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A simple program for dummping out a sorted history from a log file.
 *
 * TODO: check version
 *	 make log-description-file a command-line argument
 *	 config support for log-description-file path
 */

//#include "manticore-config.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <assert.h>
#include "log-file.h"
#include "event-desc.hxx"
#include "log-desc.hxx"
#include "default-log-paths.h"

#define logDescFile	DEFAULT_LOG_EVENTS_PATH
#define logViewFile	DEFAULT_LOG_VIEW_PATH

/* the actual size of a block in the log buffer */
static size_t	LogBufSzB = LOGBLOCK_SZB;
static int	NEventsPerBuf = LOGBUF_SZ;

/* internal representation of event occurrences */
struct Event {
    uint64_t		timestamp;	// time stamp
    int32_t		vpId;		// vproc ID
    EventDesc		*desc;		// description of the event
/*    uint32_t		data[5];	// upto 20 bytes of extra data */
};


LogFileHeader_t		*Hdr;		/* the file's header */
Event			*Events;	/* an array of all of the events */
int			NumEvents;	/* number of events */

static void LoadLogFile (LogFileDesc *logFileDesc, const char *file);
static void PrintEvent (FILE *out, Event *evt);
static void Usage (int sts);

int main (int argc, const char **argv)
{
    const char *logFile = DFLT_LOG_FILE;
    FILE *out = stdout;

  // process args
    for (int i = 1;  i < argc; ) {
	if (strcmp(argv[i], "-h") == 0) {
	    Usage (0);
	}
	else if (strcmp(argv[i], "-o") == 0) {
	    if (++i < argc) {
		out = fopen(argv[i], "w"); i++;
		if (out == NULL) {
		    perror("fopen");
		    exit(1);
		}
	    }
	    else {
		fprintf(stderr, "missing filename for \"-o\" option\n");
		Usage (1);
	    }
	}
	else if (strcmp(argv[i], "-log") == 0) {
	    if (++i < argc) {
		logFile = argv[i]; i++;
	    }
	    else {
		fprintf(stderr, "missing filename for \"-log\" option\n");
		Usage (1);
	    }
	}
	else {
	    fprintf(stderr, "invalid argument \"%s\"\n", argv[i]);
	    Usage(1);
	}
    }

    LogFileDesc *logFileDesc = LoadLogDesc (logDescFile, logViewFile);
    if (logFileDesc == 0) {
	fprintf(stderr, "unable to load \"%s\"\n", logDescFile);
	exit (1);
    }

    LoadLogFile (logFileDesc, logFile);

    fprintf (out, "Log taken on %s\n", Hdr->date);
    fprintf (out, "%d/%d processors; %d events; clock = %s\n",
	Hdr->nVProcs, Hdr->nCPUs, NumEvents, Hdr->clockName);

    for (int i = 0;  i < NumEvents;  i++)
	PrintEvent (out, &(Events[i]));

}

/* compare function for events */
int CompareEvent (const void *ev1, const void *ev2)
{
    int64_t t = (int64_t)((Event *)ev1)->timestamp - (int64_t)((Event *)ev2)->timestamp;
    if (t == 0) return (((Event *)ev1)->vpId - ((Event *)ev2)->vpId);
    else if (t < 0) return -1;
    else return 1;

}

/* convert a timestamp to nanoseconds */
static inline uint64_t GetTimestamp (LogTS_t *ts)
{
    if (Hdr->tsKind == LOGTS_MACH_ABSOLUTE)
	return ts->ts_mach;
    else if (Hdr->tsKind == LOGTS_TIMESPEC)
	return ts->ts_val.sec * 1000000000 + ts->ts_val.frac;
    else /* Hdr->tsKind == LOGTS_TIMEVAL */
	return ts->ts_val.sec * 1000000000 + ts->ts_val.frac * 1000;
}

static void LoadLogFile (LogFileDesc *logFileDesc, const char *file)
{
    char	*buf = new char[LOGBLOCK_SZB];

  /* get the file size */
    off_t fileSize;
    {
	struct stat st;
	if (stat(file, &st) < 0) {
	    perror ("stat");
	    exit (1);
	}
	fileSize = st.st_size;
    }

  /* open the file */
    FILE *f = fopen(file, "rb");
    if (f == NULL) {
	perror ("fopen");
	exit (1);
    }

  /* read the header */
    fread (buf, LOGBLOCK_SZB, 1, f);
    Hdr = new LogFileHeader_t;
    memcpy (Hdr, buf, sizeof(LogFileHeader_t));

  /* check the header */
    if (Hdr->magic != LOG_MAGIC) {
	fprintf(stderr, "bogus magic number\n");
	exit (1);
    }
    if (Hdr->hdrSzB != sizeof(LogFileHeader_t)) {
	fprintf(stderr, "bogus header size %d (expected %d)\n",
	    Hdr->hdrSzB, (int)sizeof(LogFileHeader_t));
	exit (1);
    }
    if (Hdr->majorVersion != LOG_VERSION_MAJOR) {
	fprintf(stderr, "wrong version = %d.%d.%d; expected %d.x.y\n",
	    Hdr->majorVersion, Hdr->minorVersion, Hdr->patchVersion, LOG_VERSION_MAJOR);
	exit (1);
    }
    if (Hdr->bufSzB != LogBufSzB) {
	fprintf (stderr, "using different block size %d\n", Hdr->bufSzB);
      // recompute the sizes
	LogBufSzB = Hdr->bufSzB;
	NEventsPerBuf = (LogBufSzB / sizeof(LogEvent_t)) - 1;
      // reallocate the input buffer
	delete buf;
	buf = new char[LogBufSzB];
      // reset the input file pointer
	if (fseek(f, LogBufSzB, SEEK_SET) == -1) {
	    perror ("fseek");
	    exit (1);
	}
    }

    int numBufs = (fileSize / LogBufSzB) - 1;
    if (numBufs <= 0) {
	fprintf(stderr, "no buffers in file\n");
	exit (1);
    }

  // count the number of buffers per vproc
    int NumBufs[Hdr->nVProcs];
    for (int i = 0;  i < Hdr->nVProcs; i++)
	NumBufs[i] = 0;

    int MaxNumEvents = NEventsPerBuf*numBufs;
    Events = new Event[MaxNumEvents];
    NumEvents = 0;

  /* read in the events */
    for (int i = 0;  i < numBufs;  i++) {
	fread (buf, LogBufSzB, 1, f);
	LogBuffer_t *log = (LogBuffer_t *)buf;
      // check for valid vproc ID
	if ((log->vpId < 0) || (Hdr->nVProcs <= log->vpId)) {
	    fprintf (stderr, "Invalid vproc ID %d\n", log->vpId);
	    exit (1);
	}
      // check sequence number
	if (log->seqNum != NumBufs[log->vpId]) {
	    fprintf (stderr,
		"Vproc %d has missing/out-of-order buffers; expected %d but found %d\n",
		log->vpId, NumBufs[log->vpId], log->seqNum);
	}
	NumBufs[log->vpId] = log->seqNum+1;
	if (log->next > NEventsPerBuf)
	    log->next = NEventsPerBuf;
	for (int j = 0;  j < log->next;  j++) {
	    assert (NumEvents < MaxNumEvents);
	    LogEvent_t *lp = &(log->log[j]);
	    Event *ep = &(Events[NumEvents++]);
	  /* extract event and data fields */
	    ep->timestamp = GetTimestamp(&(lp->timestamp));
	    ep->vpId = log->vpId;
	    ep->desc = logFileDesc->FindEventById (lp->event);
/* FIXME: skip data for now */
	}
    }

  /* sort the events by timestamp */
    qsort (Events, NumEvents, sizeof(Event), CompareEvent);

  /* Adjust the timestamps to be relative to the start of the run */
    uint64_t startTime = GetTimestamp (&(Hdr->startTime));
    if (Events[0].timestamp < startTime) {
	fprintf (stdout, "** Warning: first event occurs %lld ns. before start\n",
	    startTime - Events[0].timestamp);
	startTime = Events[0].timestamp;
    }
    for (int i = 0;  i < NumEvents;  i++) {
	Events[i].timestamp -= startTime;
    }

    fclose (f);
    delete buf;

}

static void PrintEvent (FILE *out, Event *evt)
{

    fprintf (out, "[%3d.%09d] ",
	(int)(evt->timestamp / 1000000000),
	(int)(evt->timestamp % 1000000000));
    for (int i = 0;  i < evt->vpId;  i++)
	fprintf(out, " %20s", " ");
    const char *tag = evt->desc->Name();
    char buf[21];
    int n = strlen(tag);
    strncpy(buf, tag, (n > 20) ? 20 : n);
    buf[(n > 20) ? 20 : n] = '\0';
    fprintf (out, "%-20s\n", buf);

}

static void Usage (int sts)
{
    fprintf (stderr, "usage: log-dump [-o outfile] [-log logfile]\n");
    exit (sts);
}
