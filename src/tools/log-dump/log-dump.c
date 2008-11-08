/* log-dump.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A simple program for dummping out a sorted history from a log file.
 */

#include "manticore-config.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <assert.h>
#include "log-file.h"
#include "log-info.h"

/* object allocation in the C heap */
#define MALLOC(sz)		malloc(sz)
#define CALLOC(n,sz)		calloc(n,sz)
#define FREE(p)			free(p)
#define NEW(ty)			((ty *)MALLOC(sizeof(ty)))
#define NEWVEC(ty, n)		((ty *)MALLOC((n)*sizeof(ty)))


typedef struct struct_logbuf LogBuffer_t;

/* internal representation of events */
typedef struct {
    uint64_t		timestamp;	// time stamp
    int32_t		vpId;		// vproc ID
    uint32_t		event;		// event code
/*    uint32_t		data[5];	// upto 20 bytes of extra data */
} Event_t;


LogFileHeader_t		*Hdr;		/* the file's header */
Event_t			*Events;	/* an array of all of the events */
int			NumEvents;	/* number of events */

static void LoadLogFile (const char *file);
static void PrintEvent (FILE *out, Event_t *evt);
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

    LoadLogFile (logFile);

    fprintf(out, "%d/%d processors; %d events; clock = %s\n",
	Hdr->nVProcs, Hdr->nCPUs, NumEvents, Hdr->clockName);

    for (int i = 0;  i < NumEvents;  i++)
	PrintEvent (out, &(Events[i]));

}

/* compare function for events */
int CompareEvent (const void *ev1, const void *ev2)
{
    int64_t t = (int64_t)((Event_t *)ev1)->timestamp - (int64_t)((Event_t *)ev2)->timestamp;
    if (t == 0) return (((Event_t *)ev1)->vpId - ((Event_t *)ev2)->vpId);
    else if (t < 0) return -1;
    else return 1;

}

/* convert a timestamp to microseconds */
static inline uint64_t GetTimestamp (LogTS_t *ts)
{
    if (Hdr->tsKind == LOGTS_MACH_ABSOLUTE)
	return ts->ts_mach / 1000;
    else if (Hdr->tsKind == LOGTS_TIMESPEC)
	return ts->ts_val.sec * 1000000 + ts->ts_val.frac / 1000;
    else /* Hdr->tsKind == LOGTS_TIMEVAL */
	return ts->ts_val.sec * 1000000 + ts->ts_val.frac;
}

static void LoadLogFile (const char *file)
{
    char	buf[LOGBLOCK_SZB];

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
    FILE	*f = fopen(file, "rb");
    if (f == NULL) {
	perror("fopen");
	exit(1);
    }

  /* read the header */
    fread (buf, LOGBLOCK_SZB, 1, f);
    Hdr = NEW(LogFileHeader_t);
    memcpy(Hdr, buf, sizeof(LogFileHeader_t));

  /* check the header */
    if (Hdr->magic != 0x6D616E7469636F72) {
	fprintf(stderr, "bogus magic number\n");
	exit (1);
    }
    if (Hdr->version != LOG_VERSION) {
	fprintf(stderr, "wrong version = %#x; expected %#x\n",
	    Hdr->version, LOG_VERSION);
	exit (1);
    }
    if (Hdr->bufSzB != LOGBLOCK_SZB) {
	fprintf(stderr, "bogus block size\n");
	exit (1);
    }

    int numBufs = (fileSize / LOGBLOCK_SZB) - 1;
    if (numBufs <= 0) {
	fprintf(stderr, "no buffers in file\n");
	exit (1);
    }

    int MaxNumEvents = LOGBUF_SZ*numBufs;
    Events = NEWVEC(Event_t, MaxNumEvents);
    NumEvents = 0;
    uint64_t startTime = GetTimestamp (&(Hdr->startTime));

  /* read in the events */
    for (int i = 0;  i < numBufs;  i++) {
	fread (buf, LOGBLOCK_SZB, 1, f);
	LogBuffer_t *log = (LogBuffer_t *)buf;
	if (log->next > LOGBUF_SZ)
	    log->next = LOGBUF_SZ;
	for (int j = 0;  j < log->next;  j++) {
	    assert (NumEvents < MaxNumEvents);
	    LogEvent_t *lp = &(log->log[j]);
	    Event_t *ep = &(Events[NumEvents++]);
	  /* extract event and data fields */
	    ep->timestamp = GetTimestamp(&(lp->timestamp)) - startTime;
	    ep->vpId = log->vpId;
	    ep->event = lp->event;
/* FIXME: skip data for now */
	}
    }

  /* sort the events by timestamp */
    qsort (Events, NumEvents, sizeof(Event_t), CompareEvent);

    fclose (f);

}

/* the event table */
typedef struct {
    const char		*tag;	/* name of the event */
    LogEventKind_t	kind;	/* kind: independent/start/stop */
    const char		*desc;	/* description */
} EventInfo_t;

#define DEF_EVENT(ID, NARGS, KIND, DESC) \
	[ID] = { .tag = #ID, .kind = KIND, .desc = DESC },	\

EventInfo_t	Info[NumLogEvents] = {
#include "log-events.h"
};

static void PrintEvent (FILE *out, Event_t *evt)
{

    fprintf(out, "[%4d.%06d] ",
	(int)(evt->timestamp / 1000000),
	(int)(evt->timestamp % 1000000));
    for (int i = 0;  i < evt->vpId;  i++)
	fprintf(out, " %20s", " ");
    const char *tag = Info[evt->event].tag;
    char buf[21];
    strncpy(buf, tag, (n > 20) ? 20 : n);
    buf[(n > 20) ? 20 : n] = '\0';
    fprintf (out, "%-20s\n", buf);

}

static void Usage (int sts)
{
    fprintf (stderr, "usage: log-dump [-o outfile] [-log logfile]\n");
    exit (sts);
}

