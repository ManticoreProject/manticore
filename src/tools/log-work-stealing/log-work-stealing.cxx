/* log-work-stealing.cxx
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Reports on several statistics relevant to the performance of the work stealing scheduler.
 *   - Number of steals
 *   - Number of failed steal attempts
 *   - The average and maximum time to attempt a steal.
 *   - Per-vproc time spent idle, busy and sleeping. Note that a vproc is considered idle
 *     while it is sleeping.
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
static void PrintTimestamp (FILE *out, uint64_t timestamp);
static void Usage (int sts);
static inline uint64_t max (uint64_t x, uint64_t y) { return x < y ? y : x; }

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

    /** check for bogus log data **/
    bool foundInitFirst = false;

    for (int i = 0; i < NumEvents; i++) {
	if (foundInitFirst)
	    break;
	Event *evt = &(Events[i]);
	int evtId = evt->desc->Id();
	switch (evtId) {
	case WSInitEvt:
	    foundInitFirst = true;
	    break;
	case WSThiefSendEvt:
	case WSThiefSuccessfulEvt:
	case WSThiefUnsuccessfulEvt:
	case WSExecuteEvt:
	    printf ("Bogus log file: WSInitEvt was preceded by another WS* event.");
	    exit (1);                 // WSInitEvt must precede all other work stealing events
	}	
    }

  /** count the overall number of steals and failed steal attempts **/
    int VProcNumSteals[Hdr->nVProcs];
    int VProcNumFailedStealAttempts[Hdr->nVProcs];

    {
	bool IsTerminated = false;
	for (int i = 0; i < Hdr->nVProcs; i++) {
	    VProcNumSteals[i] = 0;
	    VProcNumFailedStealAttempts[i] = 0;
	}

	for (int i = 0; i < NumEvents; i++) {
	    Event *evt = &(Events[i]);
	    if (evt->desc->Id() == WSThiefSuccessfulEvt) {
		VProcNumSteals[evt->vpId]++;
	    }
	    else if (evt->desc->Id() == WSThiefUnsuccessfulEvt) {
		VProcNumFailedStealAttempts[evt->vpId]++;
	    } else if (evt->desc->Id() == WSTerminateEvt) {
		break;
	    }
	}
    }

    uint64_t TotalTimeRebalancing[Hdr->nVProcs];
    int NumRebalances[Hdr->nVProcs];
    {
	uint64_t VProcTimestamp[Hdr->nVProcs];          // timestamp of immediately preceding, relevant event 
	
	for (int i = 0; i < Hdr->nVProcs; i++) {
	    VProcTimestamp[i] = 0ul;
	    NumRebalances[i] = 0;
	    TotalTimeRebalancing[i] = 0ul;
	}

	for (int i = 0; i < NumEvents; i++) {
	    Event *evt = &(Events[i]);
	    int evtId = evt->desc->Id();
	    switch (evtId) {
	    case RopeRebalanceBeginEvt:
		VProcTimestamp[evt->vpId] = evt->timestamp;
		break;
	    case RopeRebalanceEndEvt:
		uint64_t timeRebalancing = evt->timestamp - VProcTimestamp[evt->vpId];
		TotalTimeRebalancing[evt->vpId] += timeRebalancing;
		NumRebalances[evt->vpId]++;
		break;
	    }
	}
    }

  /** measure the average and max time spent stealing (per vproc) **/
    uint64_t AvgTimeStealing[Hdr->nVProcs];
    uint64_t MaxTimeStealing[Hdr->nVProcs];
    {
	bool IsTerminated = false;
	uint64_t VProcTimestamp[Hdr->nVProcs];          // timestamp of immediately preceding, relevant event 
	uint64_t TotalTimeStealing[Hdr->nVProcs];       // total time spent stealing so far
	uint64_t NumStealAttempts[Hdr->nVProcs];

	for (int i = 0; i < Hdr->nVProcs; i++) {
	    AvgTimeStealing[i] = 0ul;
	    MaxTimeStealing[i] = 0ul;
	    VProcTimestamp[i] = 0ul;
	    TotalTimeStealing[i] = 0ul;
	    NumStealAttempts[i] = 0ul;
	}

	for (int i = 0; i < NumEvents; i++) {
	    if (IsTerminated)
		break;

	    Event *evt = &(Events[i]);
	    int evtId = evt->desc->Id();
	    switch (evtId) {
	    case WSTerminateEvt:
		IsTerminated = true;
		break;
	    case WSThiefSendEvt:
		VProcTimestamp[evt->vpId] = evt->timestamp;
		break;
	    case WSThiefSuccessfulEvt:
	    case WSThiefUnsuccessfulEvt:
		{
		    if (VProcTimestamp[evt->vpId] == 0ul) {
			printf ("bug: steal event preceding a send event...\n");
			continue;
		    }
		    uint64_t timeStealing = evt->timestamp - VProcTimestamp[evt->vpId];
		    TotalTimeStealing[evt->vpId] += timeStealing;
		    MaxTimeStealing[evt->vpId] = max(MaxTimeStealing[evt->vpId], timeStealing);
		    NumStealAttempts[evt->vpId]++;
		}
		break;
	    }
	}

	for (int i = 0; i < Hdr->nVProcs; i++) 
	    if (NumStealAttempts[i] != 0)
		AvgTimeStealing[i] = TotalTimeStealing[i] / NumStealAttempts[i];

    }

  /** measure the time spent either busy or idle (per vproc) **/
    uint64_t VProcTimeBusy[Hdr->nVProcs];
    uint64_t VProcTimeIdle[Hdr->nVProcs];
    {
	bool     IsTerminated = false;            // true, if the computation has finished
	bool     VProcIdle[Hdr->nVProcs];         // true, if the vproc is currently idle
	bool     VProcTerminated[Hdr->nVProcs];   // true, if the vproc terminated cleanly
	uint64_t VProcTimestamp[Hdr->nVProcs];    // timestamp of immediately preceding event
	uint32_t LastEvent = 0;             // index of the last event to be measured during the execution of work stealing
	
	for (int i = 0; i < Hdr->nVProcs; i++) {
	    VProcIdle[i] = true;
	    VProcTerminated[i] = false;
	    VProcTimestamp[i] = 0ul;
	    VProcTimeBusy[i] = 0ul;
	    VProcTimeIdle[i] = 0ul;
	}
	
	for (int i = 0; i < NumEvents; i++) {
	    LastEvent = i;
	    if (IsTerminated)
		break;

	    Event *evt = &(Events[i]);
	    int evtId = evt->desc->Id();
	    switch (evtId) {
	    case WSInitEvt:
		{
		  /* we mark vprocs as idle right after the workgroup is created */
		    for (int j = 0; j < Hdr->nVProcs; j++)
			VProcTimestamp[j] = evt->timestamp;
		}
		break;
	    case WSTerminateEvt:
		{
		    if (VProcIdle[evt->vpId])
			VProcTimeBusy[evt->vpId] += evt->timestamp - VProcTimestamp[evt->vpId];
		    else
			VProcTimeIdle[evt->vpId] += evt->timestamp - VProcTimestamp[evt->vpId];
		    VProcTerminated[evt->vpId] = true;
		}
		break;
	    case WSExecuteEvt:
		{
		    if (VProcTimestamp[evt->vpId] == 0ul) {
			// we fudge a bit here to deal with clock skew
			VProcTimestamp[evt->vpId] = evt->timestamp;
		    } 
		    else if (VProcIdle[evt->vpId]) {
			VProcTimeIdle[evt->vpId] += evt->timestamp - VProcTimestamp[evt->vpId];
			VProcTimestamp[evt->vpId] = evt->timestamp;
		    } 
		    else {
			// already busy
		    }
		    VProcIdle[evt->vpId] = false;
		}
		break;
	    case WSPreemptedEvt:
	    case WSThiefSendEvt:
		{
		    if (VProcTimestamp[evt->vpId] == 0ul) {
			// we fudge a bit here to deal with clock skew
			VProcTimestamp[evt->vpId] = evt->timestamp;
		    }
		    else if (!VProcIdle[evt->vpId]) {
			VProcTimeBusy[evt->vpId] += evt->timestamp - VProcTimestamp[evt->vpId];
			VProcTimestamp[evt->vpId] = evt->timestamp;
		    } 
		    else {
			// already idle
		    }
		    VProcIdle[evt->vpId] = true;		    
		}
		break;
	    }
	}

      /* account for any remaining time in case the vproc did not shut down explicitly */
	Event *lastEvt = &(Events[LastEvent]);
	/*
	for (int i = 0; i < Hdr->nVProcs; i++) {
	    if (!VProcTerminated[i]) {
		if (VProcIdle[i])
		    VProcTimeIdle[i] += lastEvt->timestamp - VProcTimestamp[i];
		else
		    VProcTimeBusy[i] += lastEvt->timestamp - VProcTimestamp[i];
	    }
	}
	*/
    }

  /** measure the time spent sleeping **/
    uint64_t VProcTimeSleeping[Hdr->nVProcs];
    {
	bool     IsTerminated = false;
	bool     VProcSleeping[Hdr->nVProcs];           // true, if the vproc
	bool     VProcTerminated[Hdr->nVProcs];         // true, if the vproc terminated cleanly
	uint64_t VProcTimestamp[Hdr->nVProcs];          // timestamp of immediately preceding, relevant event 

	for (int i = 0; i < Hdr->nVProcs; i++) {
	    VProcSleeping[i] = false;
	    VProcTerminated[i] = false;
	    VProcTimestamp[i] = 0ul;
	    VProcTimeSleeping[i] = 0ul;
	}

	for (int i = 0; i < NumEvents; i++) {
	    if (IsTerminated)
		break;

	    Event *evt = &(Events[i]);
	    int evtId = evt->desc->Id();
	    switch (evtId) {
	    case WSInitEvt:
		{
		    for (int j = 0; j < Hdr->nVProcs; j++)
			VProcTimestamp[j] = evt->timestamp;
		}
		break;
	    case WSTerminateEvt:
		{
		    if (VProcSleeping[evt->vpId])
			VProcTimeSleeping[evt->vpId] += evt->timestamp - VProcTimestamp[evt->vpId];
		    IsTerminated = true;
		    VProcTerminated[evt->vpId] = true;
		}
		break;
	    case WSExecuteEvt:
		{
		    if (VProcTimestamp[evt->vpId] == 0ul) {
			// fudge a bit here to deal with clock skew
		    }
		    else if (VProcSleeping[evt->vpId]) {
			VProcTimeSleeping[evt->vpId] += evt->timestamp - VProcTimestamp[evt->vpId];
		    } 
		    else {
			// already busy
		    }
		    VProcSleeping[evt->vpId] = false;
		}
		break;
	    case WSPreemptedEvt:
	    case WSThiefSendEvt:
		{
		    if (VProcTimestamp[evt->vpId] == 0ul) {
			// fudge a bit here to deal with clock skew
		    }
		    else if (VProcSleeping[evt->vpId]) {
			VProcTimeSleeping[evt->vpId] += evt->timestamp - VProcTimestamp[evt->vpId];
		    } 
		    else {
			// already idle
		    }
		    VProcSleeping[evt->vpId] = false;
		}
		break;
	    case WSSleepEvt:
		{
		    VProcTimestamp[evt->vpId] = evt->timestamp;
		    VProcSleeping[evt->vpId] = true;
		}
		break;
	    }
	}

      /* account for any remaining time in case the vproc did not shut down explicitly */
	Event *lastEvt = &(Events[NumEvents-1]);
	for (int i = 0; i < Hdr->nVProcs; i++) {
	    if (!VProcTerminated[i]) {
		if (VProcSleeping[i])
		    VProcTimeSleeping[i] += lastEvt->timestamp - VProcTimestamp[i];
	    }
	}
    }

    fprintf (out, "{\n");
    fprintf (out, "numVProcs=%d,\n", Hdr->nVProcs);
    fprintf (out, "clock=\"%s\",\n", Hdr->clockName);
    fprintf (out, "numSteals=\n");
    fprintf (out, "[");    
    for (int i = 0; i < Hdr->nVProcs; i++) {
	fprintf (out, " %d", VProcNumSteals[i]);
	if (i < Hdr->nVProcs - 1)
	    fprintf (out, ",");	    
	else
	    fprintf (out, "],\n");
    }
    fprintf (out, "numFailedStealAttempts=\n");
    fprintf (out, "[");    
    for (int i = 0; i < Hdr->nVProcs; i++) {
	fprintf (out, " %d", VProcNumFailedStealAttempts[i]);
	if (i < Hdr->nVProcs - 1)
	    fprintf (out, ",");
	else
	    fprintf (out, "],\n");
    }
    fprintf (out, "vprocState=\n");
    fprintf (out, "[\n");    
    for (int i = 0; i < Hdr->nVProcs; i++) {
	fprintf (out, " {timeBusy=");
	PrintTimestamp (out, VProcTimeBusy[i]);
	fprintf (out, " ,");
	fprintf (out, " timeIdle=");
	PrintTimestamp (out, VProcTimeIdle[i]);
	fprintf (out, " ,");
	fprintf (out, " timeSleeping=");
	PrintTimestamp (out, VProcTimeSleeping[i]);
	fprintf (out, " ,");
	fprintf (out, " timeRebalancing=");
	PrintTimestamp (out, TotalTimeRebalancing[i]);
	fprintf (out, " ,");
	fprintf (out, " numRebalances=%d", NumRebalances[i]);
	if (i < Hdr->nVProcs - 1)
	    fprintf (out, " },\n");
	else
	    fprintf (out, " }\n");
    }
    fprintf (out, "],\n");
    fprintf (out, "timeStealing=\n");
    fprintf (out, "[\n");    
    for (int i = 0; i < Hdr->nVProcs; i++) {
	fprintf (out, " {avg=");
	PrintTimestamp (out, AvgTimeStealing[i]);
	fprintf (out, " ,");
	fprintf (out, " max=");
	PrintTimestamp (out, MaxTimeStealing[i]);
	if (i < Hdr->nVProcs - 1)
	    fprintf (out, " },\n");
	else
	    fprintf (out, " }\n");
    }
    fprintf (out, "]\n");
    fprintf (out, "}\n");
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

    int MaxNumEvents = NEventsPerBuf*numBufs;
    Events = new Event[MaxNumEvents];
    NumEvents = 0;

  /* read in the events */
    for (int i = 0;  i < numBufs;  i++) {
	fread (buf, LogBufSzB, 1, f);
	LogBuffer_t *log = (LogBuffer_t *)buf;
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

static void PrintTimestamp (FILE *out, uint64_t timestamp)
{
    fprintf (out, "%3d.%09d",
	     (int)(timestamp / 1000000000),
	     (int)(timestamp % 1000000000));
}

static void Usage (int sts)
{
    fprintf (stderr, "usage: log-work-stealing [-o outfile] [-log logfile]\n");
    exit (sts);
}

