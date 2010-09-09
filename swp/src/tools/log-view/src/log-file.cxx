/* log-file.cxx
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "log-file.hxx"
#include <stdio.h>
#include <sys/mman.h>
#include <sys/stat.h>

/***** The representation of events *****/

EventAttrs::~EventAttrs () { }

TaggedArgValue EventAttrs::Arg (int i) const
{
    TaggedArgValue v;
    v.desc = 0;
    v.val.a = 0;
    return v;
}

WithArgsAttrs::WithArgsAttrs (EventDesc *d, LogEvent_t *rawEvt)
    : EventAttrs (d)
{
    this->_args = new ArgValue[d->NArgs()];
    for (int i = 0;  i < d->NArgs();  i++) {
	this->_args[i] = d->GetArg(rawEvt, i);
    }
}

WithArgsAttrs::~WithArgsAttrs ()
{
    delete this->_args;
}

TaggedArgValue WithArgsAttrs::Arg (int i) const
{
    TaggedArgValue value;

    value.desc = this->_desc->GetArgDesc(i);
    value.val = this->_args[i];

    return value;
}

IntervalEventAttrs::IntervalEventAttrs (EventDesc *d)
    : EventAttrs (d)
{
    this->_start = 0; // to be filled in later
    this->_end = 0; // to be filled in later
}

IntervalWithArgsAttrs::IntervalWithArgsAttrs (EventDesc *d, LogEvent_t *rawEvt)
    : WithArgsAttrs (d, rawEvt)
{
    this->_start = 0; // to be filled in later
    this->_end = 0; // to be filled in later
}

DependentEventAttrs::DependentEventAttrs (EventDesc *d, LogEvent_t *rawEvt)
    : WithArgsAttrs (d, rawEvt)
{
  // find the runtime ID for this event
    for (int i = 0;  i < d->NArgs();  i++) {
	if (d->GetArgType(i) == EVENT_ID) {
	    this->_id = this->_args[i].id;
	    break;
	}
    }
    this->_other = 0; // to be filled in later
}

// This function allocates event attributes based on the descriptor and log data.
// for simple events that do not have arguments, it caches their representation
// so that it can be shared across all instances.
//
static EventAttrs *EventAttrsFactory (LogFileDesc *lfd, LogEvent_t *rawEvt)
{
    static bool isFirst = true;
    static SimpleEventAttrs **attrs = 0;

    if (isFirst) {
	isFirst = false;
	attrs = new SimpleEventAttrs *[lfd->NumEventKinds()];
	for (int i = 0;  i < lfd->NumEventKinds(); i++) {
	    attrs[i] = 0;
	}
    }

    EventDesc *desc = lfd->FindEventById(rawEvt->event);
    switch (desc->Kind()) {
      case LOG_EVENT:
	if (desc->NArgs() == 0) {
	    if (attrs[rawEvt->event] == 0)
		attrs[rawEvt->event] = new SimpleEventAttrs(desc);
	    return attrs[rawEvt->event];
	}
	else { // event with args
	    return (new WithArgsAttrs(desc, rawEvt));
	}
      case LOG_START:
      case LOG_END:
	if (desc->NArgs() == 0) {
	    return (new IntervalEventAttrs(desc));
	}
	else {
	    return (new IntervalWithArgsAttrs(desc, rawEvt));
	}
      case LOG_SRC:
      case LOG_DST:
	return (new DependentEventAttrs(desc, rawEvt));
    }

}

/***** Helper functions for converting timestamps *****/

typedef Time_t (*TimeCvtFn_t) (LogTS_t);

// conversion to nanoseconds for struct timeval representation of time
//
Time_t TimevalCvt (LogTS_t t)
{
    return 1000000000*(Time_t)t.ts_val.sec + 1000*(Time_t)t.ts_val.frac;
}

// conversion to nanoseconds for struct timespec representation of time
//
Time_t TimespecCvt (LogTS_t t)
{
    return 1000000000*(Time_t)t.ts_val.sec + (Time_t)t.ts_val.frac;
}

// conversion to nanoseconds for Mach's absolute time representation of time
//
Time_t MachAbsoluteCvt (LogTS_t t)
{
    return t.ts_mach;
}

/***** class LogFile members *****/

LogFile::LogFile (const char *logDescFileName)
{
    this->_nVProcs = 0;
    this->_startTime = 0;
    this->_endTime = 0;
    this->_traces = 0;

    if (! this->_filter.Init(logDescFileName)) {
      // error loading log-description file
	exit (1);
    }

}

LogFile::~LogFile ()
{
    if (this->_nVProcs != 0) {
	delete this->_traces;
//	delete this->_timeline;
    }

}

// load a log file
//
bool LogFile::LoadFile (const char *file)
{
    if (this->_nVProcs != 0) {
	return false;  // file already loaded
    }

  /* get the file size */
    off_t fileSize;
    {
	struct stat st;
	if (stat(file, &st) < 0) {
	    perror ("stat");
	    return false;
	}
	fileSize = st.st_size;
    }

  /* open the file */
    FILE *f = fopen(file, "rb");
    if (f == NULL) {
	perror ("fopen");
	return false;
    }

  /* read the header */
    LogFileHeader_t hdr;
    fread (&hdr, sizeof(LogFileHeader_t), 1, f);

  /* check the header */
    if (hdr.magic != LOG_MAGIC) {
	fprintf(stderr, "bogus magic number\n");
	return false;
    }
    if (hdr.hdrSzB != sizeof(LogFileHeader_t)) {
	fprintf(stderr, "bogus header size %d (expected %d)\n", hdr.hdrSzB, sizeof(LogFileHeader_t));
	return false;
    }
    if (hdr.majorVersion != LOG_VERSION_MAJOR) {
	fprintf(stderr, "wrong version = %d.%d.%d; expected %d.x.y\n",
	    hdr.majorVersion, hdr.minorVersion, hdr.patchVersion, LOG_VERSION_MAJOR);
	return false;
    }

    size_t LogBufSzB = LOGBLOCK_SZB;
    int NEventsPerBuf = LOGBUF_SZ;
    if (hdr.bufSzB != LogBufSzB) {
	fprintf (stderr, "using different block size %d\n", hdr.bufSzB);
      // recompute the sizes
	LogBufSzB = hdr.bufSzB;
	NEventsPerBuf = (LogBufSzB / sizeof(LogEvent_t)) - 1;
    }

  // compute the number of buffers in the log
    int numBufs = (fileSize / LogBufSzB) - 1;
    if (numBufs <= 0) {
	fprintf(stderr, "no buffers in file\n");
	return false;
    }

  // get the timestamp format and start time
    TimeCvtFn_t convertTime;
    if (hdr.tsKind == LOGTS_TIMEVAL) convertTime = TimevalCvt;
    else if (hdr.tsKind == LOGTS_TIMESPEC) convertTime = TimespecCvt;
    else convertTime = MachAbsoluteCvt;
    Time_t startTime = convertTime (hdr.startTime);

  // memory-map the rest of the file
    size_t szB = numBufs*LogBufSzB;
    void *addr = mmap (0, szB, PROT_READ, MAP_PRIVATE, fileno(f), LogBufSzB);
    if (addr == (void *)-1) {
	perror ("mmap");
	return false;
    }

  /* count the number of events per vproc */
    uint64_t *nEvents = new uint64_t[hdr.nVProcs];
    for (int i = 0;  i < hdr.nVProcs;  i++) {
	nEvents[i] = 0;
    }
    LogBuffer_t *buf = static_cast<LogBuffer_t *>(addr);
    for (int i = 0;  i < numBufs;  i++) {
	int n = (buf[i].next > NEventsPerBuf) ? NEventsPerBuf : buf[i].next;
	nEvents[buf[i].vpId] += n;
    }

  /* initialize the _traces */
    Event *next[hdr.nVProcs];
    this->_traces = new VProcTrace [hdr.nVProcs];
    for (int i = 0;  i < hdr.nVProcs;  i++) {
	nEvents[i] = 0;
	this->_traces[i].vprocId = i;
	this->_traces[i].cpuId = i;	// FIXME
	this->_traces[i].numEvents = nEvents[i];
	this->_traces[i].events = new Event [nEvents[i] + 2];
	next[i] = this->_traces[i].events;
	if (nEvents[i] == 0)
	    fprintf (stderr, "warning: no events for VProc %d\n", i);
    }

  /* initialize the start sentinel in the event arrays */
    EventAttrs *noEventAttrs = EventAttrsFactory (_filter.LogFileInfo(), 0);
    for (int i = 0;  i < hdr.nVProcs;  i++) {
	next[i]->ts = 0;
	next[i]->attrs = noEventAttrs;
	next[i]++;
    }

  /* initialize the events arrays */
    for (int i = 0;  i < numBufs;  i++) {
	int vp = buf[i].vpId;
	int n = (buf[i].next > NEventsPerBuf) ? NEventsPerBuf : buf[i].next;
	for (int j = 0;  j < n;  j++) {
	    LogEvent_t *lp = &(buf[i].log[j]);
	    next[i]->ts = convertTime(lp->timestamp) - startTime;
	    next[i]->attrs = EventAttrsFactory (this->_filter.LogFileInfo(), lp);
	    next[i]++;
	}
    }

  /* initialize the end sentinel in the event arrays */
    Time_t endTime = 0;
    for (int i = 0;  i < hdr.nVProcs;  i++) {
	if (endTime < this->_traces[i].events[this->_traces[i].numEvents].ts)
	    endTime = this->_traces[i].events[this->_traces[i].numEvents].ts;
    }
    for (int i = 0;  i < hdr.nVProcs;  i++) {
	next[i]->ts = endTime;
	next[i]->attrs = noEventAttrs;
    }

  /* fill in information for intervals and dependent events */
// FIXME

    munmap (addr, szB);
    fclose (f);

    return true;
}

Event *LogFile::Get (uint32_t vpID, uint64_t index)
{
    return &(this->_traces[vpID].events[index]);
}

Event *LogFile::Get (EventId_t id)
{
    uint32_t vpID = (uint32_t)(id >> LOG_MAX_NEVENTS);
    uint64_t index = (id & (MAX_NEVENTS-1));
    return &(this->_traces[vpID].events[index]);
}

