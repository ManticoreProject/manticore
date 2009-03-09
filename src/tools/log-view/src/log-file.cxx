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


/***** class LogFile members *****/

LogFile::LogFile (const char *logDescFileName)
{
    this->_nVProcs = 0;
    this->_startTime = 0;
    this->_endTime = 0;
    this->_events = 0;
    this->_raw = 0;
    this->_rawSzB = 0;

    if (! this->_filter.Init(logDescFileName)) {
      // error loading log-description file
	exit (1);
    }

}

LogFile::~LogFile ()
{
    if (this->_nVProcs != 0) {
	for (int i = 0;  i < this->_nVProcs;  i++)
	    delete this->_events[i];
	delete this->_nEvents;
	delete this->_events;
	munmap (this->_raw, this->_rawSzB);
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

  // memory-map the rest of the file
    size_t szB = numBufs*LogBufSzB;
    void *addr = mmap (0, szB, PROT_READ, MAP_PRIVATE, fileno(f), LogBufSzB);
    if (addr == (void *)-1) {
	perror ("mmap");
	return false;
    }
    this->_raw = static_cast<LogBuffer_t *>(addr);
    this->_rawSzB = szB;

  /* count the number of events per vproc */
    this->_nEvents = new uint64_t[hdr.nVProcs];
    this->_events = new Event *[hdr.nVProcs];
    for (int i = 0;  i < hdr.nVProcs;  i++) {
	this->_nEvents[i] = 0;
	this->_events[i] = 0;
    }
    LogBuffer_t *buf = this->_raw;
    for (int i = 0;  i < numBufs;  i++) {
	int n = (buf[i].next > NEventsPerBuf) ? NEventsPerBuf : buf[i].next;
	this->_nEvents[buf[i].vpId] += n;
    }

  /* initialize the events arrays */
    Event *next[hdr.nVProcs];
    for (int i = 0;  i < hdr.nVProcs;  i++) {
	if (this->_nEvents[i] > 0) {
	    this->_events[i] = new Event[this->_nEvents[i]];
	    next[i] = this->_events[i];
	}
	else
	    fprintf (stderr, "warning: no events for VProc %d\n", i);
    }
    for (int i = 0;  i < numBufs;  i++) {
	int vp = buf[i].vpId;
	int n = (buf[i].next > NEventsPerBuf) ? NEventsPerBuf : buf[i].next;
	for (int j = 0;  j < n;  j++) {
	    LogEvent_t *lp = &(buf[i].log[j]);
	    next[i]->desc = this->_filter.FindEventById (lp->event);
	    next[i]->evt = lp;
	    next[i]++;
	}
    }

    fclose (f);

    return true;
}

Event *LogFile::Get (uint32_t vpID, uint32_t index)
{
    return &(this->_events[vpID][index]);
}

Event *LogFile::Get (EventId_t id)
{
    uint32_t vpID = (id >> LOG_MAX_NEVENTS);
    uint32_t index = (id & (MAX_NEVENTS-1));
    return &(this->_events[vpID][index]);
}

