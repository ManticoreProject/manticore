/* log-file.cxx
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "log-file.cxx"
#include <stdio.h>

/***** The representation of events *****/


/***** class LogFile members *****/

LogFile::LogFile (const char *logDescFileName)
{
    this->_nVProcs = 0;
    this->_startTime = 0;
    this->_endTime = 0;
    this->_events = 0;

    if (! this->Init(logDescFileName)) {
      // error loading log-description file
	exit (1);
    }

}

LogFile::~LogFile ();

// load a log file
//
bool LogFile::LoadFile (const char *file)
{
    if (this->_vProcs != 0) {
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

  // reset the input file pointer to the first block
    if (fseek(f, LogBufSzB, SEEK_SET) == -1) {
	perror ("fseek");
	return false;
    }

}

Event *LogFile::Get (EventId_t id);
Event *LogFile::Get (uint32_t vpID, uint32_t index);
