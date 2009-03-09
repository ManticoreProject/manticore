/*! \file log-file.hxx
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _LOG_FILE_HXX_
#define _LOG_FILE_HXX_

#include <stdint.h>

typedef uint64_t Time_t;		//!< \brief Time measured in nanoseconds
typedef uint32_t EventId_t;		//!< \brief ID of an event occurrence; composed from
					//! VProc ID and index.

#define LOG_MAX_NVPROCS		6	//! \brief 2^6 vprcs is the limit
#define LOG_MAX_NEVENTS		(32-LOG_MAX_NVPROCS)
#define MAX_NVPROCS		(1<<LOG_MAX_NVPROCS)
#define MAX_NEVENTS		(1<<LOG_MAX_NEVENTS)	//!< \brief maximum number of events
							//! per vproc.

#include "log-file.h"
#include "view-filter.hxx"

struct Event {
    EventDesc	*desc;		//!< the description of the event
    LogEvent_t	*evt;		//!< the raw event data from the memory-mapped file
};

class LogFile {
  public:

    explicit LogFile (const char *logDescFileName);
    ~LogFile ();

    bool LoadFile (const char *file);

    Event *Get (EventId_t id);
    Event *Get (uint32_t vpID, uint32_t index);

    int NumVProcs () const { return this->_nVProcs; }
    Time_t Duration () const { return this->_endTime - this->_startTime; }

  private:
    int		_nVProcs;	//!< \brief the number of VProcs in he run
    Time_t	_startTime;	//!< \brief timestamp of the log's start
    Time_t	_endTime;	//!< \brief timestamp of the log's end
    uint64_t	*_nEvents;	//!< \brief the number of events per vproc
    Event	**_events;	//!< \brief event logs; one per vproc
    ViewFilter	_filter;	//!< \brief controls visibility of events
    LogBuffer_t	*_raw;		//!< \brief the memory-mapped log file
    size_t	_rawSzB;	//!< \brief the size of the memory-mapped log file (in bytes)
};

#endif /* !_LOG_FILE_HXX_ */
