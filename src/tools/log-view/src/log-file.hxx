/* log-file.hxx
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _LOG_FILE_HXX_
#define _LOG_FILE_HXX_

typedef uint64_t Time_t;		//!< \brief Time measured in nanoseconds
typedef uint32_t EventId_t;		//!< \brief ID of an event occurrence; composed from
					//! VProc ID and index.

#define LOG_MAX_NVPROCS		6	//! \brief 2^6 vprcs is the limit
#define MAX_NVPROCS		(1<<LOG_MAX_NVPROCS)
#define MAX_NEVENT		(1<<(32-LOG_MAX_NVPROCS))	//!< \brief maximum number of events
								//! per vproc.

#include "view-filter.hxx"

struct Event;

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
    Time_t	_startTime;	//!< \brief timestamp of the log's start (in nanoseconds)
    Time_t	_endTime;	//!< \brief timestamp of the log's end (in nanoseconds)
    Event	**_events;	//!< \brief event logs; one per vproc
    ViewFilter	_filter;	//!< \brief controls visibility of events
};

#endif /* !_LOG_FILE_HXX_ */
