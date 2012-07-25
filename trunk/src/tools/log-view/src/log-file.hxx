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
typedef uint64_t EventId_t;		//!< \brief ID of an event occurrence; composed from
					//! VProc ID and index.

#define LOG_MAX_NVPROCS		8	//! \brief 2^8 vprcs is the limit
#define LOG_MAX_NEVENTS		(64-LOG_MAX_NVPROCS)
#define MAX_NVPROCS		(1 << LOG_MAX_NVPROCS)
#define MAX_NEVENTS		((uint64_t)1 << LOG_MAX_NEVENTS)
					//!< \brief maximum number of events per vproc.

#include "log-file.h"
#include "view-filter.hxx"

//!\brief the base class for extra event information, including arguments, matching ends
//! of intervals, and dependencies.
//
class EventAttrs {
  public:
    EventDesc *Desc () const	{ return this->_desc; }

    int NArgs () const		{ return this->_desc->NArgs(); }

    virtual TaggedArgValue Arg (int i) const;
    virtual ~EventAttrs ();

  protected:
    EventDesc	*_desc;

    EventAttrs (EventDesc *d)	{ this->_desc = d; }
};

//!\brief attributes for events that have kind #LOG_EVENT and do not have any
//! arguments.
//
class SimpleEventAttrs : public EventAttrs {
  public:
    SimpleEventAttrs (EventDesc *d) : EventAttrs(d) { }
    ~SimpleEventAttrs () { }
};

//!\brief attributes for events that have kind #LOG_EVENT and have arguments.
//
class WithArgsAttrs : public EventAttrs {
  public:
    WithArgsAttrs (EventDesc *d, LogEvent_t *rawEvt);
    virtual ~WithArgsAttrs ();
    TaggedArgValue Arg (int i) const;
  protected:
    ArgValue *_args;
};

//!\brief attributes for an interval events without arguments.
//
class IntervalEventAttrs : public EventAttrs {
  public:
    IntervalEventAttrs (EventDesc *d);
    ~IntervalEventAttrs () { };
  protected:
    uint64_t _start;			//!< start index of interval
    uint64_t _end;			//!< end index of interval
};

//!\brief attributes for an interval events with arguments.
//
class IntervalWithArgsAttrs : public WithArgsAttrs {
  public:
    IntervalWithArgsAttrs (EventDesc *d, LogEvent_t *rawEvt);
    ~IntervalWithArgsAttrs () { };
  protected:
    uint64_t _start;			//!< start index of interval
    uint64_t _end;			//!< end index of interval
};

//!\brief attributes for events that have kind #LOG_SRC or #LOG_DST.  These will
//! always have arguments, since they have an ID.
//
class DependentEventAttrs : public WithArgsAttrs {
  public:
    DependentEventAttrs (EventDesc *d, LogEvent_t *rawEvt);
    ~DependentEventAttrs () { };
  protected:
    uint64_t _id;			//!< runtime ID of source event
    EventId_t _other;			//!< in-memory ID of other end of dependency
};

//! \brief the in-memory representation of an event from the log file.
//
struct Event {
    Time_t		ts;		//!< \brief the normalized timestamp of the event
    EventAttrs		*attrs;		//!< additional information about the event

    Time_t Time () const		{ return this->ts; }
    EventDesc *Desc () const		{ return this->attrs->Desc(); }
    uint32_t EventCode () const		{ return this->Desc()->Id(); }
    EventKind Kind() const		{ return this->Desc()->Kind(); }
};

//! \brief the sequence of events for a given vproc sorted in increasing-timestamp order
//
struct VProcTrace {
    uint32_t		vprocId;
    uint32_t		cpuId;
    uint64_t		numEvents;	//!< the number of events in the array
    Event		*events;	//!< the array of events sorted by timestamp.
					//! there are numEvents+2 items in this array, with
					//! sentinels at each end of the array.

  //! \brief return the timestamp of the first event in the array
    Time_t TimeOfFirstEvent () const	{ return this->events[1].ts; }
  //! \brief return the timestamp of the last event in the array
    Time_t TimeOfLastEvent () const	{ return this->events[this->numEvents].ts; }

  //! \brief return the index of the events array that immediately precedes the time \arg t
  //! \param t the time to index the array by; we assume that t is within the run.
  //! \return the largest index i, such that events[i].ts <= t.
    int64_t IndexByTimestamp (Time_t t) const;

    VProcTrace () { }
    ~VProcTrace () { delete this->events; }
};

//! \brief this class contains the log-file data from a single log file.
//
class LogFile {
  public:

    explicit LogFile (const char *logDescFileName);
    ~LogFile ();

    bool LoadFile (const char *file);

    Event *Get (EventId_t id);
    Event *Get (uint32_t vpID, uint64_t index);

    int NumVProcs () const { return this->_nVProcs; }
    Time_t Duration () const { return this->_endTime - this->_startTime; }

  private:
    int		_nVProcs;	//!< the number of VProcs in he run
    Time_t	_startTime;	//!< timestamp of the log's start
    Time_t	_endTime;	//!< timestamp of the log's end
    VProcTrace	*_traces;	//!< the event traces for each vproc.
    uint64_t	_totalNEvents;	//!< total number of events in the log
    uint64_t	*_timeline;	//!< event IDs sorted by timestamp
    ViewFilter	_filter;	//!< controls visibility of events
};

#endif /* !_LOG_FILE_HXX_ */
