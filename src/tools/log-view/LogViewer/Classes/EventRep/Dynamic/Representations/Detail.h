/** \file Detail.h
 * \author Korei Klein
 * \date 7/10/09
 *
 *
 * Define the representation of dynamic events and details.
 *
 * IMPORTANT:
 *	None of LogView or the other rendering classes
 *	are allowed to access the fields of these structures directly.
 *	Instead, they must be accessed using the functions defined in DetailAccess.h.
 *	This is to enforce a layer of abstraction which will make the following statement true:
 *	    if a developer would like to use an alternate representation of events
 *	    then he must implement new code to load events into a LogData object,
 *	    and he must implement all the functions in DetailAccess.h,
 *	    but he need not modify any of the rendering code.
 *
 */



#import "log-file.h"
struct Group;

/// The amount by which to grow the array of dsts for a dependent detail
/// when it runs out of space
#define DSTS_ARRAY_INC ( 5 )

struct event_struct
{
    uint64_t timestamp; ///< time stamp (8 bytes)
    struct struct_log_event value; ///< The original event as read from the logfile
};

/**
 * An event is meant to represent something that happened
 * to a processor at A SINGLE POINT IN TIME.
 * When the log file is loaded into LogData, events will
 * be in 1-to-1 correspondence with LogEvent_t as defined in log-file.h
 *
 * Every detail d is defined by m events,
 *	where m is { 1 if d is simple
 *		     k + 1 for some natural number k if d is dependent
 *		     2 OR 1 if d is interval:
 *			m == 1 if d never ends
 *			m == 2 if d ends
 *		     <= 2 if d is state:
 *
 *			mostly m will be 2, but if d is the first OR last state detail s
 *			for a StateGroup g then m <= 1.
 *
 *			m == 0 if d is the first AND last
 *			state detail s for a StateGroup g
 *
 *			m == 1 if d is the first XOR last
 *			state detail s for a StateGroup g
 *		   }
 *
 * Also, a single event e can be one of defining events for n details for any positive natural number n.
 * Thus events and details are in a m-to-n correspondence.
 */
typedef struct event_struct event;

/**
 * A State_Detail represents the fact that a vproc v was in state s
 * of StateGroup g for an interval of time (start, end).
 * Iff start == NULL then the interval started at -infinity
 * Iff end == NULL then the interval ended at infinity.
 * At a single point in time, a vproc v will be in precisely n states:
 *	s1, s2, ... sn
 * where n is the number of StateGroups g1, g2 ... gn defined in LogFileDesc.
 *	 and si is one of the states of group gi forall i
 */
struct State_Detail
{
    int state;
    event *start; ///< NULL iff this State_Detail starts at the begginning of the file
    event *end; ///< NULL iff this State_Detail ends at the end of the file
};


/** A simple detail s represents that vproc v performed action g at time value->timestamp
 *	where g is the EventGroup containing s
 * A simple detail is the unique type of detail that DOES NOT occur
 * over an interval of time.  A simple detail occurs instantaneously,
 * and is defined by a single event.
 */
struct Simple_Detail
{
    event *value;
};

/** An interval detail i represents that vproc v was doing action g
 * during the time (start, end)
 * where
 *  if end == NULL then the interval ended at infinity
 *  g is the EventGroup containing i
 *
 */
struct Interval_Detail
{
    double height;
    event *start; ///< Not NULL
    event *end;   ///< NULL iff this Interval_Detail ends at the end of the file
};

/** A dependent destination dd is really only a part of a detail.
 * dd represents that vproc vpId received a message from vproc V at time t
 */
struct Dependent_Dst
{
    int32_t vpId;
    event *value;
};


/** A dependent detail d [in state 2 (see below)] represents that vproc vpId sent
 * message g to n_dsts vprocs, where g is the EventGroup
 * containing d.
 *
 * dsts is an array of the destinations.
 * the size of dsts is dsts_array_size, but only
 * n_dsts of its elements should ever be filled in.
 */
struct Dependent_Detail
{
    event *src; ///< Not NULL
    int32_t vpId;

    // The following three fields can exists in one of two states
    // 1. dsts_array_size == dsts == 0
    //	this implies that there are no destination events
    // then the value of n_dsts is undefined
    // 2. dsts != 0 < n_dsts <= dsts_array_size
    //	this implies that there are n_dsts destination events, in the first
    //	n_dsts indexes in dsts[dsts_array_size]
    //
    // You can check which state you are in with
    // if n_dsts == 0 then 1. else 2.
    struct Dependent_Dst **dsts;
    int n_dsts;
    int dsts_array_size;

};

union Detail_union
{
    struct Simple_Detail simple;
    struct State_Detail state;
    struct Interval_Detail interval;
    struct Dependent_Detail dependent;
};

struct TaggedDetail_struct
{
    struct Group *type;
    // type->kind() determines which field of data is filled in
    // according to the obvious mapping from kinds to fields of a Detail_union.
    struct EventDesc *eventDesc;
    union Detail_union data;
};

typedef struct TaggedDetail_struct *Detail;






///////////////////// OLD VERSION //////////////////////


// The current representation of Dynamic Events.
/* A single dynamic event corresponds to exactly one instance of a LogEvent_t
 * found in a single log file.
 *
 * This structure is not part of the interface to events.
 * It is highly subject to change.  Use the interface in DynamicEventRep.h to interact with events.

struct DynamicEvent_struct
{

    // Fields common to all DynamicEvents

    struct EventDesc *desc; ///< The description of this type of event.  Should be an EventDesc
    **< You can determine if a DynamicEvent e is a:
     * -# simple
     * -# interval
     * -# message
     * -# state
     * -# some of the above
     * event by checking the event group of e->desc
     *
    uint64_t timestamp; ///< time stamp (8 bytes)
    struct struct_log_event value; ///< The original event as read from the logfile
};

/// The DynamicEvent type is exposed in the interface, but not the DynamitEvent_struct
typedef DynamicEvent_struct DynamicEvent;


/// The time the event was logged in nanoseconds
static inline uint64_t timeStamp(DynamicEvent event, LogFileDesc *desc)
{
    return event.timestamp;
}

/// The static version of this event
static inline EventDesc *description(DynamicEvent event, LogFileDesc *desc)
{
    return event.desc;
}

/// Recover the arguments of this event
static inline ArgValue getArg(DynamicEvent event, LogFileDesc *desc, int argNum)
{
    return event.desc->GetArg(&event.value, argNum);
}



 
 
#pragma mark References Accessors


 
/// Get this event's destinations.  The event must be a message source event.
static inline DynamicEvent **getRefDsts(DynamicEvent e)
{
    return (DynamicEvent **) (e.references.dsts);
}

/// Get this event's source.  The event must be a message destination event.
static inline DynamicEvent *getRefSrc(DynamicEvent e)
{
    return e.references.src;
}

/// Get this event's start.  The event must be an interval end event.
static inline DynamicEvent *getRefStart(DynamicEvent e)
{
    return e.references.start;
}

/// Get this event's end.  The event must be a interval start event.
static inline DynamicEvent *getRefEnd(DynamicEvent e)
{
    return e.references.end;
}



 *
 * NOTE: To allow for the functions defined here to be static and lined,
 * this file will define both the interface to events and the current choice of representation.
 * The interface consists of the function prototypes and the fact that a Detail abstract type
 * is defined.
 *
 * The implementation consists of the type Detail is defined to be, along with the bodies of
 * of the functions, and the implementation of LogFile in LogFile.mm.
*/

