/** \file Detail.h
 * \author Korei Klein
 * \date 7/10/09
 *
 *
 * Define the representation of dynamic events.
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

typedef struct event_struct event;

struct State_Detail
{
    int state;
    event *start; ///< NULL iff this State_Detail starts at the begginning of the file
    event *end; ///< NULL iff this State_Detail ends at the end of the file
};

struct Simple_Detail
{
    event *value;
};

struct Interval_Detail
{
    double height;
    event *start; ///< Not NULL
    event *end;   ///< NULL iff this Interval_Detail ends at the end of the file
};

struct Dependent_Dst
{
    int32_t vpId;
    event *value;
};



struct Dependent_Detail
{
    event *src; ///< Not NULL
    int32_t vpId;

    // The following three fields can exists in one of two states
    // 1. n_dsts == dsts_array_size == dsts == 0
    //	this implies that there are no destination events
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
STATIC_INLINE uint64_t timeStamp(DynamicEvent event, LogFileDesc *desc)
{
    return event.timestamp;
}

/// The static version of this event
STATIC_INLINE EventDesc *description(DynamicEvent event, LogFileDesc *desc)
{
    return event.desc;
}

/// Recover the arguments of this event
STATIC_INLINE ArgValue getArg(DynamicEvent event, LogFileDesc *desc, int argNum)
{
    return event.desc->GetArg(&event.value, argNum);
}



 
 
#pragma mark References Accessors


 
/// Get this event's destinations.  The event must be a message source event.
STATIC_INLINE DynamicEvent **getRefDsts(DynamicEvent e)
{
    return (DynamicEvent **) (e.references.dsts);
}

/// Get this event's source.  The event must be a message destination event.
STATIC_INLINE DynamicEvent *getRefSrc(DynamicEvent e)
{
    return e.references.src;
}

/// Get this event's start.  The event must be an interval end event.
STATIC_INLINE DynamicEvent *getRefStart(DynamicEvent e)
{
    return e.references.start;
}

/// Get this event's end.  The event must be a interval start event.
STATIC_INLINE DynamicEvent *getRefEnd(DynamicEvent e)
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

