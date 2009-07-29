/** \file DynamicEventRep.h
 * \author Korei Klein
 * \date 7/10/09
 *
 *
 * Define the representation of dynamic events.
 *
 * NOTE: To allow for the functions defined here to be static and lined,
 * this file will define both the interface to events and the current choice of representation.
 * The interface consists of the function prototypes and the fact that a DynamicEvent abstract type
 * is defined.
 *
 * The implementation consists of the type DynamicEvent is defined to be, along with the bodies of
 * of the functions, and the implementation of LogFile in LogFile.mm.
 *
 */

//#import "event-desc.hxx"
#import "log-file.h"
#import "event-desc.hxx"
#import "log-desc.hxx"
#define STATIC_INLINE static inline
struct LogFileDesc;


/// The current representation of Dynamic Events.
/** A single dynamic event corresponds to exactly one instance of a LogEvent_t
 * found in a single log file.
 *
 * This structure is not part of the interface to events.
 * It is highly subject to change.  Use the interface in DynamicEventRep.h to interact with events.
 */
struct DynamicEvent_struct
{

    // Fields common to all DynamicEvents

    struct EventDesc *desc; ///< The description of this type of event.  Should be an EventDesc
    /**< You can determine if a DynamicEvent e is a:
     * -# simple
     * -# interval
     * -# message
     * -# state
     * -# some of the above
     * event by checking the event group of e->desc
     */
    uint64_t timestamp; ///< time stamp (8 bytes)
    struct struct_log_event value; ///< The original event as read from the logfile
};

/// The DynamicEvent type is exposed in the interface, but not the DynamitEvent_struct
typedef DynamicEvent_struct DynamicEvent;

STATIC_INLINE struct struct_log_event dynamicEventValue(DynamicEvent e)
{
    return e.value;
}

/// The time the event was logged in nanoseconds
STATIC_INLINE uint64_t dynamicEventTimeStamp(DynamicEvent event, struct LogFileDesc *desc)
{
    return event.timestamp;
}

/// The static version of this event
STATIC_INLINE EventDesc *dynamicEventDescription(DynamicEvent event, struct LogFileDesc *desc)
{
    return event.desc;
}

/// Recover the arguments of this event
STATIC_INLINE ArgValue dynamicEventGetArg(DynamicEvent event, struct LogFileDesc *desc, int argNum)
{
    return event.desc->GetArg(&event.value, argNum);
}


/*
 
 
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
*/

