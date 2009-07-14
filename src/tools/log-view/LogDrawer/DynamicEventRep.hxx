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
 * of the functions.
 *
 */

//#import "event-desc.hxx"
#import "log-file.h"
#import "event-desc.hxx"
#import "log-desc.hxx"
#define STATIC_INLINE static inline




/// The current representation of Dynamic Events.
/** A single dynamic event corresponds to exactly one instance of a LogEvent_t
 * found in a single log file.
 *
 * This structure is not part of the interface to events.
 * It is highly subject to change.  Use the interface in DynamicEventRep.h to interact with events.
 */
typedef struct DynamicEvent_struct
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

    // Fields dependent of the group(s) of the DynamicEvent
    /// the references field contains pointers to events which this event is related to
    union _DynamicEventReferences_t {
	struct _DynamicEvent *(*dsts)[]; ///< For a Depenedent Event which is a source
	struct _DynamicEvent *src; ///< For a Dependent Event which is a destination
	struct _DynamicEvent *end; ///< For an Interval Event which is a start
	struct _DynamicEvent *start; ///< For an Interval Event which is an end
	// simple and state events do not have any references
    } references;
} DynamicEvent;

/// The time the event was logged in nanoseconds
STATIC_INLINE uint64_t timeStamp(DynamicEvent event, LogFileDesc *desc)
{
    return event.timestamp;
}

/// The static version of this event
STATIC_INLINE EventDesc *desc(DynamicEvent event, LogFileDesc *desc)
{
    return event.desc;
}

/// Recover the arguments of this event
STATIC_INLINE ArgValue getArg(DynamicEvent event, LogFileDesc *desc, int argNum)
{
    return event.desc->GetArg(&event.value, argNum);
}


