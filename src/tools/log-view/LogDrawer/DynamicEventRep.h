/** \file DynamicEventRep.h
 * \author Korei Klein
 * \date 7/10/09
 *
 *
 * Define the representation of dynamic events.
 */

//#import "event-desc.hxx"
#import "log-file.h"




/// The representation of Dynamic Events.
/** A single dynamic event corresponds to exactly one instance of a LogEvent_t
 * found in a single log file. */
typedef struct _DynamicEvent
{

    // Fields common to all DynamicEvents

	void *desc; ///< The description of this type of event.  Should be an EventDesc
    	/**< You can determine if a DynamicEvent e is a:
    	 * -# simple
    	 * -# interval
    	 * -# message
    	 * -# state
    	 * -# some of the above
    	 * event by checking the event group of e->desc
    	 */
    	LogTS_t timestamp; ///< time stamp (8 bytes)
    	uint32_t data[5]; ///< upto 20 bytes of extra data

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


