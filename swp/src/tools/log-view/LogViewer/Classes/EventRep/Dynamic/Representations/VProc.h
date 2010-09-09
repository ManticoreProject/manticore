/** \file VProc.h
 * \author Korei Klein
 * \date 7/10/09
 *
 * Define the representation of a virtual processor.
 *
 */

#import <Cocoa/Cocoa.h>
#import "log-file.h"
#import "Detail.h"

@class StateMap;
@class IntervalMap;
@class DependentMap;
struct EventDesc;
struct DependentGroup;

struct LogFileDesc;




/// The representation of a virtual processor
/** Most, but not all of the data in a log file will be represented by VProcs.
 * Specifically:
 *  Each state, simple, and interval event will be represented by the VProc to which is belongs.
 *  Dependent events, however, do not belong to a single vproc.  They are properties of the interaction
 *  between vprocs.  For this reason, Dependent events are represented in the LogData object.
 *
 * VProcs are model objects, they exist independent of the UI.
 */
@interface VProc : NSObject {
    int32_t vpId; ///< The identifier for this VProc

    /// A sorted array of the events (state, interval, simple) relevant to this vProc. (Not dependent events)
    event **events;
    uint64_t numEvents; //< number of events in events

    /// An array of details (state, interval, simple) relevant to this vProc. (Not dependent events)
    /** Note:
     *	    There is no natural order to impose on details, they are at most partially ordered.
     *	    Without defining a way to order details, there is no way to keep them sorted.
     *
     *	The partial order on details is defined by:
     *	        (a < b)  iff
     *		(let A be the set of all timestamps of events corresponding to a
     *		 let B be the set of all timestamps of events corresponding to b
     *		 all elements of A are less than all elements of B)
     *	
     *	details will be sorted with respect to this partial order.
     *	do not make any other assumptions about the order of details in the array.
     *
     *	OPTIMIZE: There is a slight inefficiency in the way details are represented:
     *	    Given an interval of time (s, f) suppose you wish to find the set S of all details d such that
     *	    the time span of d intersects (s, f).
     *
     *	    To compute this set S of details correctly will require time linear in numDetails.
     *		This is because every detail must be tested for intersection with (s, f)
     *		    This is because:
     *			suppose the set of details is partitioned into two sets D and {d}
     *			where {d} contains only d, and D contains the rest of the details.
     *			(Knowing the partial order on details, and knowing all there is no know about D,
     *			and knowing the position of d in the array of details) does not determine if
     *			the time span of d intersects (s, f)
     *
     *	    It may be possible to compute a decent approximation of S more efficiently using
     *		the partial order on details
     *		some reasonable assumptions about details
     *		an algorithm based on binary search
     *
     *	 Here is the asymptotically fastest actual solution (not an approximation) that I know of:
     *	    There exists a more efficient representation r of details.
     *	    r is based on binary trees.
     *	    given r, you can compute S in O( log-base-2(numDetails) ) time
     *
     *	    the idea behind r is:
     *		r is a tree with a root node, and where every node is either a leaf or has a left and right branch
     *		each node represents an interval of time
     *		each node will store some number of details
     *		the root represents the entire file
     *		if n is a node with left and right branches a and b, and n represents the interval (s, f)
     *		    then a represents (s, (f - s ) / 2) and b represents ( (f - s) / 2, f )
     *		define a function p from nodes to times, where if n represents (s, f) then p(n) = (f - s) / 2
     *		to store a detail d in the tree, store it at the node n of least depth such that p(n) is in the
     *		    time span of d
     *
     *	    r can be created from LogData in sort of linear time, given some reasonable assumptions
     */
    Detail *details;
    uint64_t numDetails;

    /// self represents the interval of time (start, end)
    uint64_t start;
    uint64_t end;

    /// LogFileDesc for the json description files
    struct LogFileDesc *logDesc;

    /// header of the log file which self's data comes from
    LogFileHeader_t *header;
}

/// Initialization.  Use only this initializer, no others
- (VProc *)initWithVpId:(uint32_t)i
		   events:(event **)eventsVal numEvents:(uint64_t)n
		  details:(Detail *)detailsVal numDetails:(uint64_t)m
		  logDesc:(struct LogFileDesc *)logDescVal
		    start:(uint64_t)firstTime end:(uint64_t)lastTime
		   header:(LogFileHeader_t *)hdr;

/// modify and access the events array
@property (readwrite, assign) event **events;
@property (readwrite, assign) Detail *details;
/// modify and access the size of the events array
@property (readonly) uint64_t numEvents;
@property (readonly) uint64_t numDetails;

/// modify and access vpId
@property (readwrite, assign) int32_t vpId;


@end


































/* Old version
/// The representation of a virtual processor
@interface VProc : NSObject {
    int32_t vpId; ///< The identifier for this VProc

    event *events; ///< A sorted array of the events relevant to this vProc
    int numEvents; //< number of events in events
    int events_size; //< size of events

    // Workaround and safety mechanism
    // This array is used to detect when two calls
    // to readBlock pass in the same block.
    // Any two calls to readBlock must pass different blocks,
    // but in case they don't first_event_times can be used to check.
    NSMutableArray *first_event_times;

    Detail *details;
    int numDetails;
    int details_size;

    uint64_t start;
    uint64_t end;

    struct LogFileDesc *logDesc;
    LogFileHeader_t *header;

    StateMap *stateMap;
    IntervalMap *intervalMap;
    DependentMap *dependentMap;
}

- (VProc *)initWithEvents:eventsVal numEvents:n
		  details:detailsVal numDetails:m
		  logDesc:logDescVal
		    start:firstTime end:lastTime
		   header:hdr;

/// modify and access the events array
@property (readwrite, assign) event *events;
@property (readwrite, assign) Detail *details;
/// modify and access the size of the events array
@property (readonly) int numEvents;
@property (readonly) int numDetails;

/// Check the times of the first and last events for this vproc
@property (readonly) uint64_t start;
@property (readonly) uint64_t end;

/// modify and access vpId
@property (readwrite, assign) int32_t vpId;

/// Initialize
- (VProc *)initWithLog:(LogBuffer_t *)logBuffer
	    andLogDesc:(struct LogFileDesc *)logDesc
		header:(LogFileHeader_t *)headerVal
	     numEvents:(int)n
	     allStates:(NSArray *)allStates
	  dependentMap:(DependentMap *)dependentMapVal;


- (void)readBlock:(LogBuffer_t *)logBuffer
	numEvents:(int)n;

- (void)read:(int)n eventsFrom:(LogEvent_t *)array;

- (void) addEvent:(event *)e
    withEventDesc:(struct EventDesc *)eventDesc
andDependentGroup:(struct DependentGroup *)g
	 toDetail:(struct Dependent_Detail *)d;


@end
 */
