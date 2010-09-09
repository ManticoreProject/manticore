/** \file  Summary.h
 * \author Korei Klein
 * \date 8/13/09
 *
 *
 *
 */

#import "LogDoc.h"
@class LogData;
struct StateGroup;


/// Compute summary data about a logData object.
/** The Summary is the data object represented by the summary view beneath the logview.
 * A Summary computes summary data about the events in a log file, by reading that data from LogData.
 * Now it computes only various averages of the data averaged according to one of a set of averaging techniques.
 *
 * Summary makes the followig assumptions about LogData:
 * 0. There exists some resource that is consumed by various consumers.
 *	a. In a 1 second interval of time, there is some amount of resource to be consumed
 *	   In that second, the consumers will consume all of and not more than the amount of resource in that second.
 *
 * In the current implementation:
 *  The resource is a state group and the consumers are the states of that group.
 *  In every second, each vproc spends a total of 1 second collectively in those states.
 *
 *  The total amount of resource is then ( n * ( 1 second ) ) where n is the number of
 *  VProcs over which the summary is being taken.
 *
 * Summary computes an average by creating some number of pies, indexed by their positions in an array.
 * The pies with lower indecies should correspond to data earlier in the log file, and pies with later
 * indecies should correspond to data later in the log file.
 *
 * A pie is like a pie chart.  In a pie chart there are 2 * pi radians of resource and various consumers
 * take up their portions of those 2 * pi radians. See Pie.h for more information about pies.
 */
@interface Summary : NSObject {
    /// Feel free to change the type of resource if the need arises
    struct StateGroup *resource;


    /// The pies,
    /// each representing how the resource was used in an interval of length size.
    /// Pies is sorted chronologically
    NSMutableArray *pies;

    uint64_t size;
    // the logInterval over which we summarize
    struct LogInterval logInterval;

}

/// Use this method for creating new summary data, do not call allocation and init functions
+ (Summary *)coarseSummaryFromLogData:(LogData *)logData
			     forState:(StateGroup *)state
			     forVProc:(int32_t)vp
			     withSize:(uint64_t)s
			  andInterval:(struct LogInterval)interval
			    andNumber:(uint64_t)n;


@property (readonly) NSMutableArray *pies;

/// Feel free to change the return type if desired
@property (readonly) struct StateGroup *resource;

@property (readonly) uint64_t size;
@property (readonly) struct LogInterval logInterval;

/// For internal use
+ (Summary *)fineSummaryFromLogData:(LogData *)logData
			   forState:(StateGroup *)state
			   forVProc:(int32_t)vp
			   withSize:(uint64_t)size
			andInterval:(struct LogInterval)interval
			  andNumber:(uint64_t)n;

@end


