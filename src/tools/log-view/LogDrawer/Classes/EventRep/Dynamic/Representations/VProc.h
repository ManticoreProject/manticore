/** \file VProc.h
 * \author Korei Klein
 * \date 7/10/09
 *
 * Define the representation of a virtual processor
 */

#import <Cocoa/Cocoa.h>
#import "log-file.h"
#import "Detail.h"
#import "VProcMaps.h"

struct LogFileDesc;

/// The representation of a virtual processor
@interface VProc : NSObject {
    int32_t vpId; ///< The identifier for this VProc
    
    event *events; ///< A sorted array of the events relevant to this vProc
    int numEvents; //< number of events in events
    int events_size; //< size of events
    
    Detail *details;
    int numDetails;
    int details_size;
    
    struct LogFileDesc *logDesc;
    LogFileHeader_t *header;
    
    StateMap *stateMap;
    IntervalMap *intervalMap;
    DependentMap *dependentMap;
}

/// modify and access the events array
@property (readwrite, assign) event *events;
@property (readwrite, assign) Detail *details;
/// modify and access the size of the events array
@property (readwrite, assign) int numEvents;

/// modify and access vpId
@property (readwrite, assign) int32_t vpId;

/// Initialize
- (VProc *)initWithLog:(LogBuffer_t *)logBuffer
	    andLogDesc:(struct LogFileDesc *)logDesc
		header:(LogFileHeader_t *)headerVal
	     numEvents:(int)n
	     allStates:(NSArray *)allStates;


- (void)readBlock:(LogBuffer_t *)logBuffer
	numEvents:(int)n;

- (void)read:(int)n eventsFrom:(LogEvent_t *)array;

@end
