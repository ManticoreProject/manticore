/** \file VProc.h
 * \author Korei Klein
 * \date 7/10/09
 *
 * Define the representation of a virtual processor
 */

#import <Cocoa/Cocoa.h>
#import "log-file.h"
#import "DynamicEventRep.hxx"

/// The representation of a virtual processor
@interface VProc : NSObject {
    int32_t vpId; ///< The identifier for this VProc
    DynamicEvent (*events)[]; ///< A sorted array of the events relevant to this vProc
    int numEvents;
}

/// modify and access the events array
@property (readwrite, assign) DynamicEvent (*events)[];
/// modify and access the size of the events array
@property (readwrite, assign) int numEvents;

/// modify and access vpId
@property (readwrite, assign) int32_t vpId;

/// Initialize
- (VProc *)initWithVpId:(int32_t)vpIdVal;


@end
