/** \file VProc.h
 * \author Korei Klein
 * \date 7/10/09
 *
 * Define the representation of a virtual processor
 */

#import <Cocoa/Cocoa.h>
#import "log-file.h"
#import "DynamicEventRep.h"


/// The representation of a virtual processor
@interface VProc : NSObject {
    int32_t vpId; ///< The identifier for this VProc
    DynamicEvent (*events)[]; ///< A sorted array of the events relevant to this vProc
}

@end
