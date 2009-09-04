/** \file  EventArg.h
 * \author Korei Klein
 * \date 8/7/09
 *
 */

#import <Cocoa/Cocoa.h>
#import "log-file.h"

/// Model object for use in eventInfoView. Represents an argument to an event.
@interface EventArg : NSObject {
    NSString *name;
    NSString *type;
    NSString *description;
    NSString *value;
    NSString *size;
}

@property (readwrite, assign) NSString *name;
@property (readwrite, assign) NSString *type;
@property (readwrite, assign) NSString *description;
@property (readwrite, assign) NSString *value;
@property (readwrite, assign) NSString *size;

/// Initialize
- (EventArg *)initWithArgDesc:(struct ArgDesc)argDesc
		  andArgValue:(union ArgValue)argValue;

@end
