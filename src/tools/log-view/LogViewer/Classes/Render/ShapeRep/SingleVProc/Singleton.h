/*! \file Singleton.h
 \author Korei Klein
 \date 7/7/09
 */

#import <Cocoa/Cocoa.h>
#import "EventShape.h"
#import "Detail.h"

/// Default color of singleton events
#define DEFAULT_SINGLETON_COLOR ([NSColor yellowColor])
/// Height of diamond
#define DIAMOND_HEIGHT (14)
/// Width of diamond
#define DIAMOND_WIDTH (7)
/// Width of the border of a diamond
#define DEFAULT_LINE_WIDTH (.6)



/// Represents a singleton event as a diamond
@interface Singleton : EventShape {
	NSPoint place; //!< point at which to display the diamond
	NSBezierPath *path; //!< the diamond (a cache of place).

    event *eventVal;
}

@property (readonly) event *eventVal;

/// Initialize
- (Singleton *)initWithPoint:(NSPoint)p
		       color:(NSColor *)c
		       event:(event *)eventVal;


@end
