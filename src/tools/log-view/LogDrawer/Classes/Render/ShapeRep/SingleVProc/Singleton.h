/*! \file Singleton.h
 \author Korei Klein
 \date 7/7/09
 */

#import <Cocoa/Cocoa.h>
#import "EventShape.h"


/// Default color of singleton events
#define DEFAULT_SINGLETON_COLOR ([NSColor yellowColor])
/// Height of diamond
#define DIAMOND_HEIGHT (14)
/// Width of diamond
#define DIAMOND_WIDTH (7)
/// Width of the border of a diamond
#define DEFAULT_LINE_WIDTH (.6)

/// Represents an singleton event as a diamond
@interface Singleton : EventShape {
	NSPoint place; //!< point at which to display the diamond
	NSBezierPath *path; //!< the diamond (a cache of place).
	
	void *start; //!< The corresponding event
}

@property (readonly) void *start;

/// Testing function.  Generates a warining.
- (Singleton *)initWithX:(CGFloat)x;
///Initialize
/*! Initialize
 \param p point at which to display the diamond
 \param c color of the diamond
 \param s event corresponding to this diamond

 \return the initialized object
 */
- (Singleton *)initWithPoint:(NSPoint)p color:(NSColor *)c start:(void *)s;


@end
