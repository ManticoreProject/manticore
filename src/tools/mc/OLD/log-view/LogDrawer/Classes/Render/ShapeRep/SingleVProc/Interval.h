/*! \file Interval.h
 \author Korei Klein
 \date 7/7/09
 Define an object to represent the shape of an interval event

*/

#import <Cocoa/Cocoa.h>
#import "EventShape.h"

/// Represents an interval event by drawing a rectangle
@interface Interval : EventShape {
	NSRect rect; //!< Rectangle representing the event. Not null.
	NSBezierPath *roundedRect; //!< Rounded rectangle (a cache of rect). Not null.
	
	void *start; //!< The event corresponding to the begginig of the interval. Not null.
	void *end; //!< The event corresponding to the end of the interval. Not null.
}

@property (readonly, assign) NSRect rect;
@property (readonly) void *start;
@property (readonly) void *end;

/// Testing function.  Implementation prints a warning.
- (Interval *)initWithRect:(NSRect)r;


/**
 Initialization happens in two steps.
    -# all parameters of the given rectangle but for the width are set
    -# the width of the rectangle is set
 this is so that intervals can be generated as their endpoints become known, left to right
 */

///Initialize
/*! initialize
 \param x x of the rectangle
 \param y y of the rectangle
 \param h height of the rectangle
 \param c color of the rectangle
 \param s event marking the start of this interval

 \return the initialized object
 
 note: the width of the rectangle is determined by a later call
 */
- (Interval *)initWithX:(CGFloat)x
		      y:(CGFloat)y
		 height:(CGFloat)h
		  color:(NSColor *)c 
		  start:(void *)s;

/// Finish initializing the rectangle
- (void)setWidth:(CGFloat)w end:(void *)e;

@end
