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

/// Testing function.  Implementation prints a warning.
- (Interval *)initWithRect:(NSRect)r;

///Initialize
/*! initialize
 \param r rectangle to draw
 \param c color of the rectangle
 \param s event marking the start of this interval
 \param e event marking the end of this interval

 \return the initialized object
 */
- (Interval *)initWithRect:(NSRect)r color:(NSColor *)c start:(void *)s end:(void *)e;


@end
