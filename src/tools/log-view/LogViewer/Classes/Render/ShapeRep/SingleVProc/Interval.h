/*! \file Interval.h
 \author Korei Klein
 \date 7/7/09
 Define an object to represent the shape of an interval event

*/

#import <Cocoa/Cocoa.h>
#import "EventShape.h"
#import "Detail.h"

/// Represents an interval event by drawing a rectangle
@interface Interval : EventShape {
	NSRect rect; //!< Rectangle representing the event. Not null.
	NSBezierPath *roundedRect; //!< Rounded rectangle (a cache of rect). Not null.
	
	event *start; //!< The event corresponding to the begginig of the interval. Not null.
	event *end; //!< The event corresponding to the end of the interval. Not null.
}

@property (readonly, assign) NSRect rect;
@property (readonly) event *start;
@property (readonly) event *end;

/// Testing function.  Implementation prints a warning.
- (id)initWithRect:(NSRect)r
	     color:(NSColor *)c
	     start:(event *)s
	       end:(event *)f;





@end
