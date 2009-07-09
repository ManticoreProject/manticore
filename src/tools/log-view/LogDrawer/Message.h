/*! \file Message.h
 \author Korei Klein
 \date 7/7/09
 */

#import <Cocoa/Cocoa.h>
#import "Structures.h"

/// Represent a message event by drawing an arrow
/*! Before creating any message objects, you must call initArrowHead.
 */
@interface Message : NSObject {
	NSPoint start; //!< Point where the arrow starts
	NSPoint end; //!< Point where the arrow ends
	NSBezierPath *path; //!< The entire arrow (cache of start and end).
	
	NSColor *color; //!< Color of the arrow
	CGFloat lineWidth; //!< Width of the arrow
	
	event sender; //!< The event where this message was sent
	event receiver; //!< The event where this message was received
}

/*!
 The arrow head.
 It should point rightwards along the positive x-axis.
 The tip of the arrow head should be at the origin
 */
NSBezierPath *arrowHead; //!< arrowhead

+ (void)initArrowHead; //!< create the arrowHead path


///Initialize
- (Message *)initArrowFromPoint:(NSPoint)p1
			toPoint:(NSPoint)p2;
///Initialize
- (Message *)initArrowFromPoint:(NSPoint)p1
			toPoint:(NSPoint)p2
			 sender:(event)s
		       receiver:(event)r;
///Initialize
/*! initialize
 \param p1 the point where the arrow starts
 \param p2 the point where the arrow ends
 \param c color of the arrow
 \param w width of the line
 \param s event marking the start of this message
 \param r event marking the end of this message
 */
- (Message *)initArrowFromPoint:(NSPoint)p1
			toPoint:(NSPoint)p2
			  color:(NSColor *)c
		      lineWidth:(CGFloat)w
			 sender:(event)s
		       receiver:(event)r;


/// Determine if a point lies within the area this shape is drawn on
- (BOOL)containsPoint:(NSPoint)p;

#pragma mark Drawing Methods


- (void)drawShape; //!< Draw the arrow

@end
