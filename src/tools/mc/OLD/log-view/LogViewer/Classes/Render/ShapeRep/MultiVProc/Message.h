/*! \file Message.h
 \author Korei Klein
 \date 7/7/09
 */


#import <Cocoa/Cocoa.h>
#import "EventShape.h"
#import "Detail.h"


/// Represent a message event by drawing an arrow
@interface Message : EventShape {
	NSPoint start; //!< Point where the arrow starts
	NSPoint end; //!< Point where the arrow ends
	NSBezierPath *path; //!< The entire arrow (cache of start and end).
	
	CGFloat lineWidth; //!< Width of the arrow
	
	event *sender; //!< The event where this message was sent
	event *receiver; //!< The event where this message was received
}

@property (readonly) event *sender;
@property (readonly) event * receiver;




///Initialize
- (Message *)initArrowFromPoint:(NSPoint)p1
			toPoint:(NSPoint)p2;
///Initialize
- (Message *)initArrowFromPoint:(NSPoint)p1
			toPoint:(NSPoint)p2
			 sender:(event *)s
		       receiver:(event *)r;
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
			 sender:(event *)s
		       receiver:(event *)r;


/// Determine if a point lies within the area this shape is drawn on
- (BOOL)containsPoint:(NSPoint)p;

#pragma mark Drawing Methods


- (void)drawShape; //!< Draw the arrow

@end
