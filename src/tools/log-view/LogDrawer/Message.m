/*! \file Message.m
 \author Korei Klein
 \date 7/7/09
 */

#import "Message.h"
#import "Utils.h"


#pragma mark Constants

/// Color of arrow
#define DEFAULT_MESSAGE_COLOR ([NSColor greenColor])
/// Width of arrow
#define DEFAULT_LINE_WIDTH (1)

/// Size of arrow
#define ARROW_HEAD_WIDTH (6)
/// Size of arrow
#define ARROW_HEAD_HEIGHT (10)

/// How close a point has to be to the start or end of a message to be contained in it
#define CLICK_RANGE (5.5)


@implementation Message

#pragma mark Initializations

+ (void)inititialize
{
    // Initialize the arrowHead
    arrowHead = [[NSBezierPath alloc] init];
    
    
    [arrowHead moveToPoint:
     NSMakePoint( - ARROW_HEAD_HEIGHT, ARROW_HEAD_WIDTH / 2)];
    [arrowHead lineToPoint:
     NSZeroPoint];
    [arrowHead lineToPoint:
     NSMakePoint(- ARROW_HEAD_HEIGHT, - ARROW_HEAD_WIDTH / 2)];
}

- (Message *)initArrowFromPoint:(NSPoint)p1
			toPoint:(NSPoint)p2
{
	NSLog(@" ****\tMessage:\tBad initialization");
	return [self initArrowFromPoint:p1
				toPoint:p2
				 sender:nil
			       receiver:nil];
}

- (Message *)initArrowFromPoint:(NSPoint)p1
			toPoint:(NSPoint)p2
			 sender:(event)s
		       receiver:(event)r
{
	// Okay initialization. Arrows are assumed to be black
	return [self initArrowFromPoint:p1
				toPoint:p2
				  color:DEFAULT_MESSAGE_COLOR
			      lineWidth:DEFAULT_LINE_WIDTH
				 sender:s
			       receiver:r];
}

- (Message *)initArrowFromPoint:(NSPoint)p1
			toPoint:(NSPoint)p2
			  color:(NSColor *)c
		      lineWidth:(CGFloat)w
			 sender:(event)s
		       receiver:(event)r
{
	if (![super init])
		return nil;
	
	start = p1;
	end = p2;
	
	// Do simple initializations
	color = c;
	lineWidth = w;
	
	sender = s;
	receiver = r;
	
	// Create the arrow head
	NSAffineTransform *T = [NSAffineTransform transform];
	NSAffineTransform *A = [NSAffineTransform transform];
	CGFloat theta = (1 / (2 * 3.14159) ) * 360 * atan( (end.y - start.y) / (end.x - start.x) );
	NSLog(@"theta = %f", theta);
	[T rotateByDegrees: theta];
	[A translateXBy:end.x yBy:end.y];
	
	path = [arrowHead copy];
	[path transformUsingAffineTransform:T];
	[path transformUsingAffineTransform:A];
	
	// Create the arrow body
	[path moveToPoint:start];
	[path lineToPoint:end];
	
	[path setLineWidth:lineWidth];
	
	return self;
}


#pragma mark EventShape Methods
- (BOOL)containsPoint:(NSPoint)p
{
	CGFloat d = pointToLineSegmentDistance(p, start, end);
	return (d <= CLICK_RANGE &&
		between(p.x, start.x, end.x) &&
		between(p.y, start.y, end.y));
}



- (void)drawShape
{
	[color set];
	[path stroke];
}

@end







