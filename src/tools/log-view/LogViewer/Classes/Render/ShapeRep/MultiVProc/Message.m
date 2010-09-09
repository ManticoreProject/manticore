/*! \file Message.m
 \author Korei Klein
 \date 7/7/09
 */

#import "Message.h"
#import "Utils.h"


#pragma mark Constants

/// Color of arrow
#define DEFAULT_MESSAGE_COLOR ([NSColor orangeColor])
/// Width of arrow
#define DEFAULT_LINE_WIDTH ( 3 )

/// Size of arrow
#define ARROW_HEAD_WIDTH (6)
/// Size of arrow
#define ARROW_HEAD_HEIGHT (10)

/// How close a point has to be to the start or end of a message to be contained in it
#define CLICK_RANGE (5.5)


/*!
 The arrow head.
 It should point rightwards along the positive x-axis.
 The tip of the arrow head should be at the origin
 */
static NSBezierPath *arrowHead; //!< arrowhead


@implementation Message

@synthesize sender;
@synthesize receiver;
@synthesize path;

#pragma mark Initializations

- (void)initArrowHead
{
    // Initialize the arrowHead
    arrowHead = [[NSBezierPath alloc] init];
    
    [arrowHead moveToPoint: NSMakePoint( - ARROW_HEAD_HEIGHT, ARROW_HEAD_WIDTH / 2)];
    [arrowHead lineToPoint: NSZeroPoint];
    [arrowHead lineToPoint: NSMakePoint(- ARROW_HEAD_HEIGHT, - ARROW_HEAD_WIDTH / 2)];
    
 //   NSLog(@"arrowHead just created %@", arrowHead);
}


- (Message *)initArrowFromPoint:(NSPoint)p1
			toPoint:(NSPoint)p2
			  color:(NSColor *)c
		      lineWidth:(CGFloat)w
			 sender:(event *)s
		       receiver:(event *)r
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
	//NSLog(@"theta = %f", theta);
	[T rotateByDegrees: theta];
	[A translateXBy:end.x yBy:end.y];
	
	if (!arrowHead) self.initArrowHead;
	path = [arrowHead copy];
	//NSLog(@"path just initialized from arrow %@ as %@", arrowHead, path);
	
	[path transformUsingAffineTransform:T];
	[path transformUsingAffineTransform:A];
	
	// Create the arrow body
	[path moveToPoint:start];
	[path lineToPoint:end];
	
	[path setLineWidth:lineWidth];
	
	return self;
}


- (Message *)initArrowFromPoint:(NSPoint)p1
			toPoint:(NSPoint)p2
			  color:(NSColor *)c
			 sender:(event *)s
		       receiver:(event *)r
{
	// Okay initialization. Arrows are assumed to be black
	return [self initArrowFromPoint:p1
				toPoint:p2
				  color:c
			      lineWidth:DEFAULT_LINE_WIDTH
				 sender:s
			       receiver:r];
}



- (Message *)initArrowFromPoint:(NSPoint)p1
			toPoint:(NSPoint)p2
{
	NSLog(@" ****\tMessage:\tBad initialization");
	return [self initArrowFromPoint:p1
				toPoint:p2
				  color:DEFAULT_COLOR
				 sender:nil
			       receiver:nil];
}

- (Message *)initArrowFromPoint:(NSPoint)p1
			toPoint:(NSPoint)p2
			 sender:(event *)s
		       receiver:(event*)r
{
	return [self initArrowFromPoint:p1
				toPoint:p2
				  color:DEFAULT_COLOR
				 sender:s
			       receiver:r];
}


#pragma mark EventShape Methods
- (BOOL)containsPoint:(NSPoint)p
{
    return [path containsPoint:p];
}

- (NSRect)bounds
{
    return [path bounds];
}


- (shapeTag)kind
{
    return MESSAGE_SHAPE;
}

- (void)drawShape
{
    //NSLog(@"path being drawn for arrow %@", path);
    [color set];
    [path stroke];
}


@end

