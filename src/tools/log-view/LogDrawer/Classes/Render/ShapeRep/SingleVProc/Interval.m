/*! \file  Interval.m
    \author Korei Klein
    \date 7/7/09
 */




#import "Exceptions.h"
#import "Utils.h"
#import "Interval.h"


#pragma mark Defaults

/// Default color of an interval shape.
/** For testing only.  Define the color of each interval explicitly
 in production.
 */
#define DEFAULT_INTERVAL_COLOR ([NSColor redColor])

/// Determines the geometry of interval shape corners
#define X_ROUNDING_RADIUS (5)
/// Determines the geometry of interval shape corners
#define Y_ROUNDING_RADIUS (5)

@implementation Interval

@synthesize rect;

#pragma mark Initializations
- (Interval *)initWithRect:(NSRect)r
{
	NSLog(@" ****\tInterval:\tBad initialization");
    if (![super init])
	return nil;
    rect = r;
    end = nil;
    start = nil;
    
    color = DEFAULT_INTERVAL_COLOR;
    
    return self;
}

- (Interval *)initWithX:(CGFloat)x
		      y:(CGFloat)y
		 height:(CGFloat)h
		  color:(NSColor *)c 
		  start:(void *)s
{
	if (![super init])
		return nil;
	rect.origin.x = x;
	rect.origin.y = y;
	rect.size.height = h;
	rect.size.width = 0; //< To be initialized later
    
	
	color = c;
	
	start = s;
	return self;
}

- (void)setWidth:(CGFloat)w end:(void *)e
{
    end = e;
    rect.size.width = w;
    roundedRect = [[NSBezierPath alloc] init];
    if (!roundedRect)
	[Exceptions raise:@"could not allocate roundedRect"];
    [roundedRect appendBezierPathWithRoundedRect:rect
					 xRadius:X_ROUNDING_RADIUS
					 yRadius:Y_ROUNDING_RADIUS];
}

#pragma mark EventShape Methods

- (void)drawShape
{
	if (!rect.size.width)
	{
	    // [Exceptions raise:@"Interval: asked to draw shape when rectangle size was uninitialized"];
	}
	[color set];
	[roundedRect fill];
	[[NSColor blackColor] set];
	[roundedRect stroke];
}
- (BOOL)containsPoint:(NSPoint)p
{
	return [roundedRect containsPoint:p];
}

@end
