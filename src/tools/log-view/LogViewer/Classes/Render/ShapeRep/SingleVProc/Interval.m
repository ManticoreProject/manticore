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

/// If an interval's width is less than TINY_WIDTH, hitbox testing is done differently
#define TINY_WIDTH ( 2 )
#define CONTAINMENT_PADDING ( 3 )

/// Determines the geometry of interval shape corners
#define X_ROUNDING_RADIUS (5)
/// Determines the geometry of interval shape corners
#define Y_ROUNDING_RADIUS (5)

@implementation Interval

@synthesize start;
@synthesize end;
@synthesize rect;

#pragma mark Initializations

- (Interval *)initWithRect:(NSRect)r
		     color:(NSColor *)c
		     start:(event *)s
		       end:(event *)f;
{
    if (![super init])
	return nil;
    rect = r;
    if (rect.size.width > TINY_WIDTH)
    {
	roundedRect = [NSBezierPath bezierPathWithRoundedRect:rect
						      xRadius:X_ROUNDING_RADIUS
						      yRadius:Y_ROUNDING_RADIUS];
    }
    else
    {
	roundedRect = [[NSBezierPath alloc] init];
	[roundedRect moveToPoint:rect.origin];
	[roundedRect lineToPoint:NSMakePoint(rect.origin.x, rect.origin.y + rect.size.height)];
    }
    color = c;
    start = s;
    end = f;

    return self;
}


#pragma mark EventShape Methods

- (void)drawShape
{
    [color set];
    roundedRect = [NSBezierPath bezierPathWithRoundedRect:rect
						  xRadius:X_ROUNDING_RADIUS
						  yRadius:Y_ROUNDING_RADIUS];
    [roundedRect fill];
    [[NSColor blackColor] set];
    [roundedRect stroke];
}
- (BOOL)containsPoint:(NSPoint)p
{
    if (rect.size.width > TINY_WIDTH)
	return [roundedRect containsPoint:p];
    else
    {
	return ( rect.origin.x - CONTAINMENT_PADDING <= p.x ) &&
	       ( p.x <= rect.origin.x + CONTAINMENT_PADDING ) &&
	       ( rect.origin.y <= p.y && p.y <= rect.origin.y + rect.size.height);
    }
    
}

@end
