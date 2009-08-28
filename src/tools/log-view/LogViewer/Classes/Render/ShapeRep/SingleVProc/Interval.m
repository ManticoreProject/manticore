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

#define CONTAINMENT_PADDING ( 3 )

/// Determines the geometry of interval shape corners
#define X_ROUNDING_RADIUS (5)
/// Determines the geometry of interval shape corners
#define Y_ROUNDING_RADIUS (5)

#define ROUNDING 0

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
#if ROUNDING
	roundedRect = [NSBezierPath bezierPathWithRoundedRect:rect
						      xRadius:X_ROUNDING_RADIUS
						      yRadius:Y_ROUNDING_RADIUS];
#else
#endif
    }
    else
    {
	
	roundedRect = [[NSBezierPath alloc] init];
#if ROUNDING
	[roundedRect moveToPoint:rect.origin];
	[roundedRect lineToPoint:NSMakePoint(rect.origin.x, rect.origin.y + rect.size.height)];
#else
	NSPoint s = rect.origin;
	[roundedRect moveToPoint:s];
	s.y += rect.size.height;
	[roundedRect lineToPoint:s];
#endif
	 
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
    /*
    NSRect r = rect;

    if (r.size.width < TINY_WIDTH)
	r.size.width = 2; malloc
    [NSBezierPath fillRect:r];
    return;
    roundedRect = [NSBezierPath bezierPathWithRoundedRect:rect
						  xRadius:X_ROUNDING_RADIUS
						  yRadius:Y_ROUNDING_RADIUS];
     */
    
    
    if (rect.size.width > TINY_WIDTH)
    {
#if ROUNDING
	[roundedRect fill];
	[[NSColor blackColor] set];
	[roundedRect stroke];
#else
	[NSBezierPath fillRect:rect];
#endif

    }
    else
    {
	[[NSColor blackColor] set];
	[roundedRect stroke];
    }
}
- (BOOL)containsPoint:(NSPoint)p
{
    if (rect.size.width > TINY_WIDTH)
    {
#if ROUNDING
	return [roundedRect containsPoint:p];
#else
	return ( rect.origin.x <= p.x ) &&
	       ( p.x <= rect.origin.x + rect.size.width) &&
	       ( rect.origin.y <= p.y ) &&
	       ( p.y <= rect.origin.y + rect.size.height );
#endif
    }
    else
    {
	return ( rect.origin.x - CONTAINMENT_PADDING <= p.x ) &&
	       ( p.x <= rect.origin.x + CONTAINMENT_PADDING ) &&
	       ( rect.origin.y <= p.y && p.y <= rect.origin.y + rect.size.height);
    }
    
}

- (shapeTag)kind
{
    return INTERVAL_SHAPE;
}


@end
