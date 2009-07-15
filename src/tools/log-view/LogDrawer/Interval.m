/*! \file  Interval.m
    \author Korei Klein
    \date 7/7/09
 */




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

#pragma mark Initializations
- (Interval *)initWithRect:(NSRect)r
{
	NSLog(@" ****\tInterval:\tBad initialization");
	return [self initWithRect:r color:DEFAULT_INTERVAL_COLOR start:nil end:nil];
}

- (Interval *)initWithRect:(NSRect)r
		     color:(NSColor *)c
		     start:(void *)s
		       end:(void *)e
{
	if (![super init])
		return nil;
	rect = r;
	roundedRect = [[NSBezierPath alloc] init];
	if (!roundedRect)
		@throw @"could not allocate roundedRect";
	[roundedRect appendBezierPathWithRoundedRect:r
					     xRadius:X_ROUNDING_RADIUS
					     yRadius:Y_ROUNDING_RADIUS];
	color = c;
	
	start = s;
	end = e;
	return self;
}

#pragma mark EventShape Methods

- (void)drawShape
{
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
