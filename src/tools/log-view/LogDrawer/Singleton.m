/*! \file Singleton.m
 \author Korei Klein
 \date 7/7/09
 */

#import "Singleton.h"

#pragma mark Defaults

/// Default color of singleton events
#define DEFAULT_SINGLETON_COLOR ([NSColor yellowColor])
/// Height of diamond
#define DIAMOND_HEIGHT (16)
/// Width of diamond
#define DIAMOND_WIDTH (8)
/// Width of the border of a diamond
#define DEFAULT_LINE_WIDTH (.6)

@implementation Singleton

#pragma mark Initializations

- (Singleton *)initWithX:(CGFloat)x
{
	NSLog(@" ****\tSingleton:\tBad initialization");
	return [self initWithPoint:NSMakePoint(x,0)
			     color:DEFAULT_SINGLETON_COLOR
			     start:nil];
}

- (Singleton *)initWithPoint:(NSPoint)p
		       color:(NSColor *)c
		       start:(event)s
{
	if (![super init])
		return nil;
	place = p;
	color = c;
	start = s;
	
	path = [[NSBezierPath alloc] init];
	[path setLineWidth:DEFAULT_LINE_WIDTH];
	NSPoint p1, p2, p3, p4; // The corners of the diamond
	p1 = NSMakePoint(p.x - DIAMOND_WIDTH / 2, p.y);
	p2 = NSMakePoint(p.x, p.y + DIAMOND_HEIGHT / 2);
	p3 = NSMakePoint(p.x + DIAMOND_WIDTH / 2, p.y);
	p4 = NSMakePoint(p.x, p.y - DIAMOND_HEIGHT / 2);
	
	[path moveToPoint:p1];
	[path lineToPoint:p2];
	[path lineToPoint:p3];
	[path lineToPoint:p4];
	[path lineToPoint:p1];
	
	return self;
}

#pragma mark EventShape Methods

- (void)drawShape
{
	[color set];
	[path fill];
	[[NSColor blackColor] set];
	[path stroke];
}



- (BOOL)containsPoint:(NSPoint)p
{
	return [path containsPoint:p];
}

@end
