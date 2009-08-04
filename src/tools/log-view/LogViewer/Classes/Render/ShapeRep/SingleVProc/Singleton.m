/*! \file Singleton.m
 \author Korei Klein
 \date 7/7/09
 */

#import "Singleton.h"

#pragma mark Defaults



@implementation Singleton

@synthesize eventVal;

#pragma mark Initializations



- (Singleton *)initWithPoint:(NSPoint)p
		       color:(NSColor *)c
		       event:(event *)eventValVal;
{
	if (![super init])
		return nil;
    
	eventVal = eventValVal;
	place = p;
	color = c;
	
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
