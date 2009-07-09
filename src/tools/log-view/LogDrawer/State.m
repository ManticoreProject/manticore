/*! \file State.m
 \author Korei Klein
 \date 7/7/09
 */

#import "State.h"

#pragma mark Defaults
/// Color of states
#define DEFAULT_STATE_COLOR ([NSColor blueColor])



@implementation State

#pragma mark Initializations
- (State *)initWithRect:(NSRect)r
{
	NSLog(@" ****\tState:\tBad initialization");
	return [self initWithRect:r color:DEFAULT_STATE_COLOR start:nil end:nil];
}
- (State *)initWithRect:(NSRect)r
		  color:(NSColor *)c
		  start:(event)s
		    end:(event)e
{
	if (![super init])
		return nil;
	rect = r;
	color = c;
	start = s;
	end = e;
	return self;
}

#pragma mark EventShape Methods

- (void)drawShape
{
	[color set];
	[NSBezierPath fillRect:rect];
}


- (BOOL)containsPoint:(NSPoint)p
{
	return
		(
		p.x >= rect.origin.x &&
		p.x <= rect.origin.x + rect.size.width &&
		p.y >= rect.origin.y &&
		p.y <= rect.origin.y + rect.size.height
		);
		
}

@end
