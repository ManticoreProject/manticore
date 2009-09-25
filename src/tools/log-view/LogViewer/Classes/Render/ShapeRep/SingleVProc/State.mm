/*! \file State.mm
 \author Korei Klein
 \date 7/7/09
 */

#import "State.h"
#import "log-desc.hxx"
#import "DetailAccess.h"

#pragma mark Defaults

/// Color of states
#define DEFAULT_STATE_COLOR ([NSColor blueColor])



@implementation State

@synthesize rect;
@synthesize start;
@synthesize end;

#pragma mark Initializations

- (State *)initWithRect:(NSRect)r
		  color:(NSColor *)c
		  start:(event *)startVal
		    end:(event *)endVal;
{
	if (![super init])
		return nil;
    
  //  NSLog(@"state being initialized at %f width %f", r.origin.x, r.size.width);
	rect = r;
	color = c;
    //NSLog(@"color is %@", c);
	start = startVal;
	end = endVal;

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

- (shapeTag)kind
{
    return STATE_SHAPE;
}


/// For help in testing
- (void)nslog
{
    NSLog(@"Logging State");
    if (start == NULL) NSLog(@"start == null");
    else
    {
	// StateGroup *g = Event_Type(*start);
	NSLog(@"start at %qu for some group", Event_Time(*start));//, g->Desc());
    }
    if (end == NULL) NSLog(@"end == null");
    else NSLog(@"end at %qu for some group", Event_Time(*end));
}

@end
