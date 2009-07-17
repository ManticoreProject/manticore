//
//  BandView.m
//  Visualizer
//
//  Created by Korei Klein on 7/7/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "BandView.h"
#import "ShapeRep.h"

#define XRADIUS ( 10 )
#define YRADIUS ( 10 )
#define DEFAULT_BAND_COLOR ( [NSColor greenColor] )

@implementation BandView

- (id)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
	shapes = [[NSMutableArray alloc] init];
	lastState = nil;
	bandColor = DEFAULT_BAND_COLOR;
    }
    return self;
}

- (void)drawRect:(NSRect)rect {
    [bandColor set];
    [NSBezierPath fillRect:[self visibleRect]];
    [[NSColor blackColor] set];
    
    NSLog(@"BandView is drawing shapes");
    for (EventShape *e in shapes)
    {
	NSLog(@"\t\tDrawing");
	[e drawShape];
    }
}

- (void)addEvent:(void *)e withColor:(NSColor *)c andStart:(CGFloat)s
{
    NSRect bounds = [self bounds];
    [shapes addObject:[[Singleton alloc]
		       initWithPoint:NSMakePoint(s , bounds.origin.y + bounds.size.height / 2)
		       color:c start:e]];
}
- (void)addState:(void *)e withColor:(NSColor *)c andStart:(CGFloat)s;
{
    NSLog(@"BandView is adding a state event");
    NSRect bounds = [self bounds];
    
    // For now, this state's rectangle extends to the end of the BandView
    State *newState = [[State alloc]
		      initWithRect:NSMakeRect(s,
				    bounds.origin.y,
				    bounds.origin.x + bounds.size.width - s,
				    bounds.size.height)
		      color:c
		      start:e];
    if (lastState)
    {
	NSRect oldLastStateRect = lastState.rect;
	// Now that a new state has been added, we can shorten the width of the last state
	oldLastStateRect.size.width = s - oldLastStateRect.origin.x;
	lastState.rect = oldLastStateRect;
	
    }
    lastState = newState;
    [shapes addObject:newState];
}


@end

