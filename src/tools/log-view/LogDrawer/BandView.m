//
//  BandView.m
//  Visualizer
//
//  Created by Korei Klein on 7/7/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "BandView.h"
#import "ShapeRep.h"


@implementation BandView

- (id)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
	shapes = [[NSMutableArray alloc] init];
	states = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)drawRect:(NSRect)rect {
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
    [shapes addObject:[[State alloc] initWithRect:NSMakeRect(s, bounds.origin.y, bounds.origin.x + bounds.size.width - s, bounds.size.height)
					    color:c start:e]];
}


@end
