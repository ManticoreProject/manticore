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
        // Initialization code here.
    }
    return self;
}

- (void)drawRect:(NSRect)rect {
    // FIXME simple implementation for testing
    [[NSColor greenColor] set];
    [NSBezierPath fillRect:[self visibleRect]];
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
    NSRect bounds = [self bounds];
    [shapes addObject:[[State alloc] initWithRect:NSMakeRect(s, bounds.origin.y, bounds.origin.x + bounds.size.width - s, bounds.size.height)
					    color:c start:e]];
}


@end
