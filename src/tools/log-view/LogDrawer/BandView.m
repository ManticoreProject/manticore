//
//  BandView.m
//  Visualizer
//
//  Created by Korei Klein on 7/7/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "BandView.h"


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

@end
