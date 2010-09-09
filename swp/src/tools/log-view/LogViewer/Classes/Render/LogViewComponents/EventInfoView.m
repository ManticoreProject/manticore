//
//  EventInfoView.m
//  LogViewer
//
//  Created by Korei Klein on 8/10/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "EventInfoView.h"


@implementation EventInfoView


@synthesize table;


- (EventInfoView *)initWithFrame:(NSRect)frame {
    if (![super initWithFrame:frame]) return nil;
    
  //  NSLog(@"EventInfoView: I am being initialized into %f %f %f %f of %@",
//	  self.bounds.origin.x, self.bounds.origin.y,
//	  self.bounds.size.width, self.bounds.size.height,
//	  self.superview);
    
    table = nil;
    
    return self;
}

- (void)drawRect:(NSRect)rect {
}

@end
