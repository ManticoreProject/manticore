//
//  TimeDisplay.m
//  LogViewer
//
//  Created by Korei Klein on 8/5/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "TimeDisplay.h"
#import "LogDoc.h"

@implementation TimeDisplay



- (id)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code here.
    }
    return self;
}

- (void)drawRect:(NSRect)rect {
    NSLog(@"horizontal positinon %d", logDoc.horizontalPosition);
}

@end
