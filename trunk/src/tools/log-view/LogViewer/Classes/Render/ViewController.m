//
//  ViewController.m
//  LogViewer
//
//  Created by Jordan Lewis on 6/30/10.
//  Copyright 2010 The University of Chicago. All rights reserved.
//

#import "ViewController.h"
#import "LogView.h"

@class BandView;


@implementation ViewController

@synthesize logView;


- (void)splitViewDidResizeSubviews:(NSNotification *)notification
{
    [logView setNeedsDisplay:YES];
}


@end
