//
//  ViewController.h
//  LogViewer
//
//  Created by Jordan Lewis on 6/30/10.
//  Copyright 2010 The University of Chicago. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class LogView;


@interface ViewController : NSObject <NSSplitViewDelegate>
{
    IBOutlet LogView *logView;
}

@property LogView *logView;

- (void)splitViewDidResizeSubviews:(NSNotification *)notification;


@end
