//
//  AppDelegate.m
//  LogViewer
//
//  Created by Korei Klein on 8/12/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "AppDelegate.h"


@implementation AppDelegate

- (AppDelegate *)init
{
    if (![super init]) return nil;
    
    [NSApp setDelegate:self];
  //  NSLog(@"AppDelegate: set self as delegate to NSApp %@", NSApp);
    
    return self;
}

- (BOOL)applicationShouldOpenUntitledFile:(NSApplication *)sender
{
 //   NSLog(@"AppDelegate returning NO to request to open new file");
    return NO;
}

@end
