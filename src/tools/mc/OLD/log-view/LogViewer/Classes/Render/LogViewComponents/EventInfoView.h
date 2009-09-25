//
//  EventInfoView.h
//  LogViewer
//
//  Created by Korei Klein on 8/10/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface EventInfoView : NSView {
    IBOutlet NSTableView *table;
}

@property (readwrite, assign) NSTableView *table;

@end
