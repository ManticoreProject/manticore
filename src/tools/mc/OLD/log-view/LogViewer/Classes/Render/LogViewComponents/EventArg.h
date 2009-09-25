//
//  EventArg.h
//  LogViewer
//
//  Created by Korei Klein on 8/7/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "log-file.h"


@interface EventArg : NSObject {
    NSString *name;
    NSString *type;
    NSString *description;
    NSString *value;
    NSString *size;
}

@property (readwrite, assign) NSString *name;
@property (readwrite, assign) NSString *type;
@property (readwrite, assign) NSString *description;
@property (readwrite, assign) NSString *value;
@property (readwrite, assign) NSString *size;

- (EventArg *)initWithArgDesc:(struct ArgDesc)argDesc
		  andArgValue:(union ArgValue)argValue;

@end
