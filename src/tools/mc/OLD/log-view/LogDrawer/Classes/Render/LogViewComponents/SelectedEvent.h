//
//  SelectedEvent.h
//  LogDrawer
//
//  Created by Korei Klein on 7/30/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
struct DynamicEvent_struct;
struct ArgDesc;
union ArgValue;



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


@interface SelectedEvent : NSObject {
    NSString *name;
    NSString *description;
    NSMutableArray *arguments;
    struct DynamicEvent_struct *value;
}

@property (readwrite, assign) NSString *name;
@property (readwrite, assign) NSString *description;
@property (readwrite, assign) NSArray *arguments;


@property (readwrite, assign) struct DynamicEvent_struct *value;


- (SelectedEvent *)initWithValue:(struct DynamicEvent_struct *)valueVal;


@end
