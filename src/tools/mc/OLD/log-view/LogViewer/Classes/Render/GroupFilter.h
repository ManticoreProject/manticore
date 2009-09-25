//
//  GroupFilter.h
//  LogDrawer
//
//  Created by Korei Klein on 7/30/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
@class LogDoc;

@interface GroupFilter : NSObject {
    IBOutlet LogDoc *logDoc;

}

@property (readwrite, assign) LogDoc *logDoc;

- (NSNumber *)enabled:(struct Group *)g;

@end
