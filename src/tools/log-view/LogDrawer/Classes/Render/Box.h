//
//  Box.h
//  LogDrawer
//
//  Created by Korei Klein on 7/29/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface Box : NSObject {
    void *value;
}

@property void *value;
+ (Box *)box:(void *)valueVal;
- (void *)unbox;

@end
