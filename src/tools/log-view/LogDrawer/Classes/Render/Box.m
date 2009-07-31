//
//  Box.m
//  LogDrawer
//
//  Created by Korei Klein on 7/29/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "Box.h"


@implementation Box
@synthesize value;

+ (Box *)box:(void *)valueVal
{
    Box *b = [[Box alloc] init];
    b.value = valueVal;
    return b;
}

- (void *)unbox
{
    return self.value;
}

- (BOOL)isEqual:(Box *)e
{
    return (e.value == self.value);
}

@end
