//
//  GroupFilter.m
//  LogDrawer
//
//  Created by Korei Klein on 7/30/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "GroupFilter.h"
#import "Exceptions.h"


@implementation GroupFilter


- (NSNumber *)enabled:(struct Group *)g
{
    [Exceptions raise:
     @"GroupFilter is an abstract class, enabled was not overridden"];
}

@end
