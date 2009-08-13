//
//  GroupFilter.m
//  LogDrawer
//
//  Created by Korei Klein on 7/30/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "GroupFilter.h"
#import "Exceptions.h"
#import "LogDoc.h"

@implementation GroupFilter

@synthesize logDoc;

- (NSNumber *)enabled:(struct Group *)g
{
    [Exceptions raise:
     @"GroupFilter is an abstract class, enabled was not overridden"];
    return [NSNumber numberWithInt:-1];
}

@end
