//
//  Exceptions.m
//  LogDrawer
//
//  Created by Korei Klein on 7/22/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "Exceptions.h"


@implementation Exceptions

+ (void)raise:(NSString *)s
{
    @throw [NSException exceptionWithName:@"Visualizer Exception"
				   reason:s
				 userInfo:nil];
    return;
}

@end
