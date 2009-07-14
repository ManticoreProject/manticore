/** \file  VProc.m
 * \author Korei Klein
 * \date 7/10/09
 */

#import "VProc.h"


@implementation VProc

@synthesize events;
@synthesize numEvents;

- (VProc *)initWithVpId:(int32_t)vpIdVal
{
    if (![super init])
	return nil;
    vpId = vpIdVal;
    return self;
}


@end
