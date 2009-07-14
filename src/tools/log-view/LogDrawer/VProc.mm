/** \file  VProc.m
 * \author Korei Klein
 * \date 7/10/09
 */

#import "VProc.hxx"


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
