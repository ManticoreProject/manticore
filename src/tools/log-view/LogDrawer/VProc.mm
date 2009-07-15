/** \file  VProc.m
 * \author Korei Klein
 * \date 7/10/09
 */

#import "VProc.hxx"
#import "EventUtils.h"


@implementation VProc

#pragma mark Synthesis

@synthesize events;
@synthesize numEvents;


#pragma mark Initialization

- (VProc *)initWithVpId:(int32_t)vpIdVal
{
    if (![super init])
	return nil;
    vpId = vpIdVal;
    return self;
}



#pragma mark Description

- (NSString *)description
{
    NSMutableString *ret = [NSMutableString stringWithFormat:
	@"<< VProc %d, %d events:", vpId, numEvents];
    for (int i = 0; i < numEvents; ++i)
    {
	[ret appendString:@"\n"];
	[ret appendString:@"\t\t\t"];
	[ret appendString:DynamicEventDescription((*events)[i])];
    }
    [ret appendString:@" >>"];
    return ret;
}


@end
