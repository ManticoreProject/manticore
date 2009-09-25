/** \file  DetailEnumerator.mm
\author Korei Klein
\date 8/14/09

*/

#import "DetailEnumerator.h"
#import "log-desc.hxx"
#import "Detail.h"
#import "DetailAccess.h"


@implementation DetailEnumerator


- (DetailEnumerator *)initWithLogData:(LogData *)logDataVal
			     andVProc:(int32_t)vpVal
			     andGroup:(StateGroup *)groupVal
{
    if (![super init]) return nil;

    group = groupVal;
    VProc *vp = [logDataVal.vProcs objectAtIndex:vpVal];
    details = vp.details;
    num_details = vp.numDetails;
    NSLog(@"DetailEnumerator: initializing an enumerator with %d details", num_details);
    cur_detail = 0;

    return self;
}


- (Detail)next
{
    while (1)
    {
	if (cur_detail == num_details) return NULL;
	
	if (Detail_Type(details[cur_detail]) == group) // this group matches the filter
	{
	    return details[cur_detail++];
	}
	else
	{
	    cur_detail++;
	    continue;
	}
    }
}

@end
