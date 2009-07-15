/** \file AppController.mm
 * \author Korei Klein
 * \date 7/14/09
 *
 */

#import "AppController.hxx"
#import "log-desc.hxx"
#import "event-desc.hxx"
#import "LogView.h"
#import "VProc.hxx"
#import "DynamicEventRep.hxx"


@implementation AppController

- (void)fillLogView:(LogView *)lv
	withLogFile:(LogFile *)lf
	fromTime:(uint64_t)logStart
	toTime:(uint64_t)logEnd
{
    CGFloat viewStart = lv.start;
    CGFloat viewEnd = lv.end;
    double scale = (viewStart - viewEnd) / (logStart - logEnd);

    NSMutableArray *bands = [NSMutableArray arrayWithCapacity:lf.nVProcs];
    for (VProc *vp in lf.vProcs)
    {
	DynamicEvent *events = (DynamicEvent *) vp.events;
	for (int i = 0; i < vp.vpId; ++i)
	{
	    BandView *band;
	    EventDesc *desc = description(events[i], nil);
	    NSLog(@"%@", desc);
	    Group *g = NULL; // FIXME
	    if (events[i].timestamp >= logStart &&
	        events[i].timestamp <= logEnd)
		{
		    switch (g->Kind())
		    {
			case EVENT_GROUP:
			    [band addEvent:&events[i] withColor:[self groupColor:g]
					   andStart:viewStart + scale * events[i].timestamp];
			    break;
			case STATE_GROUP:
			    [band addState:&events[i] withColor:[self groupColor:g]
					   andStart:viewStart + scale * events[i].timestamp];
			    break;
			case INTERVAL_GROUP: case DEPENDENT_GROUP:
			// not handling these cases
			    break;
		    }
		}
	}
    }
}

- (NSColor *)groupColor:(Group *)g
{
    return [NSColor redColor]; // FIXME
}


@end
