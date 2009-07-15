/** \file AppController.mm
 * \author Korei Klein
 * \date 7/14/09
 *
 */

#import "AppController.hxx"
#import "log-desc.hxx"
#import "event-desc.hxx"
#import "LogView.h"


@implementation AppController

- (void)fillLogView:(LogView *)lv
	withLogFile:(LogFile *)lf
	fromTime:(uint64_t)logStart
	toTime:(uint64_t)logEnd
{
    CGFloat viewStart = lv.start;
    CGFloat viewEnd = lv.end;
    double scale = (viewStart - viewEnd) / (logStart - logEnd);

    NSMutableArray *bands = [NSMutableArray initWithCapacity:lf.nVProcs];
    for (VProc *vp in lf.vProcs)
    {
	DynamicEvent (*events)[] = vp.events;
	for (int i = 0; i < vp.vpId; ++i)
	{
	    BandView *band;
	    EventDesc *desc = desc(events[i]);
	    Group *g = NULL; // FIXME
	    if (events[i].timeStamp >= s &&
	        events[i].timeStamp <= f)
		{
		    switch (g->Kind())
		    {
			case EVENT_GROUP:
			    [band addEvent:&events[i] withColor:groupColor(g)
					   andStart:viewStart + scale * events[i].timestamp];
			    break;
			case STATE_GROUP:
			    [band addState:&events[i] withColor:groupColor(g)
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

- (NSColor *)groupColor(Group *g)
{
    return [NSColor redColor]; // FIXME
}


@end
