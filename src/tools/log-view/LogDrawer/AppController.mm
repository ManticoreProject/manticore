/** \file AppController.mm
 * \author Korei Klein
 * \date 7/14/09
 *
 */

#import "AppController.h"
#import "log-desc.hxx"
#import "event-desc.hxx"
#import "LogView.h"
#import "VProc.hxx"
#import "LogFile.h"
#import "DynamicEventRep.hxx"

#define BAND_HEIGHT ( 200 )

@implementation AppController

- (void)fillLogView:(LogView *)lv
	withLogFile:(LogFile *)lf
	fromTime:(uint64_t)logStart
	toTime:(uint64_t)logEnd
{
    CGFloat viewStart = lv.start;
    CGFloat viewEnd = lv.end;
    double scale = (viewStart - viewEnd) / (logStart - logEnd);
    NSRect logViewBounds = [lv bounds];

    NSMutableArray *bands = [NSMutableArray arrayWithCapacity:lf.nVProcs];
    int v = 0;
    for (VProc *vp in lf.vProcs)
    {
	DynamicEvent *events = (DynamicEvent *) vp.events;
	NSRect curFrame = NSMakeRect(logViewBounds.origin.x, v * BAND_HEIGHT,
				     logViewBounds.size.width, BAND_HEIGHT);
        BandView *band = [[BandView alloc] initWithFrame:curFrame];
	for (int i = 0; i < vp.numEvents; ++i)
	{
	    NSLog(@"adding a band for VProc with vpId %d", i);
	    EventDesc *desc = description(events[i], nil);
	    
	    // NSLog(@"%@", desc);
	    Group *g = NULL; // FIXME
	    if (events[i].timestamp >= logStart &&
	        events[i].timestamp <= logEnd)
		{/*
		    switch (g->Kind())
		    {
			case EVENT_GROUP:
			    [band addEvent:&events[i] withColor:[self groupColor:g]
					   andStart:viewStart + scale * events[i].timestamp];
			    break;
			case STATE_GROUP: */
			    [band addState:&events[i] withColor:[self groupColor:g]
					   andStart:viewStart + scale * events[i].timestamp];
			    /*break;
			case INTERVAL_GROUP: case DEPENDENT_GROUP:
			// not handling these cases
			    break;
		    } */
		}
	}
        [bands addObject:band];
	++v;
    }
    [lv acquireBands:bands];
    [lv setNeedsDisplay:YES];
}

- (NSColor *)groupColor:(Group *)g
{
    return [NSColor redColor]; // FIXME
}

- (IBAction)test:(id)sender
{
    NSString *root = @"/Users/koreiklein/workspace/manticore/trunk/src/tools/log-view/LogDrawer/";
    LogFile *lf = [[LogFile alloc] initWithFilename:[root stringByAppendingString:@"fib.mlg"]
		 andEventDescFilename:[root stringByAppendingString:@"event-view.json"]
		   andLogDescFilename:[root stringByAppendingString:@"log-events.json"]];
    [self fillLogView:logView withLogFile:lf fromTime:0 toTime:2021168108];
}


@end
