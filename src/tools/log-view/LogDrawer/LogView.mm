/** \file  LogView.m
 * \author Korei Klein
 * \date 7/7/09
 *
 */

#import "LogView.h"
#import "VProc.hxx"
#define BAND_HEIGHT ( 200 )


@implementation LogView




@synthesize logStart;
@synthesize logEnd;



- (id)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) { 
        
	splitView = [[CustomSplitView alloc] initWithFrame:[self bounds]];
	messageView = [[MessageView alloc] initWithFrame:[self bounds]];
	[self addSubview:splitView];
	[self addSubview:messageView];
	logFile = nil;
	
	/* for lack of any other reasonable choice,
	 * logStart and logEnd are initialized to the minimum and maximum values.
	 * Note: this may cause poor performance if they are not reinitialized
	 * before the first call to setLogFile
	 */
	logStart = 0;
	logEnd = -1;
    }
    return self;
}

- (void)drawRect:(NSRect)rect {
    [[NSColor blueColor] set];
    [NSBezierPath fillRect:[self bounds]];
}

int sillyNumber = 0;

/// For Testing, return a new interesting color
- (NSColor *)sillyNextColor
{
    if (sillyNumber == 0)
    {
	sillyNumber = 1;
	return [NSColor redColor];
    }
    else {
	sillyNumber = 0;
	return [NSColor yellowColor];
    }
}

- (void)readNewData
{
    NSRect bounds = [self bounds];
    bounds.size.height =
	logFile.nVProcs * (BAND_HEIGHT + [splitView dividerThickness]);
    NSRect frame = [self frame];
    frame.size.width = bounds.size.width;
    frame.size.height = bounds.size.height;
    [self setFrame:frame];
    
    
    CustomSplitView *newSplitView = [[CustomSplitView alloc] initWithFrame:bounds];
    MessageView *newMessageView = [[MessageView alloc] initWithFrame:bounds];
    [self replaceSubview:splitView with:newSplitView];
    [self replaceSubview:messageView with:newMessageView];
    splitView = newSplitView;
    messageView = newMessageView;
    
    double scale = bounds.size.width / (logEnd - logStart);
    
    int v = 0;
    for (VProc *vp in logFile.vProcs)
    {
	DynamicEvent *events = (DynamicEvent *) vp.events;
	NSRect curFrame = NSMakeRect(bounds.origin.x, v * BAND_HEIGHT,
				     bounds.size.width, BAND_HEIGHT);
        BandView *band = [[BandView alloc] initWithFrame:curFrame];
	
	NSRect splitViewFrame = [splitView frame];
	NSLog(@"adding a band for VProc with vpId %d", v);


	for (int i = 0; i < vp.numEvents; ++i)
	{
	    	    
	    
	    // EventDesc *desc = description(events[i], nil);
	    // FIXME for now we assume the event is just a singleton event
	    // Which should be drawn in yellow
	    if (events[i].timestamp >= logStart &&
	        events[i].timestamp <= logEnd)
	    {
		CGFloat drawingPosition =
		    bounds.origin.x + scale * (events[i].timestamp - logStart);
		NSLog(@"\tAdding event at time %qu, position %f to band", events[i].timestamp, drawingPosition);
		[band addState:&events[i] withColor:[self sillyNextColor]
		      andStart:drawingPosition];
	    }
	    else {
		NSLog(@"Skipping event at time %qu, because it is out of timespan", events[i].timestamp);
	    }
	}
	[splitView addSubview:band];
	[band setNeedsDisplay:YES];
	++v;
    }
    
    [splitView adjustSubviews];
    [self setNeedsDisplay:YES];
    [messageView setNeedsDisplay:YES];
    [splitView setNeedsDisplay:YES];
}

- (void)setLogFile:(LogFile *)logFileVal
{
    logFile = logFileVal;
    [self readNewData];
}

- (void)setStart:(uint64_t)startVal andEnd:(uint64_t)endVal
{
    logStart = startVal;
    logEnd = endVal;
    NSLog(@"start time = %qu, end time = %qu", logStart, logEnd);
    if (logFile)
    {
	[self readNewData];
	[self setNeedsDisplay:YES];
    }
}

@end
