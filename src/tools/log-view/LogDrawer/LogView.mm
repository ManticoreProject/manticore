/** \file  LogView.m
 * \author Korei Klein
 * \date 7/7/09
 *
 */

#import "LogView.h"
#import "VProc.hxx"
#import "log-desc.hxx"

#define BAND_HEIGHT ( 200 )
#define SINGLETON_COLOR ( [NSColor yellowColor] )


@implementation LogView




@synthesize logStart;
@synthesize logEnd;
@synthesize zoomLevel;


- (id)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
	NSRect f = [self frame];
	f.size.width = 3000; // XXX For testing, to make it easy to see a big frame
	[self setFrame:f];
	splitView = [[CustomSplitView alloc] initWithFrame:[self bounds]];
	messageView = [[MessageView alloc] initWithFrame:[self bounds]];
	[self addSubview:splitView];
	[self addSubview:messageView];
	logFile = nil;
	stateGroup = NULL;
	cur_state = 0;
	
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
    switch (zoomLevel)
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
	return [NSColor blackColor];
    }
}


/// FIXME pick better colors
- (NSColor *)colorForIntervalGroup:(IntervalGroup *)g
{
    return [NSColor blueColor];
}
/// Return the color that a state should be drawn in
- (NSColor *)colorForState:(int)state
{
    switch (state)
    {
	case 0:
	    return [NSColor blueColor];
	case 1:
	    return [NSColor redColor];
	case 2:
	    return [NSColor grayColor];
	case 3:
	    return [NSColor orangeColor];
	case 4:
	    return [NSColor purpleColor];
	case 5:
	    return [NSColor greenColor];
	case 6:
	    return [NSColor brownColor];
	default:
	    @throw @"Can't decide on a color for state";
    }
}




- (void)readNewDataDeepZoom
{
    NSRect bounds = [self bounds];
    bounds.size.height =
	logFile.nVProcs * (BAND_HEIGHT + [splitView dividerThickness]);
    NSRect frame = [self frame];
    frame.size.width = bounds.size.width;
    frame.size.height = bounds.size.height;
    [self setFrame:frame];
 
    // The old subviews no longer have valid shapes on them, remove them
    CustomSplitView *newSplitView = [[CustomSplitView alloc] initWithFrame:bounds];
    MessageView *newMessageView = [[MessageView alloc] initWithFrame:bounds];
    [self replaceSubview:splitView with:newSplitView];
    [self replaceSubview:messageView with:newMessageView];
    splitView = newSplitView;
    messageView = newMessageView;

    // Converts from intervals in log file to intervals in log view
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
	    // NSLog(@"checking if event %d of %d is in timespan", i, vp.numEvents);
	    EventDesc *eventDesc = description(events[i], nil);
	    if (events[i].timestamp >= logStart &&
	        events[i].timestamp <= logEnd)
	    {
		// NSLog(@"Found event in timespan, checking for shapes to draw");
		
#pragma mark SINGLETONS
	/////////////// SINGLETONS ///////////////////
		CGFloat drawingPosition =
		bounds.origin.x + scale * (events[i].timestamp - logStart);

		if (events[i].desc->isSimpleEvent())
		{
		    NSLog(@"adding singleton");
		    // The event is a singleton
		    // The event may also be in some groups
		    [band addSingleton:&events[i]
			     withColor:SINGLETON_COLOR
			      andStart:drawingPosition];
		}
		// Convert this event into a shape, once for each group it is in
		
	/////////////// STATE GROUPS ///////////////////
#pragma mark STATE GROUPS
		
		// For now, we set the stateGroup to be the one containing
		// the first event, if such a state exists
		if (!stateGroup)
		{ 
		    NSLog(@"stateGroup is uninitialized, checking event for groups to use");
		    NSLog(@"logFile.desc = 0x%x", logFile.desc);
		    std::vector<StateGroup *> *states =
			logFile.desc->StateGroups(eventDesc);
		    if (states)
		    {
			if (states->size() >=1)
			{
			    stateGroup = states->at(0);
			    NSLog(@"Initializing stateGroup to %@", stateGroup);
			    [band setStateStartColor:
			       [self colorForState:stateGroup->StartState()]];
			}
		    }
		}
		// Warning, NOT an ELSE clause!! must be an if. see logic above.
		if (stateGroup)
		{
		    // This function returns -1 if this event does not mark
		    // a transition in this state
		    int next_state = stateGroup->NextState(cur_state, eventDesc);
		    if (next_state != -1)
		    {
			[band addState:&events[i]
			     withColor:[self colorForState:next_state]
			      andStart:drawingPosition];
		    }
		    else
		    {
			// This event is not a transition in this state,
			// therefore it defines no shape.
			// Do nothing.
		    }
		}
#pragma mark INTERVAL GROUPS
	/////////////// INTERVAL GROUPS ///////////////////
		std::vector<IntervalGroup *> *intervals =
		    logFile.desc->IntervalGroups(eventDesc);
		if (intervals)
		{
		    for (int h = 0; h < intervals->size(); ++h)
		    {
		        IntervalGroup *intervalGroup = intervals->at(i);
		        if (eventDesc == intervalGroup->Start())
		        {
		    	[band addIntervalStart:&events[i]
		    		  withColor:[self colorForIntervalGroup:intervalGroup]
		    	   forIntervalGroup:intervalGroup
		    		   andStart:drawingPosition];
		        }
		        else if (eventDesc == intervalGroup->End())
		        {
		    	[band addIntervalEnd:&events[i]
		    	    forIntervalGroup:intervalGroup
		    		    andStart:drawingPosition];
		        }
		    }
		}
		
		
#pragma mark DEPENDENT GROUPS
	///////////////// DEPENDENT GROUPS ////////////////
		NSLog(@"\tAdding event at time %qu, position %f to band", events[i].timestamp, drawingPosition);
		[band addState:&events[i] withColor:[self sillyNextColor]
		      andStart:drawingPosition];
	    }
	    else {
		// NSLog(@"Skipping event at time %qu, because it is out of timespan", events[i].timestamp);
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

/// Read data from log file.  The data read corresponds to the interval
/// (logStart, logEnd)
- (void)readNewData
{
    switch (zoomLevel)
    {
	case zoomLevelDeep:
	    [self readNewDataDeepZoom];
	    break;
	case zoomLevelMedium:
	    @throw @"medium level zooming is unimplemented";
	    break;
	case zoomLevelShallow:
	    @throw @"shallow level zooming is unimplemented";
	    break;
    }
}

- (void)setLogFile:(LogFile *)logFileVal
{
    logFile = logFileVal;
    [self readNewData];
}

#pragma mark Zoomming

/// The largest number of nanoseconds that can be displayed at deep zoom
#define MAX_DEEP_ZOOM_WIDTH ( 1000000 )
/// The largest number of nanoseconds that can be displayed at medium zoom
#define MAX_MEDIUM_ZOOM_WIDTH ( 10000000 )

- (void)setZoomWithWidth:(uint64_t)width
{
    if (width < MAX_DEEP_ZOOM_WIDTH)
    {
	NSLog(@"Entering deep zoom");
	zoomLevel = zoomLevelDeep;
    }
    else if (width < MAX_MEDIUM_ZOOM_WIDTH)
    {
	NSLog(@"Entering medium zoom");
	zoomLevel = zoomLevelMedium;
    }
    else
    {
	NSLog(@"Entering shallow zoom");
	zoomLevel = zoomLevelShallow;
    }
}

- (void)setStart:(uint64_t)startVal andEnd:(uint64_t)endVal
{
    logStart = startVal;
    logEnd = endVal;
    NSLog(@"start time = %qu, end time = %qu", logStart, logEnd);
    [self setZoomWithWidth:logEnd - logStart];
    if (logFile)
    {
	[self readNewData];
	[self setNeedsDisplay:YES];
    }
}

@end
