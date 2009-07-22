/** \file  LogView.m
 * \author Korei Klein
 * \date 7/7/09
 *
 */

#import "Utils.h"
#import "LogView.h"
#import "VProc.hxx"
#import "log-desc.hxx"
#import "CustomSplitView.h"
#import "Exceptions.h"


#define DEFAULT_LOG_VIEW_WIDTH ( 5000 )
#define MIN_BAND_HEIGHT ( 50 )
#define SINGLETON_COLOR ( [NSColor yellowColor] )
#define LOG_VIEW_BACKGROUND_COLOR ( [NSColor blackColor] )
#define DEFAULT_TIME_TICK ( 55 )
#define TICK_LINE_COLOR ( [NSColor cyanColor] )
#define TICK_LINE_WIDTH ( 0.1 )

@implementation LogView

- (BOOL)isOpaque
{
    return YES;
}



@synthesize logX;
@synthesize logWidth;
@synthesize zoomLevel;
@synthesize timeTick;
@synthesize ruler;

- (id)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
	NSRect f = [self frame];
	f.size.width = DEFAULT_LOG_VIEW_WIDTH;
	[self setFrame:f];
	splitView = [[CustomSplitView alloc] initWithFrame:self.bounds];
	messageView = [[MessageView alloc] initWithFrame:self.bounds];
	[self addSubview:splitView];
	NSRect splitViewBounds = splitView.bounds;
	splitViewBounds.size.height -= 2 * DIVIDER_THICKNESS;
	splitViewBounds.origin.y += DIVIDER_THICKNESS;
	splitView.bounds = splitViewBounds;
	[self addSubview:messageView];
	
	logFile = nil;
	stateGroup = NULL;
	cur_state = 0;
	timeTick = DEFAULT_TIME_TICK;
	/* for lack of any other reasonable choice,
	 * logX and logWidth are initialized to the minimum and maximum values.
	 * Note: this may cause poor performance if they are not reinitialized
	 * before the first call to setLogFile
	 */
	logX = 0;
	logWidth = -1;
    }
    return self;
}

- (void)drawRect:(NSRect)rect
{
    // Draw Background
    [LOG_VIEW_BACKGROUND_COLOR set];
    [NSBezierPath fillRect:[self bounds]];
    
    NSString *TickName = @"Ticks";

    // Set up the ruler
    NSArray *upArray = [NSArray arrayWithObjects:[NSNumber numberWithFloat:2.0], nil];
    NSArray *downArray = [NSArray arrayWithObjects:
			  [NSNumber numberWithFloat:0.5], [NSNumber numberWithFloat:0.2], nil];
    [NSRulerView registerUnitWithName:TickName
			 abbreviation:@"tks"
	 unitToPointsConversionFactor:timeTick
			  stepUpCycle:upArray
			stepDownCycle:downArray];
    ruler.measurementUnits = TickName;
    ruler.originOffset = X_PADDING;
    
    // Draw tick lines
    NSBezierPath *verticalLine = [[NSBezierPath alloc] init];
    NSRect shapeBounds = splitView.shapeBounds;
    NSRect bounds = self.bounds;
    
    NSPoint s = NSMakePoint(shapeBounds.origin.x, bounds.origin.y);
    NSPoint f = NSMakePoint(shapeBounds.origin.x, bounds.origin.y + bounds.size.height);
    
    [TICK_LINE_COLOR set];
    verticalLine.lineWidth = TICK_LINE_WIDTH;
    verticalLine.flatness = .3;
    while (s.x < bounds.origin.x + bounds.size.width)
    {

	[verticalLine moveToPoint:s];
	[verticalLine lineToPoint:f];
	[verticalLine stroke];
	s.x += timeTick;
	f.x += timeTick;
    }
}


- (CGFloat)image:(uint64_t)p
{
    NSRect bounds = splitView.shapeBounds;
    uint64_t scale = bounds.size.width / logWidth;
    return bounds.origin.x + scale * (p - logX);
}

- (uint64_t)preImage:(CGFloat)p
{
    NSRect bounds = [self bounds];
    uint64_t scale = logWidth / bounds.size.width;
    return logX + scale * (p - bounds.origin.x);
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
	    [Exceptions raise:@"Can't decide on a color for state"];
    }
    return nil;
}




- (void)readNewDataDeepZoom
{
    NSRect bounds = [self bounds];
    CGFloat min_height = 
	logFile.nVProcs * (MIN_BAND_HEIGHT + [splitView dividerThickness]);
    NSLog(@"min height = %f, nVProcs = %d", min_height, logFile.nVProcs);
    if (bounds.size.height < min_height)
    {
	bounds.size.height = min_height;
    }
    CGFloat band_height =
	(bounds.size.height - logFile.nVProcs * splitView.dividerThickness) /
		logFile.nVProcs;
    NSRect frame = self.frame;
    frame.size.width = bounds.size.width;
    frame.size.height = bounds.size.height;
    NSLog(@"Setting logview frame to %f,%f,%f,%f", frame.origin.x, frame.origin.y, frame.size.width, frame.size.height);
    self.frame = frame;
    self.bounds = bounds;
    
 
    // The old subviews no longer have valid shapes on them, remove them
    NSRect splitViewBounds = bounds;
    splitViewBounds.origin.y += DIVIDER_THICKNESS;
    splitViewBounds.size.height -= 2 * DIVIDER_THICKNESS;
    CustomSplitView *newSplitView = [[CustomSplitView alloc] initWithFrame:splitViewBounds];
    MessageView *newMessageView = [[MessageView alloc] initWithFrame:bounds];
    [self replaceSubview:splitView with:newSplitView];
    [self replaceSubview:messageView with:newMessageView];
    splitView = newSplitView;
    messageView = newMessageView;

   
    int v = 0;
    for (VProc *vp in logFile.vProcs)
    {
	DynamicEvent *events = (DynamicEvent *) vp.events;
	NSRect curFrame = NSMakeRect(splitViewBounds.origin.x, v * band_height,
				     splitViewBounds.size.width, band_height);
        BandView *band = [[BandView alloc] initWithFrame:curFrame];
	NSRect bandBounds = band.shapeBounds;

	// Converts from intervals in log file to intervals in log view
	double scale = bandBounds.size.width / logWidth;

	
	NSRect splitViewFrame = [splitView frame];
	NSLog(@"adding a band for VProc with vpId %d", v);


	for (int i = 0; i < vp.numEvents; ++i)
	{
	    // NSLog(@"checking if event %d of %d is in timespan", i, vp.numEvents);
	    EventDesc *eventDesc = description(events[i], nil);
	    if (events[i].timestamp >= logX &&
	        events[i].timestamp <= logX + logWidth)
	    {
		// NSLog(@"Found event in timespan, checking for shapes to draw");
	    
	    CGFloat drawingPosition =
		bandBounds.origin.x + scale * (events[i].timestamp - logX);		
#pragma mark SINGLETONS
	/////////////// SINGLETONS ///////////////////


		if (events[i].desc->isSimpleEvent())
		{
		    // NSLog(@"adding singleton");
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
		    // NSLog(@"stateGroup is uninitialized, checking event for groups to use");
		    // NSLog(@"logFile.desc = 0x%x", logFile.desc);
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
			// NSLog(@"checking interval %s", intervals->at(i)->Desc());
		        IntervalGroup *intervalGroup = intervals->at(i);
		        if (eventDesc == intervalGroup->Start())
		        {
		    	[band addIntervalStart:&events[i]
		    		  withColor:[self colorForIntervalGroup:intervalGroup]
		    	   forIntervalGroup:intervalGroup
		    		   andStart:drawingPosition];
		        }
		        else
		        {
			    assert (eventDesc == intervalGroup->End());
			    [band addIntervalEnd:&events[i]
				forIntervalGroup:intervalGroup
					andStart:drawingPosition];
		        }
		    }
		}
		
		
#pragma mark DEPENDENT GROUPS
	///////////////// DEPENDENT GROUPS ////////////////
		
		std::vector<DependentGroup *> *dependents =
		    logFile.desc->DependentGroups(eventDesc);
		if (dependents)
		{
		    for (int h = 0; h < dependents->size(); ++h)
		    {
			DependentGroup *dependentGroup = dependents->at(h);
			if (eventDesc == dependentGroup->Src())
			{
			    // 
			}
			else
			{
			    assert (eventDesc == dependentGroup->Dst());
			}
		    }
		}
		/* NSLog(@"\tAdding event at time %qu, position %f to band", events[i].timestamp, drawingPosition);
		[band addState:&events[i] withColor:[self sillyNextColor]
		      andStart:drawingPosition];
		 */
	    }
	    else {
		// The event is not part of the current timespan
		// NSLog(@"Skipping event at time %qu, because it is out of timespan", events[i].timestamp);
	    }
	}
	
	//////////////////////// FINISH ///////////////////
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
/// (logX, logX + logWidth)
- (void)readNewData
{
    switch (zoomLevel)
    {
	case zoomLevelDeep:
	    [self readNewDataDeepZoom];
	    break;
	case zoomLevelMedium:
	    [Exceptions raise:@"medium level zooming is unimplemented"];
	    break;
	case zoomLevelShallow:
	    [Exceptions raise:@"shallow level zooming is unimplemented"];
	    break;
    }
}

- (IBAction)setLogFile:(LogFile *)logFileVal
{
    logFile = logFileVal;
    [self readNewData];
}

#pragma mark Zoomming

/// The largest number of nanoseconds that can be displayed at deep zoom
#define MAX_DEEP_ZOOM_WIDTH ( -1 )
/// The largest number of nanoseconds that can be displayed at medium zoom
#define MAX_MEDIUM_ZOOM_WIDTH ( 10000000 )

/// The ratio of the scale at one zoom level to that of the next zoom level
#define ZOOM_FACTOR ( 1.2 )

- (void)setZoomWithWidth:(uint64_t)width
{
    NSLog(@"Zoom is %qu", width);
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

- (void)setStart:(uint64_t)logXVal andWidth:(uint64_t)logWidthVal
{
    logX = logXVal;
    logWidth = logWidthVal;
    NSLog(@"start time = %qu, end time = %qu, logWidth = %qu", logX, logX + logWidth, logWidth);
    [self setZoomWithWidth:logWidth];
    if (logFile)
    {
	[self readNewData];
	[self setNeedsDisplay:YES];
    }
}

- (void)resizeIntervalToSize:(uint64_t)size aboutPivot:(uint64_t)pivot
{
    assert( logX <= pivot && pivot <= logX + logWidth );
    uint64_t frac = (pivot - logX) / logWidth;

    [self setStart:pivot - frac * size andWidth:size];
}

- (IBAction)zoomIn:(id)sender
{
    [self resizeIntervalToSize:logWidth / ZOOM_FACTOR
		    aboutPivot:self.pivot];
}
- (IBAction)zoomOut:(id)sender
{
    [self resizeIntervalToSize:logWidth * ZOOM_FACTOR
		    aboutPivot:self.pivot];    
}

- (uint64_t)scale
{
    return logWidth / self.visibleRect.size.width;
}

- (uint64_t)pivot
{
    NSRect vr = self.visibleRect;
    NSLog(@"pivot is at %f in a vr starting at %f of length %f", vr.origin.x + vr.size.width / 2,
	  vr.origin.x, vr.size.width);

    // For now, just use the center of the visible rectangle
    return [self preImage:vr.origin.x + vr.size.width / 2];
}

#pragma mark Mouse Events

- (void)mouseDown:(NSEvent *)event
{
    NSLog(@"mouse went down");
    uint64_t newWidth;
    switch (event.buttonNumber) {
	case 0:
	    NSLog(@"Button 0 was clicked");
	    if (event.modifierFlags & NSControlKeyMask)
	    {
		newWidth = logWidth / ZOOM_FACTOR;
	    }
	    else
	    {
		newWidth = logWidth * ZOOM_FACTOR;
	    }
	    break;
	case NSRightMouseDown:
	    NSLog(@"Button 1 was clicked");
	    newWidth = logWidth * ZOOM_FACTOR;
	    break;
	default:
	    NSLog(@"Unrecognized mouse button was clicked");
	    return;
    }
    NSPoint point = [self convertPoint:event.locationInWindow fromView:nil];
    CGFloat p = point.x;
    [self resizeIntervalToSize:newWidth
		    aboutPivot:[self preImage:p]];
}


@end
