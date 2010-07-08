/** \file  LogView.m
 * \author Korei Klein
 * \date 7/7/09
 *
 */

#import "Exceptions.h"
#import "Utils.h"
#import "LogView.h"
#import "VProc.h"
#import "log-desc.hxx"
#import "CustomSplitView.h"
#import "LogDoc.h"
#import "LogData.h"
#import "ViewController.h"



/// Default width (in floats, representing 72nds of an inc) of the logView
#define DEFAULT_LOG_VIEW_WIDTH ( 5000 )
/// Minimum height of a bandView.  If bandviews do not fit at the height, then the user must
/// use the vertical scrollbar to scroll through them
#define MIN_BAND_HEIGHT ( 60 )
/// Color of singleton/simple events
#define SINGLETON_COLOR ( [NSColor yellowColor] )
/// Background color of the LogView.  Drawn behind tick lines and bandViews.
#define LOG_VIEW_BACKGROUND_COLOR ( [NSColor blackColor] )
/// default distance in floats between tick lines
#define DEFAULT_TIME_TICK ( 55 )

/// Color of small tick lines
#define TICK_LINE_COLOR ( [NSColor cyanColor] )
/// Width of small tick lines
#define TICK_LINE_WIDTH ( 1 )

/// Color of big tick lines
#define BIG_TICK_LINE_COLOR ( [NSColor redColor] )
/// Width of big tick lines
#define BIG_TICK_LINE_WIDTH ( 2 )

// Calculation of units to use at different scales
// If logInterval->width is less than NANO, times are in nanoseconds
#define NANO ( 10 )
#define NANO_ROUNDING ( 1LLU )
#define MICRO ( 100000000LLU )
#define MICRO_ROUNDING ( 1000LLU )
#define MILLI ( 10000000000LLU )
#define MILLI_ROUNDING ( 1000000LLU )
#define SEC_ROUNDING ( 1000000000LLU )

@implementation LogView

- (BOOL)isOpaque
{
    return YES;
}

@synthesize band_height;
@synthesize bands;
@synthesize enabled;
@synthesize ticks;
@synthesize scrollView;
@synthesize timeTick;
@synthesize splitView;
@synthesize messageView;
@synthesize mouseLoc;

- (id)initWithFrame:(NSRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
	/* Change our width from that defined in Interface Builder to the actual
	 * width that we want. This width is the width of the background of the
	 * document view of the scrollview that we're embedded in. So the
	 * scrollview's width will still be that of the window - what we're
	 * changing is the width of the view that it scrolls. */
	
	NSRect f = [self frame];
	//f.size.width = DEFAULT_LOG_VIEW_WIDTH;
	[self setFrame:f];
	
	bands = [[NSMutableArray alloc] init];
	
	timeTick = DEFAULT_TIME_TICK;
	ticks = [[NSMutableArray alloc] init];

	
	trackingArea = [[NSTrackingArea alloc] initWithRect:[self bounds]
						    options:(NSTrackingMouseMoved | NSTrackingMouseEnteredAndExited| NSTrackingActiveAlways)
						      owner:self
						   userInfo:nil];
	[self addTrackingArea:trackingArea];

    }
    return self;
}


- (void)drawRect:(NSRect)dirtyRect
{
    // check if logDoc and self are in their enabled states
    if (!logDoc.enabled) return;
    
    NSRect bounds = [self bounds];

    // Draw Background
    [LOG_VIEW_BACKGROUND_COLOR set];
    [NSBezierPath fillRect:bounds];
    
    NSBezierPath *line = [[NSBezierPath alloc] init];
    NSPoint s, f, test;
    s.y = bounds.origin.y;
    f.y = bounds.origin.y + bounds.size.height;
    test.y = dirtyRect.origin.y;

    int a = 0;
    for (NSNumber *x in ticks)
    {
	s.x = f.x = test.x = x.floatValue + .5;
	if (!NSPointInRect(test, dirtyRect))
	{
	    a++;
	    continue;
	}
	[line moveToPoint:s];
	[line lineToPoint:f];
    }

    //NSLog(@"Skipped %d tick lines", a);
    // Draw tick lines
    [TICK_LINE_COLOR set];
    [line stroke];

    [logDoc drewTicks:self];
}

/// Draw a bigTick
/// Currently unused.
- (void)bigTickAt:(CGFloat)t
{
    NSRect bounds = self.bounds;

    NSBezierPath *verticalLine = [[NSBezierPath alloc] init];
    verticalLine.lineWidth = BIG_TICK_LINE_WIDTH;
    [BIG_TICK_LINE_COLOR set];

    NSPoint s, f;
    s.x = f.x = t;
    s.y = bounds.origin.y;
    f.y = bounds.origin.y + bounds.size.height;

    [verticalLine moveToPoint:s];
    [verticalLine lineToPoint:f];
    [verticalLine stroke];
}

- (uint64_t)rounding
{
    if (rounding == 0)
    {
	uint64_t t = [logDoc logInterval]->width;
	if (t < NANO)
	    rounding = NANO_ROUNDING;
	else if (t < MICRO)
	    rounding = MICRO_ROUNDING;
	else if (t < MILLI)
	    rounding = MILLI_ROUNDING;
	else // ticks are to be displayed as seconds
	    rounding = SEC_ROUNDING;
    }

    return rounding;
}

- (NSString *)timeSuffix
{
    NSString *ret;
    switch ([self rounding])
    {
	case NANO_ROUNDING:  ret = @"ns"; break;
	case MICRO_ROUNDING: ret = @"\u03BCs";  break;
	case MILLI_ROUNDING: ret = @"ms"; break;
	case SEC_ROUNDING:   ret = @"sec"; break;
	default: [Exceptions raise:@"Disallowed rounding time"];
    }
    return ret;
}

- (NSString *)stringFromTime:(uint64_t)t
{
    uint64_t r = t / [self rounding];
    NSString *ret = [[NSString alloc] initWithFormat:@"%qu ", r];
    ret = [ret stringByAppendingString:[self timeSuffix]];
    return ret;
}

- (void)displayInterval:(struct LogInterval *)logInterval
	    atZoomLevel:(enum ZoomLevel)zoomLevel
	    fromLogData:(LogData *)logData
	     filteredBy:(GroupFilter *)filter;
{
    // The current bounds may not be suitable for displaying the requested data
    // modify them so that they are suitable
    NSRect bounds = [self bounds];

    // Minimum height of the LogView
    CGFloat min_height = DIVIDER_THICKNESS +
	logData.vProcs.count * (MIN_BAND_HEIGHT + DIVIDER_THICKNESS);

    if (bounds.size.height < min_height) bounds.size.height = min_height;

    band_height =
	(bounds.size.height - (1 + logData.vProcs.count * DIVIDER_THICKNESS)) /
		logData.vProcs.count;
    NSRect frame = self.frame;
    frame.size.width = bounds.size.width;
    frame.size.height = bounds.size.height;
    // NSLog(@"Setting logview frame to %f,%f,%f,%f", frame.origin.x, frame.origin.y, frame.size.width, frame.size.height);
    self.frame = frame;
    self.bounds = bounds;


    // The old subviews no longer have valid shapes on them, so we update them
    // with the proper new values.

    
    NSRect splitViewBounds = bounds;
    splitViewBounds.origin.y += DIVIDER_THICKNESS;
    splitViewBounds.size.height -= 2 * DIVIDER_THICKNESS;
    [splitView setFrame:splitViewBounds];


    // Add tick lines
    NSRect shapeBounds = splitView.bounds;

    float x = shapeBounds.origin.x;
    ticks = [[NSMutableArray alloc] init];
    while (x < shapeBounds.origin.x + shapeBounds.size.width)
    {
	[ticks addObject:[NSNumber numberWithFloat:x]];
	x += timeTick;
    }
    
    // Have to split this operation up, as objective-C's iterator construct
    // cannot handle mutation of its target.
    
    for (NSView *view in bands)
    {
	[view removeFromSuperview];
    }
    [bands removeAllObjects];
    
    int v = 0;
   // NSLog(@"LogView: must add %d bands, one for each vproc", logData.vProcs.count);
    for (VProc *vp in logData.vProcs)
    {
	BandView *band =[[BandView alloc]
			       initWithFrame:NSMakeRect
			       (splitViewBounds.origin.x,
				     DIVIDER_THICKNESS +
					(v * (band_height + DIVIDER_THICKNESS)),
				     splitViewBounds.size.width,
				     band_height)
			       logDoc:logDoc
			       vProc:vp
			       filter:filter];
	[bands addObject:band];
	[splitView addSubview:band];
	band.target = target;
	v++;
    }

    
    [messageView updateDependents:logData.dependentDetails];
    [messageView setFrame: splitViewBounds];


    for (BandView *band in bands)
    {
	band.messageView = messageView;
    }
    
    
    // Set up RulerView
    NSArray *upArray, *downArray;
    double scale = shapeBounds.size.width / (logDoc.logInterval->width);
    
    upArray = [NSArray arrayWithObjects:[NSNumber numberWithFloat:5.0], nil];
    downArray = [NSArray arrayWithObjects:[NSNumber numberWithFloat:0.5], [NSNumber numberWithFloat:0.2], nil];
    [NSRulerView registerUnitWithName:@"milliseconds"
			 abbreviation:@"ms"
	 unitToPointsConversionFactor:scale * [self rounding]
			  stepUpCycle:upArray
			stepDownCycle:downArray];
    
    
    [scrollView setRulersVisible:YES];
    [scrollView setHasHorizontalRuler:YES];

    [[scrollView horizontalRulerView] setOriginOffset:shapeBounds.origin.x - logInterval->x * scale];
    [[scrollView horizontalRulerView] setMeasurementUnits:@"milliseconds"];
    


    self.enabled = true;

    // Manage the views a bit more
    [splitView adjustSubviews];
    [self setNeedsDisplay:YES];
    [messageView setNeedsDisplay:YES];
    [splitView setNeedsDisplay:YES];
}


- (void)mouseMoved:(NSEvent *)e
{
    /* Update view with mouse cursor line. The messageView actually does the
     * drawing, since it's the topmost view, but since we're handling the mouse
     * updates, it's our responsibility to tell the messageView that it's 
     * dirty where the line should be and where it used to be so it knows to
     * redraw. */
    NSPoint newMouseLoc = [self convertPoint:e.locationInWindow fromView:nil];
    
    // First clear the old line's location
    NSRect invalid = self.bounds;
    invalid.origin.x = mouseLoc.x - 1;
    invalid.size.width = 3;
    [self setNeedsDisplayInRect:invalid];
    
    
    // Then redisplay the new line's location
    mouseLoc = newMouseLoc;

    uint64_t time = [logDoc preImage:mouseLoc.x];
    
    NSString *timeStr = [self stringFromTime:time];
    [timeUnderMouse setStringValue:timeStr];
    invalid.origin.x = mouseLoc.x - 1;

    [self setNeedsDisplayInRect:invalid];
}

- (void)updateTrackingAreas
{
    /* This method is called when our visibleRect changes, on behalf of the
     * tracking areas that we own. We are expected to update our tracking areas
     * with our new bounds. */
    [self removeTrackingArea:trackingArea];
    trackingArea = [[NSTrackingArea alloc] initWithRect:[self bounds]
						options:(NSTrackingMouseMoved | NSTrackingMouseEnteredAndExited| NSTrackingActiveAlways)
						  owner:self
					       userInfo:nil];
    [self addTrackingArea:trackingArea];
}


@end

