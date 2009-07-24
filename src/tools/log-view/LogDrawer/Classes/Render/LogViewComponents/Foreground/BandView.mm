/** \file BandView.mm
 * \author Korei Klein
 * \date 7/7/09
 *
 */

#import "Exceptions.h"
#import "BandView.h"
#import "ShapeRep.h"
#import "log-desc.hxx"
#import "CustomSplitView.h"
#import "Utils.h"


#define XRADIUS ( 10 )
#define YRADIUS ( 10 )
#define DEFAULT_BAND_COLOR ( [NSColor greenColor] )
#define BAND_BORDER_THICKNESS ( 2 )
#define BAND_ROUNDING_RADIUS ( 10 )


/// DIAMOND_PADDING is the amount of space allowed between adjacent diamonds
/// It may be negative to allow the diamonds to overlap
#define DIAMOND_PADDING ( 5 )

@implementation BandView


- (BOOL)isOpaque
{
    return YES;
}

/// Return the bounds of this BandView, but shrunk a little
- (NSRect)shapeBounds
{
    NSRect r = self.bounds;
    r.origin.x += X_PADDING;
    r.origin.y += Y_PADDING;
    r.size.width -= 2 * X_PADDING;
    r.size.height -= 2 * Y_PADDING;
    return r;
}


#pragma mark Initialization
- (id)initWithFrame:(NSRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
	stateStartColor = nil;
	singletons = [[NSMutableArray alloc] init];
	states = [[NSMutableArray alloc] init];
	intervals = [[NSMutableArray alloc] init];
	messages = [[NSMutableArray alloc] init];
	lastState = nil;
	bandColor = DEFAULT_BAND_COLOR;
	NSRect bounds = self.shapeBounds;
	cur_singleton_height = bounds.size.height / 2;
	srandom(time(NULL));
	intervalMap = [NSMapTable
		       mapTableWithKeyOptions:NSPointerFunctionsCStringPersonality
					    | NSPointerFunctionsOpaqueMemory
		       valueOptions:NSPointerFunctionsObjectPersonality
				  | NSPointerFunctionsOpaqueMemory];
    }
    return self;
}

#pragma mark Drawing
- (void)drawRect:(NSRect)rect {
    [bandColor set];
    NSBezierPath *bandPath =
	[NSBezierPath bezierPathWithRoundedRect:self.shapeBounds
					xRadius:BAND_ROUNDING_RADIUS
					yRadius:BAND_ROUNDING_RADIUS];
    bandPath.lineWidth = BAND_BORDER_THICKNESS;
    [bandPath fill];
    [[NSColor blackColor] set];
    [bandPath stroke];


    int i; // for help with NSLogging

    i = 0;
    for (EventShape *e in states)
    {
	[e drawShape];
	++i;
    }
    // NSLog(@"Drew %d state changes", i);

    i = 0;
    for (EventShape *e in intervals)
    {
	[e drawShape];
	++i;
    }
    // NSLog(@"Drew %d intervals", i);

    i = 0;
    for (EventShape *e in messages)
    {
	[e drawShape];
	++i;
    }
    // NSLog(@"Drew %d messages", i);

    i = 0;
    for (EventShape *e in singletons)
    {
	[e drawShape];
	++i;
    }
    // NSLog(@"Drew %d singletons", i);
}

#pragma mark Singletons


- (CGFloat)singletonHeight
{
    CGFloat height = self.bounds.size.height;
    cur_singleton_height += DIAMOND_HEIGHT + DIAMOND_PADDING;
    if (cur_singleton_height >= height - (DIAMOND_HEIGHT + DIAMOND_PADDING))
    {
	cur_singleton_height -= height - 2 * (DIAMOND_HEIGHT + DIAMOND_PADDING);
    }
    return cur_singleton_height;
}

/// Return the fraction of the current height at which the next singleton should
/// be placed
- (CGFloat)singletonHeightOld
{
    NSRect bounds = self.shapeBounds;
    double t = random();
    double r = (t / RAND_MAX) * bounds.size.height;
    // NSLog(@"random double %f, %f", t, r);
    double range = bounds.size.height - 2 * DIAMOND_HEIGHT - 2 * DIAMOND_PADDING;
    assert( range > 0);
    while (r >= range)
	r -= range;
    if (abs(r - cur_singleton_height) <= DIAMOND_PADDING + DIAMOND_HEIGHT / 2)
	r += DIAMOND_HEIGHT + 2 * DIAMOND_PADDING;
    // r is now a random value in the interval

    // (0, bounds.size.height) \
    // (cur_singleton_height - DIAMOND_HEIGHT - 2 * DIAMOND_PADDING,
    //  cur_singleton_height + DIAMOND_HEIGHT + 2 * DIAMOND_PADDING)

    // where \ is set subtraction
    // We draw our singleton at height r along the band
    cur_singleton_height = r + DIAMOND_HEIGHT;
    return (bounds.origin.y + r);
}


- (void)addSingleton:(void *)e withColor:(NSColor *)c andStart:(CGFloat)s
{
    // NSLog(@"BandView is adding a singleton event");
    [singletons addObject:[[Singleton alloc]
		       initWithPoint:NSMakePoint(s, self.singletonHeight)
		       color:c start:e]];
}

#pragma mark States
- (void)addState:(void *)e withColor:(NSColor *)c andStart:(CGFloat)s;
{
    NSLog(@"BandView is adding a state event");
    NSRect bounds = self.bounds;
    
    // For now, this state's rectangle extends to the end of the BandView
    NSRect newRect = NSMakeRect(s,
				bounds.origin.y,
				bounds.origin.x + bounds.size.width - s,
				bounds.size.height);
    State *newState = [[State alloc]
		      initWithRect:newRect
		      color:c
		      start:e];
    if (lastState)
    {
	NSRect oldLastStateRect = lastState.rect;
	// Now that a new state has been added, we can shorten the width of the last state
	oldLastStateRect.size.width = s - oldLastStateRect.origin.x;
	lastState.rect = oldLastStateRect;
	lastState.end = newState;
    }
    else
    {
	NSLog(@" *****  BandView, states: a state was added before an initial state was set");
    }
    lastState = newState;
    [states addObject:newState];
}

/// XXX Warning: this method sets the first color to be used when displaying states
/// If you set it to the color for the start state of a vproc and have the BandView
/// display from somewhere in the middle of the log, then the color may be inaccurate.
- (void)setStateStartColor:(NSColor *)c
{
    NSRect bounds = [self bounds];
    lastState = [[State alloc]
		 initWithRect:NSMakeRect(bounds.origin.x,
					 bounds.origin.y,
					 0,
					 bounds.size.height)
		 color:c
		 start:nil];
}

#pragma mark Intervals

/// Decide at what height to put at rectangle
// FIXME use a better algorithm for picking height
- (CGFloat)heightForIntervalAt:(CGFloat)x fromGroup:(Group *)g
{
    return self.shapeBounds.size.height / 2;
}

- (void)addIntervalStart:(void *)e
	       withColor:(NSColor *)c
	forIntervalGroup:(struct IntervalGroup *)g
		andStart:(CGFloat)s
{
    NSRect bounds = self.shapeBounds;
    NSLog(@"BandView is adding an interval event");
    Interval *i = [[Interval alloc]
		   initWithX:s
		   y:bounds.origin.y
		   height:[self heightForIntervalAt:s
					  fromGroup:g]
		   color:c
		   start:e];
    [intervalMap setObject:i forKey:(id)(g->Desc())];
    [intervals addObject:i];
}
- (void)addIntervalEnd:(void *)e
      forIntervalGroup:(struct IntervalGroup *)g
	      andStart:(CGFloat)s
{
    Interval *i = [intervalMap objectForKey:(id)(g->Desc())];
    if (!i)
	[Exceptions raise:@"BandView addIntervalEnd: called with an end event with no corresponding start event"];
    NSRect r = i.rect;
    [i setWidth:s - r.origin.x end:e];
}

@end

