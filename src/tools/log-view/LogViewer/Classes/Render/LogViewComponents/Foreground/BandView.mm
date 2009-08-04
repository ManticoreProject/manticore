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
#import "DetailAccess.h"
#import "VProc.h"
#import "LogDoc.h"
#import "SelectedEvent.h"
#import "GroupFilter.h"

#define XRADIUS ( 10 )
#define YRADIUS ( 10 )
#define DEFAULT_BAND_COLOR ( [NSColor greenColor] )
#define BAND_BORDER_THICKNESS ( 2 )
#define BAND_ROUNDING_RADIUS ( 10 )
#define DEFAULT_INTERVAL_HEIGHT ( 30 )


/// DIAMOND_PADDING is the amount of space allowed between adjacent diamonds
/// It may be negative to allow the diamonds to overlap
#define DIAMOND_PADDING ( 5 )

#define INTERVAL_PADDING ( 5 )

@implementation BandView

@synthesize selectedEvent;
@synthesize target;

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


- (BandView *)initWithFrame:(NSRect)frame
		     logDoc:(LogDoc *)logDocVal
		      vProc:(VProc *)vp
		     filter:(GroupFilter *)filter
{
    self = [super initWithFrame:frame];
    if (self) {
	logDoc = logDocVal;
	singletons = [[NSMutableArray alloc] init];
	states = [[NSMutableArray alloc] init];
	intervals = [[NSMutableArray alloc] init];
	
	NSRect bounds = self.shapeBounds;
	cur_singleton_height = bounds.size.height / 2;
	cur_interval_height = bounds.size.height / 2;

	Detail *details = vp.details;
	// XXX FIXME There is no filtering here
	for (int i = 0; i < vp.numDetails; ++i)
	{
	    Group *g = Detail_Type(details[i]);
	    int doDisplay = [filter enabled:g].intValue;
	    // NSLog(@"doDisplay = %d", doDisplay);
	    if (doDisplay == 1)
		[self addDetail:details[i]];
	}
    }
    return self;
}

#pragma mark Drawing
- (void)drawRect:(NSRect)rect {

    [[NSColor blueColor] set];
    [NSBezierPath fillRect:self.bounds];
    
    int a = 0;
    for (State *e in states)
    {
	[e drawShape];
	++a;
    }
    NSLog(@"Drew %d state changes", a);

    int b = 0;
    for (Interval *e in intervals)
    {
	[e drawShape];
	++b;
    }
    NSLog(@"Drew %d intervals", b);

    int c = 0;
    for (EventShape *e in singletons)
    {
	[e drawShape];
	++c;
    }
    NSLog(@"Drew %d singletons", c);
    
    NSLog(@"BandView drew %d shapes", a + b + c);
}



#pragma mark Simples

- (NSColor *)colorForSimple:(Detail)d
{
    return [NSColor yellowColor];
}

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

// XXX CURRENTLY UNUSED
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

- (void)addSimple:(Group *)g forDetail:(Detail)d
{
    CGFloat s = [logDoc image: Event_Time(*Detail_Simple_value(d))];
    NSColor *c = [self colorForSimple:d];

    [singletons addObject:[[Singleton alloc]
		       initWithPoint:NSMakePoint(s, self.singletonHeight)
		       color:c
			   event:Detail_Simple_value(d)]];
}



#pragma mark States

- (NSColor *)colorForState:(Detail)d
{
    return [NSColor redColor];
}

- (void)addState:(StateGroup *)g forDetail:(Detail)d
{
    NSColor *c = [self colorForState:d];
    NSRect bounds = self.bounds;
    NSRect r;

    r.origin.y = bounds.origin.y;
    r.size.height = bounds.size.height;
    r.origin.x = [logDoc image:Event_Time(*Detail_State_start(d))];
    r.size.width = [logDoc image:Event_Time(*Detail_State_end(d))] - r.origin.x;

    [states addObject:[[State alloc] initWithRect:r
					    color:c
					    start:Detail_State_start(d)
					      end:Detail_State_end(d)]];
}

#pragma mark Intervals

- (NSColor *)colorForInterval:(Detail)d
{
    return [NSColor redColor];
}

- (CGFloat)intervalHeightForIntervalOfHeight:(CGFloat)h
{
     CGFloat height = self.bounds.size.height;
     cur_interval_height += h + INTERVAL_PADDING;
     if (cur_interval_height >= height - (h + INTERVAL_PADDING))
     {
	cur_interval_height -= height - 2 * (h + INTERVAL_PADDING);
     }
     return cur_interval_height;
}

- (void)addInterval:(IntervalGroup *)g forDetail:(Detail)d
{
    event *start = Detail_Interval_start(d);
    event *end = Detail_Interval_end(d);
    if (start == NULL || end == NULL)
    {
	NSLog(@"BandView.mm: skipping an interval whose start or end does not exist");
	return;
    }
    NSColor *c = [self colorForInterval:d];
    NSRect r;
    r.origin.x = [logDoc image:Event_Time(*start)];
    r.size.width = [logDoc image:Event_Time(*end)] - r.origin.x;
    r.size.height = DEFAULT_INTERVAL_HEIGHT;
    r.origin.y = [self intervalHeightForIntervalOfHeight:r.size.height];
   // NSLog(@"BandView is adding an interval: origin = %f, %f size = %f %f",
	//  r.origin.x, r.origin.y, r.size.width, r.size.height);
    [intervals addObject:[[Interval alloc] initWithRect:r
						  color:c
						  start:start
						    end:end]];
}



- (void)addDetail:(struct TaggedDetail_struct *)d
{
    Group *g = Detail_Type(d);
    switch (g->Kind())
    {
	case EVENT_GROUP:
	    [self addSimple:(Group *)g forDetail:d];
	    break;
	case STATE_GROUP:
	    [self addState:(StateGroup *)g forDetail:d];
	    break;
	case INTERVAL_GROUP:
	    [self addInterval:(IntervalGroup *)g forDetail:d];
	    break;
	case DEPENDENT_GROUP:
	default:
	    [Exceptions raise:@"Unrecognized kind of group"];
    }
}

#pragma mark Mouse Events

- (void)mouseDown:(NSEvent *)e
{
    NSLog(@"Mouse was clicked");
    NSPoint p = [self convertPoint:e.locationInWindow fromView:nil];
    for (State *e in states)
    {
	if ([e containsPoint:p])
	{
	    
	    [selectedEvent setValue:e.start
		      withEventDesc:(logDoc.logDesc)->FindEventById(Event_Id(*e.start))];
	    NSLog(@"State clicked");
	}
    }
    for (Interval *e in intervals)
    {
	if ([e containsPoint:p])
	{
	    [selectedEvent setValue:e.start
		      withEventDesc:(logDoc.logDesc)->FindEventById(Event_Id(*e.start))];

    	    NSLog(@"Interval clicked");
	}
    }

    for (Singleton *e in singletons)
    {
	if ([e containsPoint:p])
	{
	    [selectedEvent setValue:e.eventVal
		      withEventDesc:(logDoc.logDesc)->FindEventById(Event_Id(*e.eventVal))];

	    NSLog(@"Singleton clicked");	    
	}
    }
}

@end
    /*
     - (CGFloat)intervalHeightForIntervalOfHeight:(CGFloat)h
     {
     CGFloat height = self.bounds.size.height;
     cur_interval_height += h + INTERVAL_PADDING;
     if (cur_interval_height >= height - (h + INTERVAL_PADDING))
     {
     cur_interval_height -= height - 2 * (h + INTERVAL_PADDING);
     }
     return cur_interval_height;
     }
     */
    /*
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
     */
