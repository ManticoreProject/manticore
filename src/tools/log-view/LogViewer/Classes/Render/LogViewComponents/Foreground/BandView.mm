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
#import "LogView.h"
#import "GroupFilter.h"
#import "MessageView.h"

#define XRADIUS ( 10 )
#define YRADIUS ( 10 )
#define DEFAULT_BAND_COLOR ( [NSColor greenColor] )
#define BAND_BORDER_THICKNESS ( 2 )
#define BAND_ROUNDING_RADIUS ( 10 )


#define DEBUG


/// DIAMOND_PADDING is the amount of space allowed between adjacent diamonds
/// It may be negative to allow the diamonds to overlap
#define DIAMOND_PADDING ( 5 )

#define INTERVAL_PADDING ( 3 )

@implementation BandView

@synthesize target;
@synthesize messageView;

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
	vProc = vp;
	logDoc = logDocVal;
	shapes = [[NSMutableArray alloc] init];
	
	NSRect bounds = self.shapeBounds;
	cur_singleton_height = bounds.size.height / 2;
	cur_interval_height = bounds.size.height / 2;

	Detail *details = vp.details;
	int skipped = 0;
	for (int i = 0; i < vp.numDetails; ++i)
	{

	    if (![logDoc isInInterval:details[i]])
	    {
		// Pass.  Only read in details which are in the interval
		skipped++;
		continue;
	    }
	    else
	    {
		// Add details whose enabled state is 1
		// don't add enabled state 0 or state -1 details
		if ( [filter enabled:Detail_Type(details[i])].intValue == 1)
		    [self addDetail:details[i]];
	    }
	}
	//NSLog(@"BandView for vProc %d skipped adding %d events", vp.vpId, skipped);
    }
    return self;
}

#pragma mark Drawing
- (void)drawRect:(NSRect)rect {

    [[NSColor purpleColor] set];
    [NSBezierPath fillRect:self.bounds];

    int q = 0;
    EventShape *selected = [[logDoc logView] selectedEvent];
    NSColor *oldColor;
    for (EventShape *e in shapes)
    {

	if (! NSIntersectsRect(rect, [e bounds]))
	{
	    q++;
	    continue;
	}
	
	if (e == selected)
	{
	    oldColor = e.color;
	    e.color = [NSColor whiteColor];
	}
	[e drawShape];
	if (e == selected)
	    e.color = oldColor;
    }
    //NSLog(@"BandView for vProc %d skipped drawing %d shapes", [vProc vpId], q);

}


#pragma mark Simples

/// Configurtation function to determine the color of a simple detail
- (NSColor *)colorForSimple:(Detail)d
{
    return [NSColor yellowColor];
}

/// Configuration alorithm for determining the heigh of a simple detail
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

// CURRENTLY UNUSED
// Return the fraction of the current height at which the next singleton should
// be placed
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

    Singleton *singleton = [[Singleton alloc]
		       initWithPoint:NSMakePoint(s, self.singletonHeight)
		       color:c
			   event:Detail_Simple_value(d)];
    singleton.description = [NSString stringWithCString:g->Desc() encoding:NSASCIIStringEncoding];

    [shapes addObject:singleton];
}



#pragma mark States


/// Configurtation function to determine the color of a state detail
/// Now uses configuration information parsed from log-view.json into the group hierarchy
- (NSColor *)colorForState:(Detail)d withGroup:(StateGroup *)g
{
    int i = Detail_State_state(d);
    const char *color_string = g->StateColor(i);
    NSColor *ret = [Utils colorFromFormatString:color_string];
    return ret;
}


// CURRENTLY UNUSED
int color_int = 0;
- (NSColor *)colorForStateOld:(Detail)d
{
    color_int++;
    if (color_int >= 3) color_int -= 3;
    switch (color_int)
    {
	case 0:
	    return [NSColor blueColor];
	case 1:
	    return [NSColor yellowColor];
	case 2:
	    return [NSColor orangeColor];
	default:
	    [Exceptions raise:@"impossible color_int"];
	    return [NSColor blackColor];
    }
}

- (void)addState:(StateGroup *)g forDetail:(Detail)d
{
//    NSLog(@"BandView is adding a state");
    NSColor *c = [self colorForState:d withGroup:g];
    NSRect bounds = self.bounds;
    NSRect r;

    r.origin.y = bounds.origin.y;
    r.size.height = bounds.size.height;
    event *start = Detail_State_start(d);
    event *end = Detail_State_end(d);
    

   //NSLog(@"BandView: state start %#x and end %#x", start, end);

    uint64_t time;
    r.origin.x = 0;
    if (start)
    {
	time = Event_Time(*start);
	if (time >= logDoc.logInterval->x)
	    r.origin.x = [logDoc image:time];
    }
    r.size.width = [logDoc image:logDoc.logInterval->x + logDoc.logInterval->width];
    
    if (end)
    {
	time = Event_Time(*end);
	if (time <= logDoc.logInterval->x + logDoc.logInterval->width)
	    r.size.width = [logDoc image:time];
    }


    if (r.size.width <= TINY_WIDTH) ;//return;
    State *state = [[State alloc] initWithRect:r
					 color:c
					 start:start
					   end:end];

    state.description = [NSString stringWithCString:g->Desc() encoding:NSASCIIStringEncoding];
    [shapes addObject:state];
}

#pragma mark Intervals

/// Configurtation function to determine the color of an interval detail
- (NSColor *)colorForInterval:(Detail)d withGroup:(IntervalGroup *)g
{
    const char *color_string = g->Color();
    NSColor *ret = [Utils colorFromFormatString:color_string];
    return ret;
}

// CURRENTLY UNUSED
- (NSColor *)colorForIntervalOld:(Detail)d
{
    return [NSColor redColor];
}

- (CGFloat)intervalHeightForIntervalOfHeight:(CGFloat)h forDetail:(Detail)d
{
    assert (Detail_Type(d)->Kind() == INTERVAL_GROUP );
    return fmod (Detail_Interval_height(d), ( self.bounds.size.height - h - INTERVAL_PADDING ));
}

- (void)addInterval:(IntervalGroup *)g forDetail:(Detail)d
{
    event *start = Detail_Interval_start(d);
    event *end = Detail_Interval_end(d);
    if (start == NULL || end == NULL)
    {
	NSLog(@"BandView.mm: an interval whose start or end does not exist");
	return;
    }
    NSColor *c = [self colorForInterval:d withGroup:g];
    NSRect r;
    uint64_t time;
    
    time = Event_Time(*start);
    r.origin.x = 0;
    if (time >= logDoc.logInterval->x)
	r.origin.x = [logDoc image:time];
    
    time = Event_Time(*end);
    r.size.width = [logDoc image:logDoc.logInterval->x + logDoc.logInterval->width];
    if (time <= logDoc.logInterval->x + logDoc.logInterval->width)
	r.size.width = [logDoc image:time];

    
    if (r.size.width <= TINY_WIDTH) return;
    r.size.height = DEFAULT_INTERVAL_HEIGHT;
    r.origin.y = [self intervalHeightForIntervalOfHeight:r.size.height forDetail:(Detail)d];
   // NSLog(@"BandView is adding an interval: origin = %f, %f size = %f %f",
	//  r.origin.x, r.origin.y, r.size.width, r.size.height);

    Interval *interval = [[Interval alloc] initWithRect:r
						  color:c
						  start:start
						    end:end];

    interval.description = [NSString stringWithCString:g->Desc() encoding:NSASCIIStringEncoding];

    [shapes addObject:interval];

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

// Perform the appropriate kind of zooming based on the clicks the user makes

- (void)mouseDown:(NSEvent *)e
{
    [super mouseDown:e];

    // If the user is holding down either the shift or the control key
    // Then we interpret the event as follows:
    // The user does not want the DetailInfoView to display information about the detail
    // he clicked on.  Instead the user is trying to zoom in/out if he pressed shift/control
    // respectively.
    NSPoint p = [self convertPoint:e.locationInWindow fromView:nil];
    if (e.modifierFlags & NSShiftKeyMask)
    {
	[logDoc zoomInAboutPivot:[logDoc preImage:p.x]];
	return;
    }
    else if (e.modifierFlags & NSControlKeyMask)
    {
	[logDoc zoomOutAboutPivot:[logDoc preImage:p.x]];
	return;
    }

    // No important keys were being held down while the user clicked the mouse.
    // We interpret the event as follows:
    //	    The user was trying to click on a dependent detail.
    //	    If all dependent details are to far away from the click, then
    //	    the user was trying to click on some nearby state, interval, or simple event

    if ([messageView bandReceivedEvent:e]) return;
    
    EventShape *first = NULL;
    EventShape *newSelection = NULL;
    BOOL foundOldSelection = NO;
    for (EventShape *a in shapes)
    {
	if ([a containsPoint:p])
	{
	    /* We want clicking repeatedly on an area to cycle through all of
	       the events that could be stacked on top of each other.
	     */
	    if (first == NULL)
		first = a;

	    if (foundOldSelection)
	    {
		newSelection = a;
		break;
	    }
	    
	    if (a == logDoc.logView.selectedEvent)
		foundOldSelection = YES;
	}
    }
    /* At this point, there are three distinct possible circumstances:
       1. we never found the old selection. Use the first event; the user has
	  clicked on a new area of the screen.
       2. we found the old selection, but newSelection is still NULL. we have
          reached the last event under the cursor, so cycle back to first.
       3. we found neither a first nor a newSelection. This means there weren't
	  any events under the cursor.
     */

    if (!foundOldSelection || newSelection == NULL)
	newSelection = first;

    [[logDoc logView] didSelectEvent:newSelection fromBand:self];
    [logDoc displayDetail:newSelection];
}

@end
