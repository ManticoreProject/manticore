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
	singletons = [[NSMutableArray alloc] init];
	states = [[NSMutableArray alloc] init];
	intervals = [[NSMutableArray alloc] init];
	
	NSRect bounds = self.shapeBounds;
	cur_singleton_height = bounds.size.height / 2;
	cur_interval_height = bounds.size.height / 2;

	Detail *details = vp.details;
	for (int i = 0; i < vp.numDetails; ++i)
	{
	    if (![logDoc isInInterval:details[i]])
	    {
//		Group *g = Detail_Type(details[i]);
//		if (g->Kind() == STATE_GROUP
	    }
	    else
	    {
		Group *g = Detail_Type(details[i]);
		int doDisplay = [filter enabled:g].intValue;
		// NSLog(@"doDisplay = %d", doDisplay);
#ifdef DEBUG
		if (g->Kind() == STATE_GROUP)
		{
		    if (Detail_State_start(details[i]) == NULL)
		    {
			NSLog(@"Detail number %d is being added, with no start event", i);
		    }
		}
#endif
		if (doDisplay == 1)
		{
		    [self addDetail:details[i]];
		}
	    }
	}
    }
    return self;
}

#pragma mark Drawing
- (void)drawRect:(NSRect)rect {

   // [[NSColor blueColor] set];
   // [NSBezierPath fillRect:self.bounds];
    
    int a = 0;
    for (State *e in states)
    {
	[e drawShape];
	++a;
    }
   // NSLog(@"Drew %d state changes", a);

    int b = 0;
    for (Interval *e in intervals)
    {
	[e drawShape];
	++b;
    }
  //  NSLog(@"Drew %d intervals", b);

    int c = 0;
    for (EventShape *e in singletons)
    {
	[e drawShape];
	++c;
    }
   // NSLog(@"Drew %d singletons", c);

   // NSLog(@"BandView drew %d shapes", a + b + c);
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

    Singleton *singleton = [[Singleton alloc]
		       initWithPoint:NSMakePoint(s, self.singletonHeight)
		       color:c
			   event:Detail_Simple_value(d)];
    NSString *S = [NSString stringWithCString:g->Desc() encoding:NSASCIIStringEncoding];
    singleton.description = [NSString stringWithString:S];

    [singletons addObject:singleton];
}



#pragma mark States


- (NSColor *)colorForState:(Detail)d withGroup:(StateGroup *)g
{
    int i = Detail_State_state(d);
    const char *color_string = g->StateColor(i);
    NSColor *ret = [Utils colorFromFormatString:color_string];
    return ret;
}


// XXX CURRENTLY UNUSED
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
    
    r.origin.x = [logDoc image:start ? Event_Time(*start) : logDoc.logInterval->x];
    r.size.width = [logDoc image:(end ? Event_Time(*end) : logDoc.logInterval->x + logDoc.logInterval->width)] - r.origin.x;

    if (r.size.width <= TINY_WIDTH) ;//return;
    State *state = [[State alloc] initWithRect:r
					 color:c
					 start:start
					   end:end];
    
    NSString *S = [NSString stringWithCString:g->Desc() encoding:NSASCIIStringEncoding];
    state.description = [NSString stringWithString:S];
    [states addObject:state];
}

#pragma mark Intervals

- (NSColor *)colorForInterval:(Detail)d withGroup:(IntervalGroup *)g
{
    const char *color_string = g->Color();
    NSColor *ret = [Utils colorFromFormatString:color_string];
    return ret;
}

// XXX CURRENTLY UNUSED
- (NSColor *)colorForIntervalOld:(Detail)d
{
    return [NSColor redColor];
}

- (CGFloat)intervalHeightForIntervalOfHeight:(CGFloat)h forDetail:(Detail)d
{
    assert (Detail_Type(d)->Kind() == INTERVAL_GROUP );
    return fmod (Detail_Interval_height(d), ( self.bounds.size.height - h - INTERVAL_PADDING ));
    
    
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
	NSLog(@"BandView.mm: an interval whose start or end does not exist");
	return;
    }
    NSColor *c = [self colorForInterval:d withGroup:g];
    NSRect r;
    r.origin.x = [logDoc image:Event_Time(*start)];
    r.size.width = [logDoc image:Event_Time(*end)] - r.origin.x;
    if (r.size.width <= TINY_WIDTH) return;
    r.size.height = DEFAULT_INTERVAL_HEIGHT;
    r.origin.y = [self intervalHeightForIntervalOfHeight:r.size.height forDetail:(Detail)d];
   // NSLog(@"BandView is adding an interval: origin = %f, %f size = %f %f",
	//  r.origin.x, r.origin.y, r.size.width, r.size.height);

    Interval *interval = [[Interval alloc] initWithRect:r
						  color:c
						  start:start
						    end:end];

    NSString *S = [NSString stringWithCString:g->Desc() encoding:NSASCIIStringEncoding];
    interval.description = [NSString stringWithString:S];

    [intervals addObject:interval];

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
    if (e.modifierFlags & NSShiftKeyMask)
    {
	NSPoint p = [self convertPoint:e.locationInWindow fromView:nil];
	[logDoc zoomInAboutPivot:[logDoc preImage:p.x]];
	return;
    }
    else if (e.modifierFlags & NSControlKeyMask)
    {
	NSPoint p = [self convertPoint:e.locationInWindow fromView:nil];
	[logDoc zoomOutAboutPivot:[logDoc preImage:p.x]];
	return;
    }
    
    NSLog(@"Bandview is sending a messageView %@ the mouse event %@", messageView, e);
    if ([messageView bandReceivedEvent:e]) return;
    NSLog(@"Mouse was clicked on band for VProc %d", vProc.vpId);
    NSPoint p = [self convertPoint:e.locationInWindow fromView:nil];
    for (State *a in states)
    {
	if ([a containsPoint:p])
	{
	    [logDoc displayDetail:a];
	    [a nslog];
	    NSLog(@"State clicked");
	}
    }
    for (Interval *a in intervals)
    {
	if ([a containsPoint:p])
	{
    	    NSLog(@"Interval clicked");
	    [logDoc displayDetail:a];
	}
    }

    for (Singleton *a in singletons)
    {
	if ([a containsPoint:p])
	{
    	    [logDoc displayDetail:a];
	    NSLog(@"Singleton clicked");	    
	}
    }
    
    //[self.superview mouseDown:e];
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
     { color is
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
