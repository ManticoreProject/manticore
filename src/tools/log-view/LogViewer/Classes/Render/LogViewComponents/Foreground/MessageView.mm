/** \file  MessageView.m
 * \author Korei Klein
 * \date 7/7/09
 */


#import "MessageView.h"
#import "DetailAccess.h"
#import "CustomSplitView.h"
#import "BandView.h"
#import "Message.h"
#import "Box.h"
#import "LogView.h"
#import "LogDoc.h"
#import "log-desc.hxx"
#import "GroupFilter.h"
#import "Utils.h"


/// The font of the text used to represent times
#define TIME_VALUE_FONT_NAME ( @"Helvetica" )
/// The size of the font of the text used to represent times
#define TIME_VALUE_FONT_SIZE ( 8 )
/// The color of the text used to represent times
#define TIME_VALUE_COLOR ( [NSColor whiteColor] )
/// Number of rightmost digits of the time value to display
#define TIME_VALUE_NUM_DIGITS ( 4 )
#define TIME_VALUE_ROUNDING ( 100000 )
/// Distance between the time values, and the top of the view
#define TIME_VALUE_PADDING ( 14 )


/// Exponentiation function
uint64_t myExp(uint64_t a, uint n)
{
    uint64_t ret = 1;
    while (n--) ret *=a;
    return ret;
}

@implementation MessageView

/// Determine how high a vproc should be based on its identifier
- (CGFloat)heightForVp:(int32_t)vp
{
    return ((logDoc.logView.bands.count - vp - 1) * (logDoc.logView.band_height + DIVIDER_THICKNESS)) +
	logDoc.logView.band_height / 2;
}

- (Message *)messageFromDependent:(struct TaggedDetail_struct *)d dst:(int)i
{
    NSPoint p1, p2;
    event *s, *r;
    struct Dependent_Dst *dep_dst;

    s = Detail_Dependent_src(d);
    if (s == nil)
    {
	NSLog(@"MessageView was asked to add an event with no source");
	return nil;
    }
    dep_dst = Detail_Dependent_dst(d, i);

    r = Detail_Dependent_Dst_Event(dep_dst);

    DependentGroup *g = static_cast<DependentGroup*>(Detail_Type(d));

    int32_t src_vpId = Detail_Dependent_src_VpId(d);
    int32_t dst_vpId = Detail_Dependent_Dst_VpId(dep_dst);

    p1.x = [logDoc image:Event_Time(*s)];
    p1.y = [self heightForVp:src_vpId];

    p2.x = [logDoc image:Event_Time(*r)];
    p2.y = [self heightForVp:dst_vpId];

 //   NSLog(@" message %x from vp %d to vp %d from %f, %f to %f, %f",
	//    d,
	//    src_vpId, dst_vpId,
	//    p1.x, p1.y,
	//    p1.x, p2.y);
//
    Message *message = [[Message alloc] initArrowFromPoint:p1
						   toPoint:p2
						     color:[Utils colorFromFormatString:g->Color()]
						    sender:s
						  receiver:r];
    NSString *S = [NSString stringWithCString:g->Desc() encoding:NSASCIIStringEncoding];
    message.description = [NSString stringWithString:S];

    return message;
}

- (MessageView *)initWithFrame:(NSRect)frame
{
    if (![super initWithFrame:frame]) return nil;

    timeValueAttributes = [[NSMutableDictionary alloc] init];
    [timeValueAttributes setObject:[NSFont fontWithName:TIME_VALUE_FONT_NAME
						   size:TIME_VALUE_FONT_SIZE]
			    forKey:NSFontAttributeName];
    [timeValueAttributes setObject:TIME_VALUE_COLOR
			    forKey:NSForegroundColorAttributeName];

    times = [[NSMutableArray alloc] init];
    timeValues = [[NSMutableArray alloc] init];

    // dependents will contain one message for every dependent
    // in dependentsVal which is enabled
    dependents = [[NSMutableArray alloc] init];

    return self;
}

- (void)updateDependents:(NSArray *)dependentsVal
{
    [dependents removeAllObjects];
    for (Box *b in dependentsVal)
    {
	struct TaggedDetail_struct *d = (TaggedDetail_struct *) [b unbox];
	for (int i = 0; i < Detail_Dependent_n_dsts(d); ++i)
	{
	    Group *g = Detail_Type(d);
	    if ([logDoc.filter enabled:g].intValue != 1) continue;
	    
	    Message *m = [self messageFromDependent:d dst:i];
	    
	    if (m == nil) continue;
	    //NSLog(@"MessageView is adding a dependent event %@", m);
	    [dependents addObject:m];
	}
    }
}

- (void)drawRect:(NSRect)rect {

    //[logDoc.logView needsDisplay];
    NSLog(@"MessageView going to draw %@", [Utils rectString:rect]);

    NSRect bounds = [self bounds];
    int a = 0;
    for (Message *m in dependents)
    {
	if (! NSIntersectsRect(rect, [[m path] bounds]))
	{
	    a++;
	    continue;
	}
	[m drawShape];
    }
    //NSLog(@"Skipped drawing %d messages", a);
    
    // draw cursor line

    NSBezierPath *verticalLine = [[NSBezierPath alloc] init];
    NSPoint s, f;
	    
    verticalLine.lineWidth = 0.0;
    [[NSColor whiteColor] set];
    s.x = f.x = [[logDoc logView] mouseLoc].x + .5; // add .5 to remove anti-aliasing issue
    s.y = 0;
    f.y = bounds.size.height;
    [verticalLine moveToPoint:s];
    [verticalLine lineToPoint:f];
    [verticalLine stroke];
}

/// Alerts the messageView that the mouse has been clicked
/// Return: whether or not this click clicked on a message
- (BOOL)bandReceivedEvent:(NSEvent *)e
{
    NSLog(@"MessageView is checking if a message was clicked on");
    for (Message *m in dependents)
    {
	NSPoint p = [self convertPoint:e.locationInWindow fromView:nil];

	if ([m containsPoint:p])
	{
	    [logDoc displayDetail:m];
	    NSLog(@"MessageView found a clicked message");
	    return YES;
	}
    }
    return NO;
}

- (BOOL)isOpaque
{
    return YES;
}


@end


// OLD CODE

/*
/// Store the given time as needing display
- (void)displayTime:(uint64_t)t atPosition:(CGFloat)f
{
   // NSLog(@"messageview is adding a time to display, position %f, time %qu", f, t);
    NSString *stringRep = [NSString stringWithFormat:@"%qu", (t / TIME_VALUE_ROUNDING) % myExp(10, TIME_VALUE_NUM_DIGITS)];
    NSNumber *n = [NSNumber numberWithFloat:f];
    [times addObject:n];
    [timeValues addObject:stringRep];
}


/// draw a time.  It should have already been stored as needing display by @selector(displayTime:atPosition:)
- (void)drawTimeValue:(NSString *)s atTime:(CGFloat)f
{
    NSRect bounds = self.visibleRect;
    NSPoint p = NSMakePoint
	(f, bounds.origin.y + bounds.size.height - TIME_VALUE_PADDING);
    [s drawAtPoint:p withAttributes:timeValueAttributes];
}
*/

