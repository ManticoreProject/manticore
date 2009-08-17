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



#define TIME_VALUE_FONT_NAME ( @"Helvetica" )
#define TIME_VALUE_FONT_SIZE ( 8 )
#define TIME_VALUE_COLOR ( [NSColor whiteColor] )
/// Number of rightmost digits of the time value to display
#define TIME_VALUE_NUM_DIGITS ( 4 )
#define TIME_VALUE_ROUNDING ( 100000 )
/// Distance between the time values, and the top of the view
#define TIME_VALUE_PADDING ( 14 )


uint64_t myExp(uint64_t a, uint n)
{
    uint64_t ret = 1;
    while (n--) ret *=a;
    return ret;
}

@implementation MessageView

- (CGFloat)heightForVp:(int32_t)vp
{
    return ((logDoc.logView.bands.count - vp - 1) * (logDoc.logView.band_height + DIVIDER_THICKNESS)) +
	logDoc.logView.band_height / 2;
    NSPoint p = NSMakePoint(0, (logDoc.logView.band_height / 2));
    NSArray *bands = logDoc.logView.bands;
    //NSLog(@"messageview has found bands %@", bands);
    BandView *band = [bands objectAtIndex:vp];
    //NSLog(@"Finding height for message in band %@", band);

    NSPoint q = [self convertPoint:p fromView:band];

    return q.y;
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
					 sender:s
				      receiver:r];
    Group *g = Detail_Type(d);
    NSString *S = [NSString stringWithCString:g->Desc() encoding:NSASCIIStringEncoding];
    message.description = [NSString stringWithString:S];
    
    return message;
}

- (MessageView *)initWithFrame:(NSRect)frame
			logDoc:(LogDoc *)logDocVal
		    dependents:(NSArray *)dependentsVal
{
    if (![super initWithFrame:frame]) return nil;
    logDoc = logDocVal;
    timeValueAttributes = [[NSMutableDictionary alloc] init];
    [timeValueAttributes setObject:[NSFont fontWithName:TIME_VALUE_FONT_NAME
						   size:TIME_VALUE_FONT_SIZE]
			    forKey:NSFontAttributeName];
    [timeValueAttributes setObject:TIME_VALUE_COLOR
			    forKey:NSForegroundColorAttributeName];

    times = [[NSMutableArray alloc] init];
    timeValues = [[NSMutableArray alloc] init];

    dependents = [[NSMutableArray alloc] init];
    for (Box *b in dependentsVal)
    {
	struct TaggedDetail_struct *d = (TaggedDetail_struct *) [b unbox];
	for (int i = 0; i < Detail_Dependent_n_dsts(d); ++i)
	{
	    Message *m = [self messageFromDependent:d dst:i];


	    if (m == nil) continue;
	    //NSLog(@"MessageView is adding a dependent event %@", m);
	    [dependents addObject:m];
	}
    }

    return self;
}

- (void)displayTime:(uint64_t)t atPosition:(CGFloat)f
{
   // NSLog(@"messageview is adding a time to display, position %f, time %qu", f, t);
    NSString *stringRep = [NSString stringWithFormat:@"%qu", (t / TIME_VALUE_ROUNDING) % myExp(10, TIME_VALUE_NUM_DIGITS)];
    NSNumber *n = [NSNumber numberWithFloat:f];
    [times addObject:n];
    [timeValues addObject:stringRep];
}


- (void)drawTimeValue:(NSString *)s atTime:(CGFloat)f
{
    NSRect bounds = self.visibleRect;
    NSPoint p = NSMakePoint
	(f, bounds.origin.y + bounds.size.height - TIME_VALUE_PADDING);
    [s drawAtPoint:p withAttributes:timeValueAttributes];
}

- (void)drawRect:(NSRect)rect {
    /*
    [[NSColor clearColor] set];
    [NSBezierPath fillRect:self.bounds];
    assert (times.count == timeValues.count);
    //NSLog(@"Message view has %d numbers to draw", times.count);
    for (int i = 0; i < times.count; ++i)
    {
	//NSLog(@"MessageView is drawing a number");
	NSNumber *n = [times objectAtIndex:i];
	[self drawTimeValue:[timeValues objectAtIndex:i]
		     atTime:n.floatValue];
    }
    */
    for (Message *m in dependents)
    {
	//NSLog(@"messageview is drawing %@", m);
	[m drawShape];
    }
}

/// Alerts the messageView that the mouse has been clicked
/// Return: whether or not this click corresponded to a message
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

/*
- (void)mouseDown:(NSEvent *)event
{
    NSLog(@"Message view received a mouse down");
    [self.superview mouseDown:event];
}
 */

- (BOOL)isOpaque
{
    return YES;
}



@end
