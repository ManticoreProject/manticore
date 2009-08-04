/** \file  MessageView.m
 * \author Korei Klein
 * \date 7/7/09
 */

#import "MessageView.h"

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

- (MessageView *)initWithFrame:(NSRect)frame
			logDoc:(LogDoc *)logDocVal
		    dependents:(NSArray *)dependentsVal
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
    return self;
}

- (void)displayTime:(uint64_t)t atPosition:(CGFloat)f
{
    NSLog(@"messageview is adding a time to display, position %f, time %qu", f, t);
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
    [[NSColor clearColor] set];
    [NSBezierPath fillRect:self.bounds];
    assert (times.count == timeValues.count);
    NSLog(@"Message view has %d numbers to draw", times.count);
    for (int i = 0; i < times.count; ++i)
    {
	NSLog(@"MessageView is drawing a number");
	NSNumber *n = [times objectAtIndex:i];
	[self drawTimeValue:[timeValues objectAtIndex:i]
		     atTime:n.floatValue];
    }
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
    return NO;
}



@end
