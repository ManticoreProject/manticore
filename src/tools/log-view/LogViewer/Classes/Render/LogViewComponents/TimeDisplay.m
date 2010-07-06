/** \file  TimeDisplay.m
 * \author Korei Klein
 * \date 8/5/09
 *
 */

#import "TimeDisplay.h"
#import "LogDoc.h"
#import "Exceptions.h"
#import "LogView.h"

#define TIME_VALUE_FONT_NAME ( @"Helvetica" )
#define TIME_VALUE_FONT_SIZE ( 12 )
#define TIME_VALUE_COLOR ( [NSColor blackColor] )
#define TIME_HEIGHT ( 13 )
#define TICKS_PER_NUMBER ( 2 )

// Calculation of units to use at different scales

    // If logInterval->width is less than NANO, times are in nanoseconds
#define NANO ( 10 )
#define NANO_ROUNDING ( 1 )

#define MICRO ( 100000000 )
#define MICRO_ROUNDING ( 1000 )

#define MILLI ( 10000000000 )
#define MILLI_ROUNDING ( 1000000 )

#define SEC_ROUNDING ( 1000000000 )

#define DIGITS_FOR_MIDDLE_TIMES ( 10000 )

@implementation TimeDisplay

@synthesize enabled;

@synthesize ticks;

- (NSString *)timeSuffix
{
    NSString *ret;
    
    switch (rounding)
    {
	case NANO_ROUNDING:
	    ret = @" ns";
	    break;
	case MICRO_ROUNDING:

	    ret = @" \u03BCs";
	    break;
	case MILLI_ROUNDING:
	    ret = @" ms";
	    break;
	case SEC_ROUNDING:
	    ret = @" sec";
	    break;
	default:
	    [Exceptions raise:@"Disallowed rounding time was set in TimeDisplay.m"];
    }
    return ret;
}

- (NSString *)stringFromTime:(uint64_t)t edge:(BOOL)e
{
    uint64_t r = t / rounding;
    if (!e) r = r % DIGITS_FOR_MIDDLE_TIMES;
    NSString *ret = [[NSString alloc] initWithFormat:e ? @"%qu" : @"... %qu", r];
    ret = [ret stringByAppendingString:[self timeSuffix]];
    return ret;
}

- (void)determineDroppedLeftRightDigitsForTime:(uint64_t)t inArea:(CGFloat)a
{
    if (t < NANO)
    {
	rounding = NANO_ROUNDING;
    }
    else if (t < MICRO)
    {
	rounding =  MICRO_ROUNDING;
    }
    else if (t < MILLI)
    {
	rounding =  MILLI_ROUNDING;
    }
    else // ticks are to be displayed as seconds
    {
	rounding =  SEC_ROUNDING;
    }
}

- (uint64_t)rounding
{
    if (rounding == 0)
	[self determineDroppedLeftRightDigitsForTime:logDoc.logInterval->width
					      inArea:0];
    return rounding;
}

- (IBAction)drewTicks:(LogView *)sender
{
    NSRect bounds = self.bounds;
    NSPoint p;




    p.y = 0;

    ticks = [[NSMutableArray alloc] init];
    //NSLog(@"sender has %d ticks", sender.ticks.count);
    for (NSNumber *n in sender.ticks)
    {
	CGFloat f = n.floatValue;
	p.x = f;
	p = [self convertPoint:p fromView:sender];
	
	if (p.x < bounds.origin.x || p.x > bounds.origin.x + bounds.size.width)
	{
	   // NSLog(@"TimeDisplay: dropping time %f because it was not in range", p.x);
	    continue;
	}
	else
	{
	    [ticks addObject:[NSNumber numberWithFloat:f]];
	}
    }

    assert (ticks.count >= 2);
    NSNumber *fst = [ticks objectAtIndex:0];
    NSNumber *lst = [ticks lastObject];
    assert (fst != nil);
    assert (lst != nil);

    CGFloat startTime = fst.floatValue;
    CGFloat endTime = lst.floatValue;
    [self determineDroppedLeftRightDigitsForTime:logDoc.logInterval->width
					  inArea:endTime - startTime];

    // print the starting and ending times
    startString = [self stringFromTime:[logDoc preImage:startTime] edge:true];
    endString = [self stringFromTime:[logDoc preImage:endTime] edge:true];
    //NSLog(@"set startstring %@ endstring %@", startString, endString);

    

    NSMutableArray *ret = [[NSMutableArray alloc] init];
    for (int i = TICKS_PER_NUMBER; i < ticks.count - TICKS_PER_NUMBER; i += TICKS_PER_NUMBER)
    {
	[ret addObject:[ticks objectAtIndex:i]];
    }
    ticks = ret;
    
    self.enabled = true;
    self.needsDisplay = true;
    
}

- (id)initWithFrame:(NSRect)frame {
    if (![super initWithFrame:frame])
	return nil;
    NSRect bounds = self.bounds;
    
    startPoint = bounds.origin; 
    endPoint = bounds.origin; endPoint.x += bounds.size.width;
    startPoint.y += TIME_HEIGHT;
    endPoint.y += TIME_HEIGHT;
    
    self.enabled = false;
    
    startString = @"";
    endString = @"";
    
    attributes = [[NSMutableDictionary alloc] init];
    [attributes setObject:[NSFont fontWithName:TIME_VALUE_FONT_NAME
						   size:TIME_VALUE_FONT_SIZE]
			    forKey:NSFontAttributeName];
    [attributes setObject:TIME_VALUE_COLOR
			    forKey:NSForegroundColorAttributeName];    
    return self;
}

- (void)drawRect:(NSRect)rect
{
    return;
    if (!self.enabled) return;
    //[[NSColor grayColor] set];
    //[NSBezierPath fillRect:self.bounds];
    //NSLog(@"TimeDisplay startpoint = %f, %f endpoint = %f, %f",
	 // startPoint.x, startPoint.y, endPoint.x, endPoint.y);
    //NSLog(@"startstring = %@ endstring = %@", startString, endString);

    
    NSPoint p = endPoint;
    p.x -= [endString sizeWithAttributes:attributes].width;
    
    [startString drawAtPoint:startPoint withAttributes:attributes];
    [endString drawAtPoint:p withAttributes:attributes];

    //NSLog(@"printing %d ticks", ticks.count);
    for (int i = 0; i < ticks.count - 0; i += 1)
    {
	NSNumber *n = [ticks objectAtIndex:i];
	assert (n != nil);
	CGFloat f = n.floatValue;
	NSPoint p;
	p.y = 0;
	p.x = f;

	NSString *S = [self stringFromTime:[logDoc preImage:f] edge:false];


	p = [self convertPoint:p fromView:logDoc.logView];
	p.y = endPoint.y;
	[S drawAtPoint:p withAttributes:attributes];
    }
}

@end
