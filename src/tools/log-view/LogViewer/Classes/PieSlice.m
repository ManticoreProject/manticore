/** \file  PieSlice.m
 * \author Korei Klein
 * \date 8/13/09
 *
 */

#import "PieSlice.h"


@implementation PieSlice

@synthesize fraction;
@synthesize consumer;

- (PieSlice *)initWithFraction:(double)fractionVal andConsumer:(NSObject *)c
{
    if (![super init])
    {
	NSLog(@"PieSlice could not init");
	return nil;
    }
    fraction = fractionVal;
    consumer = c;
    return self;
}


- (void)increaseBy:(PieSlice *)other
{
    fraction += other.fraction;
}
- (void)decreaseBy:(PieSlice *)other
{
    fraction -= other.fraction;
}
- (void)multiplyBy:(uint64_t)a
{
    fraction *= a;
}
- (void)divideBy:(uint64_t)a
{
    // NSLog(@"PieSlice %@ dividing %f by %qu to get %f", self, fraction, a, fraction / a);
    assert ( a != 0 );
    fraction = fraction / a;
}


- (PieSlice *)copy
{
    NSNumber *n = [NSNumber numberWithInt:((NSNumber *)consumer).intValue];
    return [[PieSlice alloc] initWithFraction:fraction andConsumer:n];
}

@end



