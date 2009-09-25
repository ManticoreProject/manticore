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
    if (![super init]) return nil;
    
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
    assert ( a != 0 );
    fraction /= a;
}

@end



