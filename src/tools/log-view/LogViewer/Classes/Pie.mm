/** \file  Pie.mm
 * \author Korei Klein
 * \date 8/13/09
 *
 */

#import "Pie.h"
#import "log-desc.hxx"
#import "PieSlice.h"


@implementation Pie

@synthesize consumers;

- (Pie *)init
{
    if (![super init]) return nil;
    consumers = [[NSMutableArray alloc] init];
    return self;
}


+ (Pie *)emptyForStateGroup:(StateGroup *)g
{
    Pie *ret = [[Pie alloc] init];


    for (int i = 0; i < g->NumStates(); ++i)
    {
	PieSlice *ps = [[PieSlice alloc] initWithFraction:0.0
					      andConsumer:[NSNumber numberWithInt:i]];
	[ret.consumers addObject:ps];
    }

    return ret;
}
- (void)increaseBy:(Pie *)pie
{
    assert ( consumers.count == pie.consumers.count );
    for (int i = 0; i < consumers.count; ++i)
    {
	PieSlice *ps = [consumers objectAtIndex:i];
	PieSlice *other_ps = [pie.consumers objectAtIndex:i];
	[ps increaseBy:other_ps];
    }
}
- (void)decreaseBy:(Pie *)pie
{
    assert ( consumers.count == pie.consumers.count );
    for (int i = 0; i < consumers.count; ++i)
    {
	PieSlice *ps = [consumers objectAtIndex:i];
	PieSlice *other_ps = [pie.consumers objectAtIndex:i];
	[ps decreaseBy:other_ps];
    }
}
- (void)divideBy:(uint64_t)t
{
    assert ( t != 0 );
    for (int i = 0; i < consumers.count; ++i)
    {
	PieSlice *ps = [consumers objectAtIndex:i];
	[ps divideBy:t];
    }
}
- (void)multiplyBy:(uint64_t)t
{
    for (int i = 0; i < consumers.count; ++i)
    {
	PieSlice *ps = [consumers objectAtIndex:i];
	[ps multiplyBy:t];
    }
}



/** because of rounding errors, it may not be the case that the sum of the consumers in a pie
  * is equal to 1.0, but it should be close.  We will consider the pie to be well formed iff
  * the sum of the consumption of resources is within STOCHASTIC_ERROR of 1.0
  */
#define STOCHASTIC_ERROR ( 0.2 )
- (void)assertStochastic
{
    double res = 0.0;
    for (PieSlice *ps in consumers)
    {
	res += ps.fraction;
    }
    assert ( abs (1 - res) < STOCHASTIC_ERROR );
}



- (void)incrementConsumer:(NSNumber *)n byAmount:(uint64_t)a
{
    assert (n != NULL);
    PieSlice *ps = [consumers objectAtIndex:n.intValue];
    ps.fraction = ps.fraction + a;
}

- (Pie *)copy
{
    Pie *res = [[Pie alloc] init];
    for (PieSlice *ps in consumers)
    {
	[res.consumers addObject:[ps copy]];
    }
    return res;
}


@end
