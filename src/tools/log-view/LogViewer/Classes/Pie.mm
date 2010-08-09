/** \file  Pie.mm
 * \author Korei Klein
 * \date 8/13/09
 *
 */

#import "Pie.h"
#import "log-desc.hxx"
#import "Utils.h"


@implementation Pie

@synthesize consumers;
@synthesize nConsumers;

- (Pie *)init
{
    consumers = NULL;
    nConsumers = 0;
    return self;
}

- (Pie *)initWithCapacity:(NSUInteger)numItems
{
    consumers = (PieSlice_t *) [Utils calloc:numItems size:sizeof(PieSlice_t)];
    nConsumers = numItems;
    return self;
}

+ (Pie *)emptyForStateGroup:(StateGroup *)g
{
    Pie *ret = [[Pie alloc] initWithCapacity:g->NumStates()];

    for (int i = 0; i < g->NumStates(); ++i)
    {
	ret->consumers[i].fraction = 0.0;
	ret->consumers[i].consumer = i;
    }

    return ret;
}

- (void)increaseBy:(Pie *)pie
{
    assert ( nConsumers == pie.nConsumers );
    for (unsigned int i = 0; i < nConsumers; ++i)
    {
	consumers[i].fraction += pie->consumers[i].fraction;
    }
}
- (void)decreaseBy:(Pie *)pie
{
    assert ( nConsumers == pie.nConsumers );
    for (unsigned int i = 0; i < nConsumers; ++i)
    {
	consumers[i].fraction += pie->consumers[i].fraction;
    }
}
- (void)divideBy:(uint64_t)t
{
    assert ( t != 0 );
    for (unsigned int i = 0; i < nConsumers; ++i)
    {
	consumers[i].fraction /= t;
    }
}
- (void)multiplyBy:(uint64_t)t
{
    for (unsigned int i = 0; i < nConsumers; ++i)
    {
	consumers[i].fraction *= t;
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
    for (unsigned int i = 0; i < nConsumers; i++)
    {
	res += consumers[i].fraction;
    }
    assert ( abs (1 - res) < STOCHASTIC_ERROR );
}



- (void)incrementConsumer:(int)i byAmount:(uint64_t)a
{
    consumers[i].fraction += a;
}

- (Pie *)copy
{
    Pie *res = [[Pie alloc] initWithCapacity:nConsumers];
    for (unsigned int i = 0; i < nConsumers; i++)
    {
	res->consumers[i] = consumers[i];
    }
    return res;
}


@end
