/** \file  Pie.h
 * \author Korei Klein
 * \date 8/13/09
 *
 */

#import <Cocoa/Cocoa.h>
struct StateGroup;

/** Model object representing any data which might be though of
 * as a pie chart.
 * self has a consumer for each state in the StateGroup g used to initialize self
 * each consumer takes up a certain amount of time.
 */
@interface Pie : NSObject {
    NSMutableArray *consumers;
}

@property (readwrite, assign) NSMutableArray *consumers;

/// Return a new empty pie initialized for StateGroup g
+ (Pie *)emptyForStateGroup:(struct StateGroup *)g;

// a pie can be though of as a function from a consumer c to the amount of time c takes up
// pies are thus a vector space

/// pointwise addition
- (void)increaseBy:(Pie *)pie;
/// pointwise subtraction
- (void)decreaseBy:(Pie *)pie;
/// scalar division
- (void)divideBy:(uint64_t)t;
/// scalar multiplication
- (void)multiplyBy:(uint64_t)t;

/// this function fails if ||self|| != 1
/// or, this function fails if the sum of all the times taken up by the consumers is not 1
- (void)assertStochastic;

/// increment the time taken up by the given consumer by the given amount
- (void)incrementConsumer:(NSNumber *)n byAmount:(uint64_t)a;

/// return a copy of this Pie
- (Pie *)copy;

@end
