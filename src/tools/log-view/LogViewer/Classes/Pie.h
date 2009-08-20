/** \file  Pie.h
 * \author Korei Klein
 * \date 8/13/09
 *
 */

#import <Cocoa/Cocoa.h>
struct StateGroup;

@interface Pie : NSObject {
    NSMutableArray *consumers;
}

@property (readwrite, assign) NSMutableArray *consumers;


+ (Pie *)emptyForStateGroup:(struct StateGroup *)g;
- (void)increaseBy:(Pie *)pie;
- (void)decreaseBy:(Pie *)pie;
- (void)divideBy:(uint64_t)t;
- (void)multiplyBy:(uint64_t)t;

- (void)assertStochastic;

- (void)incrementConsumer:(NSNumber *)n byAmount:(uint64_t)a;

@end
