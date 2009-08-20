/** \file  PieSlice.h
 * \author Korei Klein
 * \date 8/13/09
 *
 */

#import <Cocoa/Cocoa.h>


@interface PieSlice : NSObject {
    /// The fraction of the pice which this slice represents
    double fraction;
    /// The consumer which this slice represents
    NSObject *consumer;
}

@property (readwrite, assign) double fraction;
@property (readwrite, assign) NSObject *consumer;

- (PieSlice *)initWithFraction:(double)fractionVal andConsumer:(NSObject *)c;


- (void)increaseBy:(PieSlice *)other;
- (void)decreaseBy:(PieSlice *)other;
- (void)multiplyBy:(uint64_t)a;
- (void)divideBy:(uint64_t)a;




@end
