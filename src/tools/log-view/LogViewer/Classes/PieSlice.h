/** \file  PieSlice.h
 * \author Korei Klein
 * \date 8/13/09
 *
 */

#import <Cocoa/Cocoa.h>

/**
 * A PieSlice is meant to act as a consumer in a Pie.
 */
@interface PieSlice : NSObject {
    /// The fraction of the pie which this slice represents
    /// or, some scalar multiple of the amount of time this slice represents
    double fraction;
    /// The consumer which this slice represents
    /// Right now it is implemented as some state of a stateGroup g
    ///	    thus it is an NSNumber containing an integer
    NSObject *consumer;
}

@property (readwrite, assign) double fraction;
@property (readwrite, assign) NSObject *consumer;

- (PieSlice *)initWithFraction:(double)fractionVal andConsumer:(NSObject *)c;


- (void)increaseBy:(PieSlice *)other;
- (void)decreaseBy:(PieSlice *)other;
- (void)multiplyBy:(uint64_t)a;
- (void)divideBy:(uint64_t)a;


- (PieSlice *)copy;

@end
