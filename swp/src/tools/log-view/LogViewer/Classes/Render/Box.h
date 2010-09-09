/** \file  Box.h
 * \author Korei Klein
 * \date 7/29/09
 *
 */

/// Box and unbox pointers
/// Terrible to use, this class allows c++ objects to be treated
/// sort of like NSObjects.  Be warned however, the garbage collector
/// does not always look kindly on boxed objects, they may be reclaimed
/// if you use them in conjuction with the wrong libraries and don't keep pointers
/// to them lying around.
/// Don't keep too many pointers to them lying around, however, as doing so can cause a
/// memory leak.
@interface Box : NSObject {
    void *value;
}

+ (Box *)box:(void *)valueVal;
- (void *)unbox;

@end

enum EventGroupBoxT {
    GROUP,
    STATE
};

@interface EventGroupBox : Box {
    enum EventGroupBoxT type;
}

+ (EventGroupBox *)box:(void *)valueVal withType:(enum EventGroupBoxT)type;
@property enum EventGroupBoxT type;


@end