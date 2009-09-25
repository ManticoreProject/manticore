/** \file  Box.h
 * \author Korei Klein
 * \date 7/29/09
 *
 */

#import <Cocoa/Cocoa.h>


@interface Box : NSObject {
    void *value;
}

+ (Box *)box:(void *)valueVal;
- (void *)unbox;

@end
