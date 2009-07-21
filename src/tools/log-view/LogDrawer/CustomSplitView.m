/** \file CustomSplitView.m
 * \author Korei Klein
 * \date 7/13/09
 */

#import "CustomSplitView.h"

#pragma mark Defined Constants
/// The amount of space between bars



@implementation CustomSplitView

- (id)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code here.
    }
    return self;
}

- (void)drawRect:(NSRect)rect {
    // NSLog(@"CustomSplitView is drawing a rectangle");
}



#pragma mark Divider Thickness
- (CGFloat)dividerThickness
{
    return DIVIDER_THICKNESS;
}

@end
