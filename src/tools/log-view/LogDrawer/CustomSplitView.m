/** \file CustomSplitView.m
 * \author Korei Klein
 * \date 7/13/09
 */

#import "CustomSplitView.h"


@implementation CustomSplitView

- (BOOL)isOpaque
{
    return YES;
}


/// Return the bounds of this CustomSplitView, but shrunk a little
- (NSRect)shapeBounds
{
    NSRect r = self.bounds;
    r.origin.x += X_PADDING;
    r.origin.y += Y_PADDING;
    r.size.width -= 2 * X_PADDING;
    r.size.height -= 2 * Y_PADDING;
    return r;
}


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
