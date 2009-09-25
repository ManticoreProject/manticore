/** \file CustomSplitView.h
 * \author Korei Klein
 * \date 7/13/09
 */

#import <Cocoa/Cocoa.h>


#pragma mark Defined Constants

#define Y_PADDING ( 3 )
#define X_PADDING ( 25 )

#define DIVIDER_THICKNESS ( 17 )

/// Customized subclass of NSSplitView
/** CustomSplitView is a subclass of NSView that provides
 * 1. finer controll over the size of the borders between subviews
 * 2. the ability to draw rule lines behind its subviews
 *
 */
@interface CustomSplitView : NSSplitView {
    /// [self subviews] should be and only be the BandViews being displayed
}

@property (readonly, assign) NSRect shapeBounds;

/// Override to change divider thickness
- (CGFloat)dividerThickness;


@end
