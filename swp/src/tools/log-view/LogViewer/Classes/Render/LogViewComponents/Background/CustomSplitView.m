/** \file CustomSplitView.m
 * \author Korei Klein
 * \date 7/13/09
 *
 *
 */

#import "CustomSplitView.h"
#import "ViewController.h"
#import "Utils.h"


@implementation CustomSplitView
/* 
- (BOOL)isOpaque
{
    return NO;
} */


/// Return the bounds of this CustomSplitView,
/// but shrunk a little to make drawing a bit prettier
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


- (void) drawRect:(NSRect)dirtyRect
{    
    /* This is a bit of a hack. We're violating Cocoa's
       drawing convention by not calling [view needsDisplay] instead of
       [view drawRect] directly).
     
       Without this line (and you can try it, just comment it out), what happens
       is that the NSSplitView's transparent divider rectangles become polluted
       with old colors, because for whatever reason Cocoa refuses to tell LogView
       to redraw itself in the coordinates given by the dividing rectangles when
       LogView is not visible directly above or below the SplitView on the y-axis.
     
       This is obviously a problem because the divider rectangles are transparent,
       and thus need their background to redraw when they redraw.
     
       I tested this exact scenario in a fresh project by creating a background
       view that draws itself red always, an NSSplitView on top of that with
       two subviews that always draw themselves blue. In that situation,
       I didn't observe the broken behavior: Even when the background view was
       only visible by looking "through" the transparent divider rectangles,
       Cocoa correctly told the background view to redraw the area behind the
       rectangles.
     
       Unable to reproduce this bug in a fresh project, and having attempted to
       debug this problem for 2 days without luck, I concede by forcing the
       background to redraw.
     
       - Jordan
     */
    [[self superview] drawRect:dirtyRect];
}


- (CGFloat) dividerThickness
{
    return DIVIDER_THICKNESS;
}


@end
