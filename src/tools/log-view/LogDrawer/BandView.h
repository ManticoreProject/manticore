/*! \file BandView.h
 \author Korei Klein
 \date 7/7/09
*/

#import <Cocoa/Cocoa.h>
#import "MessageView.h"

/// A view representing a vproc
/*!
 A BandView is a view which represents the events for a single vproc.
 BandViews do not represent message events.
 */
@interface BandView : NSView {
    /// The MessageView this band should use to display its dependent events
    IBOutlet MessageView *messageView;
    NSMutableArray *shapes;
    NSMutableArray *states; //< Necessary because at first we wont know when a state ends
}


- (void)addEvent:(void *)e withColor:(NSColor *)c andStart:(CGFloat)s;
- (void)addState:(void *)e withColor:(NSColor *)c andStart:(CGFloat)s;

@end
