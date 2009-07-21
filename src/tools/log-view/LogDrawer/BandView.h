/*! \file BandView.h
 \author Korei Klein
 \date 7/7/09
*/

#import <Cocoa/Cocoa.h>
#import "MessageView.h"
#import "State.h"

struct IntervalGroup;

/// A view representing a vproc
/*!
 A BandView is a view which represents the events for a single vproc.
 BandViews do not represent message events.
 */
@interface BandView : NSView {
    /// The MessageView this band should use to display its dependent events
    IBOutlet MessageView *messageView;
    
    NSMutableArray *states;
    NSMutableArray *intervals;
    NSMutableArray *singletons;
    NSMutableArray *messages;
    
    
    State *lastState;
    NSColor *stateStartColor;
    
    NSColor *bandColor;
    
    
    CGFloat cur_singleton_height;
    NSMapTable *intervalMap; //< IntervalGroup -> most recently added interval event from that group
}

@property (readonly, assign) NSRect shapeBounds; ///< The rectangle which shapes may be drawn in

- (void)setStateStartColor:(NSColor *)c;

- (void)addSingleton:(void *)e withColor:(NSColor *)c andStart:(CGFloat)s;

- (void)addState:(void *)e withColor:(NSColor *)c andStart:(CGFloat)s;

- (void)addIntervalStart:(void *)e
	       withColor:(NSColor *)c
	forIntervalGroup:(struct IntervalGroup *)g
		andStart:(CGFloat)s;
- (void)addIntervalEnd:(void *)e
	forIntervalGroup:(struct IntervalGroup *)g
		andStart:(CGFloat)s;


@end
