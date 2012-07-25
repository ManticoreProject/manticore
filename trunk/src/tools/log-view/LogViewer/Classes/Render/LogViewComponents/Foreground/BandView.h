/*! \file BandView.h
 \author Korei Klein
 \date 7/7/09
*/

#import <Cocoa/Cocoa.h>

#import "Detail.h"


#define DEFAULT_INTERVAL_HEIGHT ( 10 )

@class VProc;
@class LogDoc;
@class SelectedEvent;
@class MessageView;
@class GroupFilter;

struct StateGroup;
struct Group;
struct IntervalGroup;


/// A view representing a vproc
/*!
 A BandView is a view which represents the details for a single vproc.
 These include state, interval, and singleton details.
 BandViews do not represent message deails.
 */
@interface BandView : NSView {
    /// The MessageView this band should use to display its dependent events
    IBOutlet MessageView *messageView;

    LogDoc *logDoc;

    /// The corresponding vproc that this BandView represents
    VProc *vProc;

    NSMutableArray *shapes;

    id target;

    // The following instance variables are for use in the algorithms
    // which compute the heights at which to render intervals and singletons
    // The currenty algorithm draws each X a constant distance higher than the
    // previous X (mod a number a bit less than the height of the view)
    // where X is one of {interval, singleton}

    /// Current height at which to draw intervals
    CGFloat cur_interval_height;
    /// Current height at which to draw singletons
    CGFloat cur_singleton_height;
}

@property (readwrite, assign) MessageView *messageView;
@property (readwrite, assign) id target;
@property (readonly, assign) NSRect shapeBounds; ///< The rectangle which shapes may be drawn in

- (BandView *)initWithFrame:(NSRect)frame
		     logDoc:(LogDoc *)logDocVal
		      vProc:(VProc *)vp
		     filter:(GroupFilter *)filter;

/// Create a shape from a detail and insure that it will be rendered during drowRect.
/// This function will call one of the functions below with the appropriate arguments
- (void)addDetail:(struct TaggedDetail_struct *)d;

// The following functions take a group g as their first argument,
//	and a detail d as their second
// In each function, g can be computed quickly from d using Detail_Type, but 
// All calls to each function can easily pass g as a parameter
// thus saving much silly recomputation and error checking
//	(checking that the group type is appropriate for the function)

// The following functions do not render any shapes, but
// create caches of them which will be rendered the next time drawRect is called

/// Prepare to render a simple detail
- (void)addSimple:(struct Group *)g forDetail:(Detail)d;
/// Prepare to render a state detail
- (void)addState:(struct StateGroup *)g forDetail:(Detail)d;
/// Prepare to render an interval detail
- (void)addInterval:(struct IntervalGroup *)g forDetail:(Detail)d;


@end
