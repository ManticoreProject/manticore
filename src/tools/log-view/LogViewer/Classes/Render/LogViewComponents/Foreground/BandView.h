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


struct IntervalGroup;

/// A view representing a vproc
/*!
 A BandView is a view which represents the events for a single vproc.
 BandViews do not represent message events.
 */
@interface BandView : NSView {
    /// The MessageView this band should use to display its dependent events
    IBOutlet MessageView *messageView;

    LogDoc *logDoc;
    
    VProc *vProc;
    
    
    NSMutableArray *states;
    NSMutableArray *intervals;
    NSMutableArray *singletons;    
    
    id target;
    
    CGFloat cur_interval_height;
    CGFloat cur_singleton_height;
}

@property (readwrite, assign) MessageView *messageView;
@property (readwrite, assign) id target;
@property (readonly, assign) NSRect shapeBounds; ///< The rectangle which shapes may be drawn in

- (BandView *)initWithFrame:(NSRect)frame logDoc:(LogDoc *)logDocVal vProc:(VProc *)vp filter:(GroupFilter *)filter;
- (void)addDetail:(struct TaggedDetail_struct *)d;

- (void)addSimple:(struct Group *)g forDetail:(Detail)d;
- (void)addState:(struct StateGroup *)g forDetail:(Detail)d;
- (void)addInterval:(struct IntervalGroup *)g forDetail:(Detail)d;


@end
