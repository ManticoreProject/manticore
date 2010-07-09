/** \file  DetailInfoController.h
 * \author Korei Klein
 * \date 8/7/09
 *
 */

#import "EventInfoController.h"
#import "ShapeRep.h"
@class DetailInfoView;
struct LogFileDesc;


/// Controller object for DetailInfoView
@interface DetailInfoController : NSWindowController {
    /// Use this eic for displaying start, source and singleton events
    EventInfoController *eventInfoControllerLeft;
    /// Use this eic for displaying end and destination events
    EventInfoController *eventInfoControllerRight;
    
    /* These views are instantiated in Interface Builder, and are placeholders
     * for the EventInfoViews we will be putting in their place. It is necessary
     * to do this juggling only because EventInfoViews have their own nibs and
     * NSViewControllers. */
    IBOutlet NSView *viewTargetLeft;
    IBOutlet NSView *viewTargetRight;

    struct LogFileDesc *logDesc;

    /// String to describe the event currently being displayed in the DetailInfoView
    NSString *name;
}


- (id)initWithLogDesc:(struct LogFileDesc *) logDescVal;

/// s has an associated detail d.
/// d is a model object
/// display the model object d in div
- (void)displayDetail:(EventShape *)s;


@property (readwrite, assign) NSString *name;


@end
