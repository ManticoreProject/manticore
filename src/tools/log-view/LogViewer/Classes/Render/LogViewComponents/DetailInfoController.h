/** \file  DetailInfoController.h
 * \author Korei Klein
 * \date 8/7/09
 *
 */

#import <Cocoa/Cocoa.h>
#import "EventInfoController.h"
#import "ShapeRep.h"
@class DetailInfoView;
struct LogFileDesc;


/// Controller object for DetailInfoView
@interface DetailInfoController : NSViewController {
    /// Use this eic for displaying start, source and singleton events
    EventInfoController *eventInfoControllerLeft;
    /// Use this eic for displaying end and destination events
    EventInfoController *eventInfoControllerRight;

    /// The view object that self uses to display details
    IBOutlet DetailInfoView *div;

    struct LogFileDesc *logDesc;

    IBOutlet NSView *leftTarget; ///< Dummy view used in InterfaceBuilder
    IBOutlet NSView *rightTarget; ///< Dummy view used in InterfaceBuilder

    /// String to describe the event currently being displayed in the DetailInfoView
    NSString *name;
}

- (id)initWithNibName:(NSString *)s
	       bundle:(NSBundle *)b
	      logDesc:(struct LogFileDesc *)logDescVal;


/// s has an associated detail d.
/// d is a model object
/// display the model object d in div
- (void)displayDetail:(EventShape *)s;


@property (readonly) DetailInfoView *div;
@property (readwrite, assign) NSString *name;


@end
