/** \file  LogDoc.h
 * \author Korei Klein
 * \date 7/30/09
 *
 */

#import <Cocoa/Cocoa.h>
struct TaggedDetail_struct;
//#import "LogView.h"
//#import "LogData.h"
@class LogView;
@class LogData;
@class OutlineViewDataSource;
@class TimeDisplay;
@class DetailInfoController;
@class EventShape;
@class GroupFilter;
@class Summary;
@class SummaryView;
struct LogFileDesc;

/// Represents an interval of time in the log file
struct LogInterval {
    uint64_t x;
    uint64_t width;
};
/// Determines what kind of rendering technique will be used to render LogData
enum ZoomLevel {
    zoomLevelDeep,
    zoomLevelMedium,
    zoomLevelShallow
};

/** LogDoc, the NSDocument controller class which manages the window which displays a log file.
 * LogDoc manages the logic involved in coordinating a LogView view object (along with some auxilliary views)
 * with a LogData model object (along with some auxilliary model objects).
 * LogDoc manages all interaction between a user and a log file, relying on the methods of LogData and LogView when necessary.
 */
@interface LogDoc : NSDocument {
    IBOutlet NSWindow *window; ///< The window object containing the LogView

    struct LogFileDesc *logDesc;

    IBOutlet LogView *logView; ///< The most important view object
    IBOutlet LogData *logData; ///< The most important model object
    IBOutlet NSScrollView *scrollView; ///< The view object in which logView is embedded

    IBOutlet NSOutlineView *outlineView; ///< The view which allows the user to filter events.
    IBOutlet OutlineViewDataSource *outlineViewDataSource; ///< The model object for outlineView

    DetailInfoController *detailInfoController; ///< A helper controller object to manage a DetailInfoView
    IBOutlet NSView *detailInfoTarget; ///< A dummy view to act as a placeholder in InterfaceBuilder

    IBOutlet NSDrawer *drawer; ///< The drawer in which to place outlineView

    /// The time interval of the log file which will be displayed
    struct LogInterval *logInterval;

    /// Property to be manipulated in InterfaceBuilder
    /// bound to the height of the horizontal scrollbar of scrollView
    float horizontalPosition;
    IBOutlet TimeDisplay *timeDisplay; ///< View to print times on top of logView

    BOOL enabled; ///< Whether or not self is ready to display data, used in initializations

    double zoomFactor; ///< the current technique to be used for rendering

    IBOutlet NSPanel *infoPanel; ///< Panel to contain the DetailInfoView

    Summary *summary; ///< Model object to hold summary data about the log file
    SummaryView *summaryView; ///< View object to draw data held in summary
    IBOutlet NSView *summaryViewTarget; ///< A dummy view to act as a placeholder in InterfaceBuilder
}


- (struct LogFileDesc *)logDesc;

@property (readonly) TimeDisplay *timeDisplay;
@property (readwrite, assign) float horizontalPosition;
@property (readonly) NSString *filename;
@property (readwrite) double zoomFactor;
@property (readonly) LogView *logView;
@property (readonly) LogData *logData;
@property (readonly) NSOutlineView *outlineView;
@property (readonly) OutlineViewDataSource *outlineViewDataSource;
@property (readwrite, assign) struct LogInterval *logInterval;
@property (readonly) struct LogFileDesc *logDesc;
@property (readonly) BOOL enabled;
@property (readonly) GroupFilter *filter;

/// Cause logView to render the data in LogData
- (void)flush;

/// returns true iff d should be drawn based on the current logInterval
- (BOOL)isInInterval:(struct TaggedDetail_struct *)d;

/// Determines what rendering technique is appropriate for the given logInterval
- (enum ZoomLevel)zoomLevelForInterval:(struct LogInterval *)logInterval;

/// Zoom based on which half of sender was clicked
- (IBAction)zoom:(NSSegmentedControl *)sender;
/// Enlarge the size of details (shrink the logInterval)
- (IBAction)zoomIn:(id)sender;
/// Shrink the size of details (enlarge the logInterval)
- (IBAction)zoomOut:(id)sender;
/// zoom in such that that details at time pivot are drawn at the same
/// coordinates before and after zooming
- (void)zoomInAboutPivot:(uint64_t)pivot;
/// zoom out such that that details at time pivot are drawn at the same
/// coordinates before and after zooming
- (void)zoomOutAboutPivot:(uint64_t)pivot;

- (IBAction)drewTicks:(LogView *)sender;

// Given the bounds of the logView and the logInterval
// there exists a unique non-singular affine map T
// such that T(logInterval) = the bounds of logView

/// T(p)
- (CGFloat)image:(uint64_t)p;
/// k such that T(k) = p
- (uint64_t)preImage:(CGFloat)p;

/// Have the DetailInfoView display d
- (void)displayDetail:(EventShape *)d;

@end
