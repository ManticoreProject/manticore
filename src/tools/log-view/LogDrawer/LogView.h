/** \file  LogView.h
 * \author Korei Klein
 * \date 7/7/09
 *
 */

#import <Cocoa/Cocoa.h>
#import "CustomSplitView.h"
#import "MessageView.h"
#import "BandView.h"
#import "LogFile.h"

struct Group;
struct StateGroup;

enum ZoomLevel {
    zoomLevelDeep,
    zoomLevelMedium,
    zoomLevelShallow
};

/// Display a log file
/**
 Draws a picture of a log file.
 LogView displays each vProc on a seperate BandView.
 Each band displays its relevant events.
 LogView also displays messages as arrows going between BandViews.
 */
@interface LogView : NSView {
    IBOutlet CustomSplitView *splitView; ///< The background view, and the view that contains the BandViews
    IBOutlet MessageView *messageView; ///< The foreground view, and the view that displays dependent events
    uint64_t logStart;
    uint64_t logEnd;
    LogFile *logFile;
    
    
    int cur_state;
    struct StateGroup *stateGroup; //< The group of state events to display
    
    enum ZoomLevel zoomLevel;
}

@property (readwrite, assign) enum ZoomLevel zoomLevel;
@property (readwrite, assign) uint64_t logStart;
@property (readwrite, assign) uint64_t logEnd;

- (void)setLogFile:(LogFile *)logFileVal;

/// Calculate the image in the bounds of a point in the logFile
- (CGFloat)image:(uint64_t)p;
/// Calculate the preimage in the logFile of a point in the bounds
- (uint64_t)preImage:(CGFloat)p;

/// Set the endpoints of the interval of the logFile being shown
- (void)setStart:(uint64_t)startVal andEnd:(uint64_t)endVal;

/// Adjust the size of the logFile interval using a pivot and a new width
/** Linearly change logStart and logEnd such that pivot is in the same place,
  * but logEnd - logStart = size.
  * invariant: logStart <= pivot <= logEnd
  */
- (void)resizeIntervalToSize:(uint64_t)size aboutPivot:(uint64_t)pivot;

@end
