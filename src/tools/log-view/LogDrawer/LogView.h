/** \file  LogView.h
 * \author Korei Klein
 * \date 7/7/09
 *
 */

#import <Cocoa/Cocoa.h>
#import "CustomSplitView.h"
#import "MessageView.h"
#import "BandView.h"


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
    uint64_t logX;
    uint64_t logWidth;
    
    NSRulerView *ruler;
    
    CGFloat timeTick; ///< The number of pixel between two adjacent time ticks
    
    int cur_state;
    struct StateGroup *stateGroup; //< The group of state events to display
    
    enum ZoomLevel zoomLevel;
}

@property (readwrite, assign) enum ZoomLevel zoomLevel;
@property (readwrite, assign) uint64_t logX;
@property (readwrite, assign) uint64_t logWidth;
@property (readwrite, assign) CGFloat timeTick;
@property (readwrite, assign) NSRulerView *ruler;
@property (readonly) uint64_t pivot;
@property (readonly) uint64_t scale;

- (IBAction)zoomIn:(id)sender;
- (IBAction)zoomOut:(id)sender;


- (void)readNewData:(void *)lf;

/// Calculate the image in the bounds of a point in the logFile
- (CGFloat)image:(uint64_t)p;
/// Calculate the preimage in the logFile of a point in the bounds
- (uint64_t)preImage:(CGFloat)p;

/// Set the endpoints of the interval of the logFile being shown
- (void)setStart:(uint64_t)logXVal andWidth:(uint64_t)logWidthVal;

/// Adjust the size of the logFile interval using a pivot and a new width
/** Linearly change logX and logWidth such that pivot is in the same place,
  * but logWidth = size.
  * invariant: pivot <= logWidth >= 0
  */
- (void)resizeIntervalToSize:(uint64_t)size aboutPivot:(uint64_t)pivot;

@end
