/** \file  LogView.h
 * \author Korei Klein
 * \date 7/7/09
 *
 */

#import <Cocoa/Cocoa.h>
#import "CustomSplitView.h"
#import "MessageView.h"
#import "BandView.h"
#import "LogDoc.h"

@class GroupFilter;


/// Display a log file
/**
 Draws a picture of a log file.
 LogView displays each vProc on a seperate BandView.
 Each band displays its relevant events.
 LogView also displays messages as arrows going between BandViews.

 A good way to get an idea of what a LogView does is to run LogViewer,
 open a log file, and look at the LogView.
 The logView will sit inside a scrollView
 There should be a few bands (BandViews) drawn inside the logView.
    These BandViews are the subviews of the splitView.
 There will be some tick marks drawn about every inch or so,
    these horizontal lines appear in the spaces between the bands.
 On top of some of the lines will be the times which they correspond to.
 At some places in the view there might be arrows going from one band to the next:
    these arrows are drawn by the messageView and represent dependent details.
 */
@interface LogView : NSView {
    IBOutlet LogDoc *logDoc;
    IBOutlet CustomSplitView *splitView; ///< The background view, and the view that contains the BandViews
    IBOutlet MessageView *messageView; ///< The foreground view, and the view that displays dependent events
    IBOutlet NSScrollView *scrollView; ///< The scroll view containing this LogView
    IBOutlet NSTextField *timeUnderMouse; ///< The text field displaying the time under the mouse
    
    NSTrackingArea *trackingArea; ///< Tracking area for picking up MouseMoved events'
    NSPoint mouseLoc; ///< Where was the mouse last time we got a mouseMoved?

    NSMutableArray *bands;
    CGFloat band_height;

    CGFloat timeTick; ///< The number of 1/72 inches between two adjacent tick marks

    IBOutlet id target;

    BOOL enabled;

    NSMutableArray *ticks; ///< Array of NSNumbers of times at which to draw tick marks

}

@property (readwrite, assign) NSMutableArray *bands;

@property (readonly) CGFloat band_height;
@property (readwrite, assign) BOOL enabled;
@property (readonly) NSArray *ticks;
@property (readonly) CustomSplitView *splitView;
@property (readwrite, assign) NSScrollView *scrollView;
@property (readonly) MessageView *messageView;
@property (readwrite, assign) CGFloat timeTick;
@property (readonly) NSPoint mouseLoc;

/// Main shape creation function
/**
 this method does not render anything.
 instead, it reads data from LogData and creates the shapes
 necessary to render that data.
 Once this method is called, all subsequent calls to drawRect
 will cause LogView to render the data that this method read.

 The data to be rendered naturally depends on these things

 \param logInterval the portion of LogData which is to be displayed
 \param zoomLevel the level of zoom to use.  Currently only one level is implemented. For each zoom level, rendering is done entirely differently.
 \param logData the logData object from which the data is to be read
 \param filter a GroupFilter object.  This abstract class will determine which of the details in the interval should be displayed based on its enabled: method
 */
- (void)displayInterval:(struct LogInterval *)logInterval
	    atZoomLevel:(enum ZoomLevel)zoomLevel
	    fromLogData:(LogData *)logData
	     filteredBy:(GroupFilter *)filter;

- (void)bigTickAt:(CGFloat)t;


@end
