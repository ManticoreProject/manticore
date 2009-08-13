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
 */
@interface LogView : NSView {
    IBOutlet LogDoc *logDoc;
    IBOutlet CustomSplitView *splitView; ///< The background view, and the view that contains the BandViews
    IBOutlet MessageView *messageView; ///< The foreground view, and the view that displays dependent events
    IBOutlet NSScrollView *scrollView; ///< The scroll view containing this LogView
    
    NSMutableArray *bands;
    CGFloat band_height;
    
    CGFloat timeTick; ///< The number of 1/72 inches between two adjacent time ticks
    
    IBOutlet id target;
    
    BOOL enabled;
    
    NSMutableArray *ticks;
    
}

@property (readwrite, assign) NSMutableArray *bands;

@property (readonly) CGFloat band_height;
@property (readwrite, assign) BOOL enabled;
@property (readonly) NSArray *ticks;
@property (readonly) CustomSplitView *splitView;
@property (readwrite, assign) NSScrollView *scrollView;
@property (readwrite, assign) CGFloat timeTick;


- (void)displayInterval:(struct LogInterval *)logInterval
	    atZoomLevel:(enum ZoomLevel)zoomLevel
	    fromLogData:(LogData *)logData
	     filteredBy:(GroupFilter *)filter;

- (void)bigTickAt:(CGFloat)t;


@end
