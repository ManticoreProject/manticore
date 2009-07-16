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
}

@property (readwrite, assign) uint64_t logStart;
@property (readwrite, assign) uint64_t logEnd;

- (void)setLogFile:(LogFile *)logFileVal;

- (void)setStart:(uint64_t)startVal andEnd:(uint64_t)endVal;

@end
