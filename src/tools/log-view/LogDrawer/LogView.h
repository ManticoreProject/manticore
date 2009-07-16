/** \file  LogView.h
 * \author Korei Klein
 * \date 7/7/09
 *
 */

#import <Cocoa/Cocoa.h>
#import "CustomSplitView.h"
#import "MessageView.h"
#import "BandView.h"


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
    CGFloat start;
    CGFloat end;
}

- (void)acquireBands:(NSMutableArray *)bands;

@property (readwrite, assign) CGFloat start;
@property (readwrite, assign) CGFloat end;

@end
