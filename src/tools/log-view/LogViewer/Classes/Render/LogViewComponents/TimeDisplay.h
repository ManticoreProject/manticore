/** \file  TimeDisplay.h
 * \author Korei Klein
 * \date 8/5/09
 *
 */

#import <Cocoa/Cocoa.h>
@class LogDoc;
@class LogView;

/// View Object to display times above tick marks
@interface TimeDisplay : NSView {
    IBOutlet LogDoc *logDoc;

    NSString *startString; ///< The leftmost time to display
    NSString *endString;   ///< The rightmost time to display
    NSMutableArray *ticks; ///< The times to display which aren't {left,right}most

    NSPoint startPoint; ///< The point at which to display startString
    NSPoint endPoint;   ///< The point at which to display endString
    NSMutableDictionary *attributes;  ///< The attributes of strings

    /// Time should be rounded to the nearest rounding before being displayed
    uint64_t rounding;

    BOOL enabled;
}

@property (readonly) NSArray *ticks;
@property (readwrite, assign) BOOL enabled;

/// Call this method to indicate that LogView has just drawn its ticks,
/// and would like times drawn above them. TimeDisplay will then draw those times
- (IBAction)drewTicks:(LogView *)sender;


@end
