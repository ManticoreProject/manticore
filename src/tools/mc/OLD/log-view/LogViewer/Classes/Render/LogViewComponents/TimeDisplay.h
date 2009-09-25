/** \file  TimeDisplay.h
 * \author Korei Klein
 * \date 8/5/09
 *
 */

#import <Cocoa/Cocoa.h>
@class LogDoc;
@class LogView;

@interface TimeDisplay : NSView {
    IBOutlet LogDoc *logDoc;
    
    NSString *startString;
    NSString *endString;
    NSMutableArray *ticks;
    
    NSPoint startPoint;
    NSPoint endPoint;
    NSMutableDictionary *attributes;
    
    uint64_t rounding;
    
    BOOL enabled;
}

@property (readonly) NSArray *ticks;
@property (readwrite, assign) BOOL enabled;

- (IBAction)drewTicks:(LogView *)sender;


@end
