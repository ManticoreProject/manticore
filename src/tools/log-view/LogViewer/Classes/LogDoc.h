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
struct LogDescFile;

struct LogInterval {
    uint64_t x;
    uint64_t width;
};
enum ZoomLevel {
    zoomLevelDeep,
    zoomLevelMedium,
    zoomLevelShallow
};


@interface LogDoc : NSDocument {  
    IBOutlet NSWindow *window;
    
    IBOutlet LogView *logView;
    IBOutlet LogData *logData;
    IBOutlet NSScrollView *scrollView;
    
    IBOutlet NSOutlineView *outlineView;
    IBOutlet OutlineViewDataSource *outlineViewDataSource;

    DetailInfoController *detailInfoController;
    IBOutlet NSView *detailInfoTarget;
    
    IBOutlet NSDrawer *drawer;
    
    // The time interval of the log file which will be displayed

    struct LogInterval *logInterval;
    
    float horizontalPosition;
    IBOutlet TimeDisplay *timeDisplay;
    
    BOOL enabled;
    
    double zoomFactor;
    
    IBOutlet NSPanel *infoPanel;
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



- (void)flush;

- (BOOL)isInInterval:(struct TaggedDetail_struct *)d;

- (enum ZoomLevel)zoomLevelForInterval:(struct LogInterval *)logInterval;
- (IBAction)zoom:(NSSegmentedControl *)sender;
- (IBAction)zoomIn:(id)sender;
- (IBAction)zoomOut:(id)sender;
- (void)zoomInAboutPivot:(uint64_t)pivot;
- (void)zoomOutAboutPivot:(uint64_t)pivot;

- (IBAction)drewTicks:(LogView *)sender;

- (CGFloat)image:(uint64_t)p;
- (uint64_t)preImage:(CGFloat)p;

- (void)displayDetail:(EventShape *)d;

@end
