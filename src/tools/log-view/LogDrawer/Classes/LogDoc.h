/** \file  LogDoc.h
 * \author Korei Klein
 * \date 7/30/09
 *
 */

#import <Cocoa/Cocoa.h>
#import "LogView.h"
#import "LogData.h"
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
    IBOutlet LogView *logView;
    IBOutlet LogData *logData;
    IBOutlet NSOutlineView *outlineView;
    IBOutlet OutlineViewDataSource *outlineViewDataSource;

    // The time interval of the log file which will be displayed
    struct LogInterval *logInterval;

    static LogDescFile *logDesc;
    
    BOOL enabled;
    
    double zoomFactor;
    enum ZoomLevel zoomLevel;
}

+ (LogDescFile *)logDesc;

@property (readwrite) double zoomFactor;
@property (readwrite) enum ZoomLevel zoomLevel;
@property (readonly) LogView *logView;
@property (readonly) LogData *logData;
@property (readonly) NSOutlineView *outlineView;
@property (readonly) OutlineViewDataSource *outlineViewDataSource;
@property (readwrite, assign) struct LogInterval *logInterval;
@property (readonly) struct LogDescFile *logDesc;
@property (readonly) BOOL enabled;

- (enum ZoomLevel)zoomLevelForInterval:(struct LogInterval *)logInterval;
- (IBAction)zoomIn:(id)sender;
- (IBAction)zoomOut:(id)sender;

- (CGFloat)image:(uint64_t)p;
- (uint64_t)preImage:(CGFloat)p;

@end
