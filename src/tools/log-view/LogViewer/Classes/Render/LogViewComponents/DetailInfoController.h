/** \file  DetailInfoController.h
 * \author Korei Klein
 * \date 8/7/09
 *
 */

#import <Cocoa/Cocoa.h>
#import "EventInfoController.h"
#import "ShapeRep.h"
@class DetailInfoView;
struct LogFileDesc;


@interface DetailInfoController : NSViewController {
    EventInfoController *eventInfoControllerLeft;
    EventInfoController *eventInfoControllerRight;
    
    IBOutlet DetailInfoView *div;
    
    struct LogFileDesc *logDesc;
    
    IBOutlet NSView *leftTarget;
    IBOutlet NSView *rightTarget;

    NSString *name;
}

- (DetailInfoController *)initWithNibName:(NSString *)s
				   bundle:(NSBundle *)b
				  logDesc:(struct LogFileDesc *)logDescVal;

- (void)displayDetail:(EventShape *)s;
@property (readonly) DetailInfoView *div;
@property (readwrite, assign) NSString *name;


@end
