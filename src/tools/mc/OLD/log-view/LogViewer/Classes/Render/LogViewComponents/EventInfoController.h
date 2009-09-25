/** \file  EventDetailController.h
 * \author Korei Klein
 * \date 8/7/09
 *
 */

#import <Cocoa/Cocoa.h>
#import "EventArg.h"
#import "Detail.h"
#import "EventInfoView.h"
struct LogFileDesc;
struct EventDesc;


@interface EventInfoController : NSViewController {
    IBOutlet EventInfoView *eiv;

    struct LogFileDesc *logDesc;

    IBOutlet NSTableView *table;
    
    event *value;
    struct EventDesc *eventDesc;

    NSString *name;
    NSMutableArray *args; //< This array contains EventArg
    NSString *description;
    
    NSString *time;
}


- (EventInfoController *)initWithNibName:(NSString *)n
				    bundle:(NSBundle *)b
				   logDesc:(struct LogFileDesc *)logDescVal;


@property (readonly) EventInfoView *eiv;

@property (readwrite, assign) NSString *time;


@property (readwrite, assign) event *value;
@property (readwrite, assign) NSString *name;
@property (readwrite, assign) NSString *description;
@property (readwrite, assign) NSArray *args;

@end
