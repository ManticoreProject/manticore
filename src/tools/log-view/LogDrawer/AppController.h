/** \file  AppController.h
 * \author Korei Klein
 * \date 7/14/09
 *
 */

#import <Cocoa/Cocoa.h>
// #import "DynamicRep.hxx"
#import "LogView.h"
#import "LogFile.h"
// #import "log-desc.hxx"

struct Group;

@interface AppController : NSObject {
    IBOutlet LogView *logView;
}

- (NSColor *)groupColor:(struct Group *)g;

- (void)fillLogView:(LogView *)lv
	withLogFile:(LogFile *)lf
	fromTime:(uint64_t)s
	toTime:(uint64_t)f;

- (IBAction)test:(id)sender;

@end
