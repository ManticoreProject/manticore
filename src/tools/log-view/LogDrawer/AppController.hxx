/** \file  AppController.h
 * \author Korei Klein
 * \date 7/14/09
 *
 */

#import <Cocoa/Cocoa.h>
#import "DynamicRep.hxx"
#import "LogView.h"

@interface AppController : NSObject {

}

- (void)fillLogView:(LogView *)lv
	withLogFile:(LogFile *)lf
	fromTime:(uint64_t)s
	toTime:(uint64_t)f;

@end
