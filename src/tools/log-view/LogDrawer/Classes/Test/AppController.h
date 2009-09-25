/** \file  AppController.h
 * \author Korei Klein
 * \date 7/14/09
 *
 */

#import <Cocoa/Cocoa.h>
// #import "DynamicRep.hxx"
#import "LogView.h"
// #import "log-desc.hxx"

struct Group;

@interface AppController : NSObject {
    IBOutlet LogView *logView;
    IBOutlet NSScrollView *scrollView;
}

- (NSColor *)groupColor:(struct Group *)g;

- (IBAction)test:(id)sender;

- (IBAction)open:(id)sender;

@end
