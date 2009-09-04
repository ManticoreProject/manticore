/** \file  EventInfoView.h
 * \author Korei Klein
 * \date 8/10/09
 *
 */

#import <Cocoa/Cocoa.h>


/// View object to display information about an event
/// This class is implemented mostly in interface builder
@interface EventInfoView : NSView {
    IBOutlet NSTableView *table;
}

@property (readwrite, assign) NSTableView *table;

@end
