/** \file  OutlineViewDataSource.h
 * \author Korei Klein
 * \date 7/27/09
 *
 */

#import <Cocoa/Cocoa.h>
#import "GroupFilter.h"
@class LogView;
@class LogFile;
struct LogFileDesc;
struct Group;

/** OutlineViewDataSource implements the NSOutlineViewDataSource informal protocol.
 * It displays an outline for Groups.
 */
@interface OutlineViewDataSource : GroupFilter {
    IBOutlet LogFile *logFile;
    IBOutlet LogView *logView;
    struct LogFileDesc *desc;
    struct Group *root;
    NSMutableDictionary *map;
}

@property (readwrite, assign) LogFile *logFile;
@property (readwrite, assign) LogView *logView;

- (OutlineViewDataSource *)initWithLogDesc:(struct LogFileDesc *)descVal;

- (id)outlineView:(NSOutlineView *)outlineView
	    child:(NSInteger)index
	   ofItem:(id)item;

- (BOOL)outlineView:(NSOutlineView *)outlineView
   isItemExpandable:(id)item;

- (NSInteger)outlineView:(NSOutlineView *)outlineView
  numberOfChildrenOfItem:(id)item;

- (id)		  outlineView:(NSOutlineView *)outlineView
    objectValueForTableColumn:(NSTableColumn *)tableColumn
		       byItem:(id)item;


- (void)outlineView:(NSOutlineView *)outlineView
     setObjectValue:(id)object
     forTableColumn:(NSTableColumn *)tableColumn
	     byItem:(id)item;


@end
