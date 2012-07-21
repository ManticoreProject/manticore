/** \file  OutlineViewDataSource.h
 * \author Korei Klein
 * \date 7/27/09
 *
 */

#import <Cocoa/Cocoa.h>
#import "GroupFilter.h"
@class LogDoc;
struct LogFileDesc;
struct Group;

/** OutlineViewDataSource implements the NSOutlineViewDataSource informal protocol.
 * It provides the information necessary to render the tree of cpp groups, along with
 * their enabled states.
 *
 * The enabled state of a group g is either 0, 1, or -1 if the group is
 * disabled, enabled, mixed
 */
@interface OutlineViewDataSource : GroupFilter < NSOutlineViewDataSource, NSOutlineViewDelegate > {
    struct LogFileDesc *desc;
    struct Group *root;

    /// This map maps each group g to an NSNumber * whose value is the enabled state
    /// which g is in
    NSMutableDictionary *map;

    // This array exists so that garbage collection does not screw up.
    // The NSOutlineView does not appear to be as compatible with GC as one
    // might like.  Problems occur in the data that OutlineViewDataSource passes
    // to NSOutlineView.  Therefore, any data which is getting GCed before its time
    // can now be safely boxed up and put in the boxes array to protect it from GC
    NSMutableArray *boxes;
}

/// Initialize
- (OutlineViewDataSource *)initWithLogDesc:(struct LogFileDesc *)descVal
				    logDoc:logDocVal;


#pragma mark Interface to NSOutlineView

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

- (void)outlineView:(NSOutlineView *)outlineView
    willDisplayCell:(id)cell
     forTableColumn:(NSTableColumn *)tableColumn
	       item:(id)item;


@end
