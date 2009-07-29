/** \file OutlineViewDataSource.mm
  * \author Korei Klein
  * \date 7/27/09
  *
  */

#import "OutlineViewDataSource.h"
#import "log-desc.hxx"
#import "Groups.h"
#import "Exceptions.h"


@implementation OutlineViewDataSource

@synthesize logFile;
@synthesize logView;
@synthesize root;

- (OutlineViewDataSource *)initWithLogDesc:(struct LogFileDesc *)desc
{
    Group *cppRoot = desc->Root(); //  [Exceptions raise:@"OutlineVieDataSource: how does one find the root?"];

    switch(cppRoot->Kind())
    {
	case EVENT_GROUP:
	    // All roots must be of kind EVENT_GROUP
	    root = [[ObjCEventGroup alloc] initWithCppGroup:cppRoot];
	    break;
	case STATE_GROUP:
	case INTERVAL_GROUP:
	case DEPENDENT_GROUP:
	default:
	    [Exceptions raise:@"OutlineViewDataSource: root has no known kind"];
    }
    return self;
}


- (id)outlineView:(NSOutlineView *)outlineView
	    child:(NSInteger)index
	   ofItem:(id)item
{
    // NSLog(@"OutlineViewDataSource: returning child of item");
    if (item == nil)
    {
	if (index != 0)
	{
	    [Exceptions
	       raise:@"OutlineVieDataSource: nil has one child, asked for a child of index != 0"];
	}
	else // index == 0
	{
	    NSLog(@"returning root");
	    return root;
	}
    }
    if (((ObjCGroup *)item).kind != EVENT_GROUP)
	[Exceptions raise:@"OutlineVieDataSource was asked for a child of a leaf node"];
    return [((ObjCEventGroup *)item).kids objectAtIndex:index];
}

- (BOOL)outlineView:(NSOutlineView *)outlineView
   isItemExpandable:(id)item
{
    if (item == nil)
    {
	[Exceptions raise:@"OutlineVieDataSource: asked if nil was expandable"];
    }
    if (((ObjCGroup *)item).kind == EVENT_GROUP)
    {
	return YES;
    }
    else
    {
	return NO;
    }
}

- (NSInteger)outlineView:(NSOutlineView *)outlineView
  numberOfChildrenOfItem:(id)item
{
    if (item == nil) return 1;
    if (((ObjCGroup *)item).kind != EVENT_GROUP)
	[Exceptions raise:@"OutlineVieDataSource was asked for a child of a leaf node"];
    return ((ObjCEventGroup *)item).kids.count;
}

	     -(id)outlineView:(NSOutlineView *)outlineView
    objectValueForTableColumn:(NSTableColumn *)tableColumn
		       byItem:(id)item
{
    if (item == nil)
	[Exceptions raise:@"nil has no valueForTableColumn"];
    NSNumber *ident = tableColumn.identifier;
    if (ident)
    {
	if (ident.intValue == 0)
	{
	    NSString *s = ((ObjCGroup *)item).desc;
	    // NSLog(@"OutlineViewDataSource: returning the description of an item: %@", s);
	    return s;
	}
	else if (ident.intValue == 1)
	{
	    return ((ObjCGroup *)item).enabled;
	}
	else
	{
	    [Exceptions raise:@"Table Column lacks a suitable identifier"];
	}
    }
    [Exceptions raise:@"Table Column's identifier was nil"];
    return [NSNumber numberWithInt:-1];
}

- (void)outlineView:(NSOutlineView *)outlineView
     setObjectValue:(id)object
     forTableColumn:(NSTableColumn *)tableColumn
	     byItem:(id)item
{
    if (item == nil)
	[Exceptions raise:@"OutlineView: passed a nil item to set"];
    NSNumber *ident = tableColumn.identifier;
    if (ident == nil)
	[Exceptions raise:@"Table column lacks an identifier"];
    if (ident.intValue != 1)
	[Exceptions raise:@"Can't edit a table column whose identifier is not 1"];


    ((ObjCGroup *)item).enabled = object;
    [logView readNewData:logFile];
}

@end



