/** \file  EventInfoController.mm
  *
  * \author Korei Klein
  * \date 8/7/09
  *
  *
  */

#import "EventInfoController.h"
#import "log-file.h"
#import "event-desc.hxx"
#import "log-desc.hxx"
#import "DetailAccess.h"
#import "Exceptions.h"

@implementation EventInfoController

#pragma mark Definitions

/// String to display if an event does not have a name
#define NO_NAME ( @"" )
/// String to display if an event does not have a description
#define NO_DESCRIPTION ( @"" )


// The following constants define the names of columns
// each column corresponds to an attribute of an argument.
// Each row corresponds to an argument.

/// The name used to identify the Name column
#define COLUMN_NAME_NAME ( @"Name" )
/// The name used to identify the Type column
#define COLUMN_NAME_TYPE ( @"Type" )
/// The name used to identify the Description column
#define COLUMN_NAME_DESCRIPTION ( @"Description" )
/// The name used to identify the Value column
#define COLUMN_NAME_VALUE ( @"Value" )



@synthesize time;

@synthesize name;
@synthesize args;
@synthesize description;


- (id)initWithNibName:(NSString *)n
	       bundle:(NSBundle *)b
	      logDesc:(LogFileDesc *)logDescVal
{

    if (![super initWithNibName:n bundle:b]) return nil;
    NSLog(@"EventInfoController %@ was just initialized with nib %@ and view %@", self, n, [self view]);

    // Strangely, this log message is necessary, without it problems start to arise

    self.name = NO_NAME;
    self.description = NO_DESCRIPTION;
    self.args = nil;
    value = nil;
    eventDesc = nil;

    logDesc = logDescVal;
    
    return self;
}

- (void)clear
{
    NSView *v = self.representedObject;
    [v setHidden:true];
}


- (event *)value
{
    return value;
}


- (void)setValueNotNull:(event *)e
{
    assert (e != NULL);

    value = e;
    // Set the view to display the event
    eventDesc = logDesc->FindEventById(Event_Id(*e));

    self.description = [NSString stringWithCString:eventDesc->Description()
					  encoding:NSASCIIStringEncoding];

    // Initialize args
    args = [[NSMutableArray alloc] init];
    for (int i = 0; i < eventDesc->NArgs(); ++i)
    {
//	NSLog(@"EventInfoController: Adding argument to array");
	// XXX Maybe get rid of this call and this Event_Value function
	// They perhaps should not be part of all possible interfaces to
	// events
	struct struct_log_event sle = Event_Value(*e);
	
	struct ArgDesc *argDesc = eventDesc->GetArgDesc(i);
	union ArgValue argValue = eventDesc->GetArg(&sle, i);
	EventArg *eventArg = [[EventArg alloc] initWithArgDesc:*argDesc
						   andArgValue:argValue];
	[args addObject:eventArg];
    }
    self.time = [NSString stringWithFormat:@"%qu", Event_Time(*e)];
}


- (void)setValue:(event *)e
{
    if (value == NULL)
    {
	// Initialize value and show the table and box
	NSTableView *tableView = [(EventInfoView *)[self view] table];
	[tableView setHidden:NO];
    }

    if (e == NULL)
    {
	value = e;
	eventDesc = nil;
	// Set the view to display a blank space
	self.name = NO_NAME;
	self.description = NO_DESCRIPTION;
	self.args = nil;
	self.time = @"";
	
	[[(EventInfoView *)[self view] table] setHidden:YES];
    }
    else // Value is already set, and e is a new value
    {
	[self setValueNotNull:e];
    }
    self.name = e ? [NSString stringWithCString:eventDesc->Description() encoding:NSASCIIStringEncoding] : NO_NAME ;
    [table reloadData];
 //   NSLog(@"table is %@", table);
    self.view.needsDisplay = YES;

}

#pragma mark Data Source Interface

- (NSInteger)numberOfRowsInTableView:(NSTableView *)t
{
    if ([self view] == NULL)
    {
//	NSLog(@"EventInfoController: asked for numberOfRowsInTableView before view was initialized");
	return 0; //< Is this okay, not okay???!?!?
    }
    if ([(EventInfoView *)[self view] table] == NULL)
    {
//	NSLog(@"EventInfoController: asked for number of rows in table when view was initialized but one of its components was not");
	return 0; //< ??!??!?
    }
    assert (t == [(EventInfoView *)[self view] table]);
    
    if (value == NULL) return 0;
    assert (value != nil);
    assert (eventDesc != nil);
    assert (args != nil);

   // NSLog(@"returning number of arguments in args for time %d", args.count);
    return args.count;

}

- (id)			tableView:(NSTableView *)t
	objectValueForTableColumn:(NSTableColumn *)c
	row:(NSInteger)i
{
    assert (t == [(EventInfoView *)[self view] table]);
    assert (value != nil);
    assert (eventDesc != nil);
    assert (args != nil);



    EventArg *arg = [args objectAtIndex:i];

    NSString *columnName = c.identifier;
    if ([columnName isEqualToString:COLUMN_NAME_NAME])
    {
	return arg.name;
    }
    else if ([columnName isEqualToString:COLUMN_NAME_TYPE])
    {
	return arg.type;
    }
    else if ([columnName isEqualToString:COLUMN_NAME_DESCRIPTION])
    {
	return arg.description;
    }
    else if ([columnName isEqualToString:COLUMN_NAME_VALUE])
    {
	return arg.value;
    }
    else
    {
	[Exceptions raise:@"EventInfoController: asked for a column which does not exist"];
	return arg.value;
    }

}

@end
