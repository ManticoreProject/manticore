/** \file  DetailInfoController.m
 * \author Korei Klein
 * \date 8/7/09
 *
 */

#import "DetailInfoController.h"
#import "Detail.h"
#import "DetailAccess.h"
#import "Exceptions.h"
#import "ShapeRep.h"
#import "DetailInfoView.h"
#import "log-desc.hxx"
#import "event-desc.hxx"


@implementation DetailInfoController

@synthesize div;
@synthesize name;

#pragma mark Definitions

/// Name to put on an EventInfoController which is displaying a simple detail
#define EIV_NAME_SIMPLE ( @"Event" )

/// Name to put on an EventInfoController which is displaying a state start detail
#define  LEFT_EIV_NAME_STATE ( @"State Start" )
/// Name to put on an EventInfoController which is displaying a state end detail
#define RIGHT_EIV_NAME_STATE ( @"State End" )

/// Name to put on an EventInfoController which is displaying an interval start detail
#define  LEFT_EIV_NAME_INTERVAL ( @"Interval Start" )
/// Name to put on an EventInfoController which is displaying an interval end detail
#define RIGHT_EIV_NAME_INTERVAL ( @"Interval End" )

/// Name to put on an EventInfoController which is displaying a dependent source detail
#define  LEFT_EIV_NAME_DEPENDENT ( @"Message Source" )
/// Name to put on an EventInfoController which is displaying a dependent destination detail
#define RIGHT_EIV_NAME_DEPENDENT ( @"Message Destination" )


/// Name of the nib which contains the EventInfoView and EventInfoController
#define INFO_CONTROLLER_NIB_NAME ( @"EventInfo" )


#pragma mark Initialization

- (id)initWithNibName:(NSString *)s
	       bundle:(NSBundle *)b
	      logDesc:(struct LogFileDesc *)logDescVal
{
    if (![super initWithNibName:s bundle:b]) return nil;

    self.name = @"";
    logDesc = logDescVal;

    eventInfoControllerLeft = [[EventInfoController alloc] initWithNibName:INFO_CONTROLLER_NIB_NAME
								    bundle:nil
								   logDesc:logDescVal];
    eventInfoControllerRight = [[EventInfoController alloc] initWithNibName:INFO_CONTROLLER_NIB_NAME
								     bundle:nil
								    logDesc:logDescVal];

    EventInfoView *lview = (EventInfoView *) (eventInfoControllerLeft.eiv);
    EventInfoView *rview = (EventInfoView *) (eventInfoControllerRight.eiv);


    self.view.needsDisplay = true;
   // NSLog(@"DetailInfoController: initializing! leftTarget = %@ rightTarget = %@ view = %@",
	    // leftTarget, rightTarget, self.view);

    // Use the dummy target views to determine where to put the newly loaded eventInfoViews
    // see LogDoc.mm:windowControllerDidLoadNib: for a more detailed explanation

    [lview setFrame:leftTarget.frame];
    [rview setFrame:rightTarget.frame];

  //  assert (splitView != NULL);
    [div replaceSubview:leftTarget with:lview];
    [div replaceSubview:rightTarget with:rview];

  //  NSLog(@"DetailInfoController: left eiv = %@ and table = %@ right eiv = %@ and table = %@",
//	  lview, lview.table, rview, rview.table);



    return self;
}

#pragma mark Drawing

- (void)clear:(EventInfoController *)eic
{
    eic.value = nil;
}

- (void)clearAll
{
    [self clear:eventInfoControllerLeft];
    [self clear:eventInfoControllerRight];
}

- (NSString *)nameFromEvent:(event *)e
{
    NSString *ret = [NSString stringWithCString:
		     (logDesc->FindEventById(Event_Id(*e)))->Description()
				       encoding:NSASCIIStringEncoding];
    return ret;
}

#pragma mark IFace

- (void)displayDetail:(EventShape *)s
{
    if (s == NULL)
    {
	//NSLog(@"DetailInfoController: clearing the view");
	[self clearAll];
    }
    Singleton *single;
    State *state;
    Interval *interval;
    Message *message;

    self.name = s.description;

    switch (s.kind)
    {
	case SIMPLE_SHAPE:
	    single = (Singleton *)s;

	    [self clear:eventInfoControllerRight];

	    //eventInfoControllerLeft.name = EIV_NAME_SIMPLE;
	    eventInfoControllerLeft.value = single.eventVal;

	    break;
	case STATE_SHAPE:
	    state = (State *)s;

	    //eventInfoControllerLeft.name = LEFT_EIV_NAME_STATE;
	    eventInfoControllerLeft.value = state.start;

	    eventInfoControllerRight.name = RIGHT_EIV_NAME_STATE;
	    eventInfoControllerRight.value = state.end;

	    break;
	case INTERVAL_SHAPE:
	    interval = (Interval *)s;

	    //eventInfoControllerLeft.name = LEFT_EIV_NAME_INTERVAL;
	    eventInfoControllerLeft.value = interval.start;

	   // eventInfoControllerRight.name = RIGHT_EIV_NAME_INTERVAL;
	    eventInfoControllerRight.value = interval.end;

	    break;
	case MESSAGE_SHAPE:
	    message = (Message *)s;

	   // eventInfoControllerLeft.name = LEFT_EIV_NAME_DEPENDENT;
	    eventInfoControllerLeft.value = message.sender;

	   // eventInfoControllerRight.name = RIGHT_EIV_NAME_DEPENDENT;
	    eventInfoControllerRight.value = message.receiver;

	    break;
	default:
	    [Exceptions raise:@"detail has group of unknown kind"];
    }
}


@end



