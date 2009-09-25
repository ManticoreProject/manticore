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

#define EIV_NAME_SIMPLE ( @"Event" )

#define  LEFT_EIV_NAME_STATE ( @"State Start" )
#define RIGHT_EIV_NAME_STATE ( @"State End" )

#define  LEFT_EIV_NAME_INTERVAL ( @"Interval Start" )
#define RIGHT_EIV_NAME_INTERVAL ( @"Interval End" )

#define  LEFT_EIV_NAME_DEPENDENT ( @"Message Source" )
#define RIGHT_EIV_NAME_DEPENDENT ( @"Message Destination" )


#define INFO_CONTROLLER_NIB_NAME ( @"EventInfo" )


#pragma mark Initialization

- (DetailInfoController *)initWithNibName:(NSString *)s
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


    [lview setFrame:leftTarget.frame];
    [rview setFrame:rightTarget.frame];
    
  //  assert (splitView != NULL);
    [div replaceSubview:leftTarget with:lview];
    [div replaceSubview:rightTarget with:rview];

    NSLog(@"DetailInfoController: left eiv = %@ and table = %@ right eiv = %@ and table = %@",
	  lview, lview.table, rview, rview.table);
    


    return self;
}

#pragma mark Accessors


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
