/** \file  VProc.mm
 * \author Korei Klein
 * \date 7/10/09
 *
 */

#import "VProc.h"
#import "log-desc.hxx"
#import "event-desc.hxx"
#import "DetailAccess.h"
#import "Exceptions.h"
#import "Detail.h"
#import "VProcMaps.h"
#import "Box.h"

/// The number of details for each event on average
#define ESTIMATED_DETAILS_PER_EVENT ( 0.4 )

/// How much should the array grow by each time it runs out of space
#define APPAY_INC ( 100 )

static inline int min( int a, int b)
{
    return a < b ? a : b;
}

@implementation VProc


#pragma mark Synthesis

@synthesize events;
@synthesize numEvents;
@synthesize vpId;
@synthesize details;

#pragma mark Initialization

/// Initialize
- (VProc *)initWithLog:(LogBuffer_t *)logBuffer
	    andLogDesc:(LogFileDesc *)logDescVal
		header:(LogFileHeader_t *)headerVal
	     numEvents:(int)n
	     allStates:(NSArray *)allStates
{
    if (![super init]) return nil;

    header = headerVal;
    vpId = logBuffer->vpId;
    logDesc = logDescVal;

    stateMap = [[StateMap alloc] init];
    intervalMap = [[IntervalMap alloc] init];
    dependentMap = [[DependentMap alloc] init];
    
    numEvents = 0;
    events_size = min(n, logBuffer->next);
    events = (event *)malloc(sizeof(event) * events_size);
    
    // The array of details and the initial states are related
    // They must be initialized in tandem
#pragma mark Detail and State Initialization
    
    numDetails = 0;
    details_size = ESTIMATED_DETAILS_PER_EVENT * events_size + allStates.count;
    details = (Detail *) malloc(sizeof(Detail) * details_size);

    int i = 0;
    for (Box *stateGroupBox in allStates)
    {
	StateGroup *stateGroup = (StateGroup *) [stateGroupBox unbox];
	details[i]->type = stateGroup;
	details[i]->data.state.state = stateGroup->StartState();
	details[i]->data.state.start = NULL; //< The detail starts at the begginig of time
	details[i]->data.state.end = NULL;   //< The detail ends at the end of time
	
	[stateMap addDetail:&details[i]->data.state forStateGroup:stateGroup];
	
	++i;
    }
    
    
    [self read:events_size eventsFrom:logBuffer->log];
    
    assert( numEvents == events_size );
    
    return self;
}


- (void)readBlock:(LogBuffer_t *)logBuffer
	numEvents:(int)n
{
    assert( numEvents == events_size );
    
    int eventsToRead = min(n, logBuffer->next);
    
    events = (event *) realloc(events, sizeof(event) * (events_size = events_size + eventsToRead));
    
    [self read:eventsToRead eventsFrom:logBuffer->log];
}

/// Find the identifier for this dependent event
/** This function makes a very serious assumption:
 * 1. Every dependent event's identifier is the first of its arguments whose
 type is EVENT_ID, and such an argument exists

 If this assumption is not met, the function will raise an exception.
*/
uint64_t GetDependentId(event *e, EventDesc *eventDesc)
{
    for (int i = 0; i < eventDesc->NArgs(); ++i)
    {
	if (eventDesc->GetArgType(i) == EVENT_ID)
	{
	    ArgValue argValue = eventDesc->GetArg(&e->value, i);
	    return argValue.id;
	}
    }
    [Exceptions raise:
	@"VProc.mm: GetDependentId: arguments did not meet a necessary assumption"];
	return -1;
}
/* convert a timestamp to nanoseconds */
static inline uint64_t GetTimestamp (LogTS_t *ts, LogFileHeader_t *Hdr)
{
    if (Hdr->tsKind == LOGTS_MACH_ABSOLUTE)
	return ts->ts_mach;
    else if (Hdr->tsKind == LOGTS_TIMESPEC)
	return ts->ts_val.sec * 1000000000 + ts->ts_val.frac;
    else /* Hdr->tsKind == LOGTS_TIMEVAL */
	return ts->ts_val.sec * 1000000000 + ts->ts_val.frac * 1000;
}


- (void)MakeDetailBigEnoughToHoldNewEvent:(struct Dependent_Detail *)d
{
    if (d->n_dsts == 0)
    {
        // Create the array
        d->dsts = (event **) malloc
        (d->dsts_array_size = DSTS_ARRAY_INC);
    }
    else if (d->n_dsts == d->dsts_array_size)
    {
        // Grow the array
        d->dsts = (event **) realloc
        (d->dsts,
         sizeof(event) * (d->dsts_array_size = d->dsts_array_size + DSTS_ARRAY_INC));
    }
}
    
    
/// Main event reading logic
/// Assume: events is big enough to hold all the events
/// Assume: details may not be big enough to hold all the details
- (void)read:(int)n eventsFrom:(LogEvent_t *)array
{
    assert ( numEvents + n == events_size );
    for (int i = 0; i < n; ++i, ++numEvents)
    {
	// Add the event
	event *cur_event = &events[numEvents];

	cur_event->timestamp = GetTimestamp(&array[i].timestamp, header);
	memcpy(&(cur_event->value), &array[i], sizeof(LogEvent_t));

	
	// Add the associated details
	EventDesc *eventDesc = logDesc->FindEventById(array[i].event);

	std::vector<StateGroup *> *stateGroups = logDesc->StateGroups(eventDesc);
    	std::vector<IntervalGroup *> *intervalGroups = logDesc->IntervalGroups(eventDesc);
    	std::vector<DependentGroup *> *dependentGroups = logDesc->DependentGroups(eventDesc);

    	// No more details will be added than the sum of the lengths of these vectors
	int n_more_details = stateGroups->size() +
			     intervalGroups->size() +
			     dependentGroups->size();
	if (numDetails + n_more_details > details_size)
	{
	    details = (Detail *) realloc(
		details,
		(details_size = details_size + n_more_details + DSTS_ARRAY_INC) * sizeof(Detail)
		);
	}

	
#pragma mark Simples
	    ///////////////// SIMPLES     ////////////////////

#pragma mark States
	    ///////////////// STATES      ////////////////////
	    for (int i = 0; i < stateGroups->size(); ++i)
	    {
		StateGroup *stateGroup = stateGroups->at(i);
		struct State_Detail *mostRecentStateDetail =
		    [stateMap getDetailForStateGroup:stateGroup];
		if (mostRecentStateDetail == NULL)
		    [Exceptions raise:@"States were not properly initialized"];

		details[numDetails]->type = stateGroup;
		struct State_Detail *next_stateDetail = &(details[numDetails++]->data.state);

		next_stateDetail->state = stateGroup->NextState
		    (mostRecentStateDetail->state, eventDesc);

		mostRecentStateDetail->end = cur_event;
		next_stateDetail->start = cur_event;
		next_stateDetail->end = NULL;
	    }

#pragma mark Intervals
	    ///////////////// INTERVALS   ////////////////////
	    for (int i = 0; i < intervalGroups->size(); ++i)
	    {
		IntervalGroup *intervalGroup = intervalGroups->at(i);
		if (eventDesc == intervalGroup->Start())
		{
		    details[numDetails]->type = intervalGroup;
		    struct Interval_Detail *next_intervalDetail =
			&(details[numDetails++]->data.interval);
		    next_intervalDetail->start = cur_event;
		    next_intervalDetail->end = NULL;

		    [intervalMap addDetail:next_intervalDetail
			  forIntervalGroup:intervalGroup];
		}
		else
		{
		    assert (eventDesc == intervalGroup->End());
		    struct Interval_Detail *mostRecentIntervalDetail =
			[intervalMap getDetailForIntervalGroup:intervalGroup];

		    if (mostRecentIntervalDetail == NULL)
			[Exceptions raise:@"VProc.mm: found an interval end with no corresponding start"];

		    mostRecentIntervalDetail->end = cur_event;

		    [intervalMap addDetail:NULL
			forIntervalGroup:intervalGroup];
		}
	    }

#pragma mark Dependents
	    ///////////////// DEPENDENTS  ////////////////////
	    for (int i = 0; i < dependentGroups->size(); ++i)
	    {
		DependentGroup *dependentGroup = dependentGroups->at(i);
		if (eventDesc == dependentGroup->Src())
		{
		    details[numDetails]->type = dependentGroup;
		    struct Dependent_Detail *next_dependentDetail =
			&(details[numDetails++]->data.dependent);
		    next_dependentDetail->src = cur_event;

		    // Put this detail in state 1. as described in Detail.h
		    next_dependentDetail->n_dsts = 0;
		    next_dependentDetail->dsts_array_size = 0;
		    next_dependentDetail->dsts = NULL;


		    // Find the identifier for this dependent detail
		    // and add it to the map under that identifier
		    [dependentMap addDetail:next_dependentDetail
			    forIdentifier:GetDependentId(cur_event, eventDesc)];
		}
		else
		{
		    assert (eventDesc == dependentGroup->Dst());

		    struct Dependent_Detail *d =
			[dependentMap
			    getDetailForIdentifier:
				GetDependentId(cur_event, eventDesc)];

		    if (d == NULL)
			[Exceptions raise:
			    @"VProc.mm: Found a dependent dst with no corresponding src"];

		    [self MakeDetailBigEnoughToHoldNewEvent:d];
		    d->dsts[d->n_dsts++] = cur_event;
		}
	    }
    }
}



#pragma mark Description

- (NSString *)description
{
    return @"<<< VProc >>>";
}
/*
{
    NSMutableString *ret = [NSMutableString stringWithFormat:
	@"<< VProc %d, %d events:", vpId, numEvents];
    for (int i = 0; i < numEvents; ++i)
    {
	[ret appendString:@"\n"];
	[ret appendString:@"\t\t\t"];
	[ret appendString:DynamicEventDescription((*events)[i])];
    }
    [ret appendString:@" >>"];
    return ret;
}
*/

@end
