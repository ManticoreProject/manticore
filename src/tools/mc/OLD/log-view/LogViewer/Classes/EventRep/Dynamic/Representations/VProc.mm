/** \file  VProc.mm
 * \author Korei Klein
 * \date 7/10/09
 *
 */

#import "VProc.h"
#import "log-desc.hxx"
#import "event-desc.hxx"
//#import "DetailAccess.h"
#import "Exceptions.h"
#import "Detail.h"
#import "VProcMaps.h"
#import "Box.h"
#import "Utils.h"

/// The number of details for each event on average.
/// The value need not be exact, it is used to guess an initial size
/// For the Details array.
#define ESTIMATED_DETAILS_PER_EVENT ( 0.4 )
#define MAX_NUM_EVENTS ( 6000000 )

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
@synthesize numDetails;
@synthesize vpId;
@synthesize details;

- (uint64_t)start
{
    if (numEvents == 0)
	[Exceptions raise:@"VProc was asked for the start when there was no start"];
    return start;
}
- (uint64_t)end
{
    if (numEvents == 0)
	[Exceptions raise:@"VProc was asked for the end when there was no end"];
    return end;
}

#pragma mark Initialization

/// Initialize
- (VProc *)initWithLog:(LogBuffer_t *)logBuffer
	    andLogDesc:(LogFileDesc *)logDescVal
		header:(LogFileHeader_t *)headerVal
	     numEvents:(int)n
	     allStates:(NSArray *)allStates
	  dependentMap:(DependentMap *)dependentMapVal
{

    if (![super init]) return nil;
    
    first_event_times = [[NSMutableArray alloc] init];
    // Initialize start and end to the latest and earliest times respectively
    start = -1;
    end = 0;

    header = headerVal;
    vpId = logBuffer->vpId;
    logDesc = logDescVal;

    stateMap = [[StateMap alloc] init];
    intervalMap = [[IntervalMap alloc] init];
    dependentMap = dependentMapVal;
    
    int events_to_read = min(n, logBuffer->next);
    numEvents = 0;
    events_size = MAX_NUM_EVENTS;// events_to_read
    events = (event *)[Utils calloc:events_size size:sizeof(event)];

    event * e = events;
    // The array of details and the initial states are related
    // They must be initialized in tandem
#pragma mark Detail and State Initialization
    
    numDetails = 0;
    details_size = ESTIMATED_DETAILS_PER_EVENT * events_size + allStates.count;
    details = (TaggedDetail_struct * *) [Utils calloc:details_size size:sizeof(TaggedDetail_struct *)];


    for (Box *stateGroupBox in allStates)
    {
	
	StateGroup *stateGroup = (StateGroup *) [stateGroupBox unbox];
	details[numDetails] = (TaggedDetail_struct *) [Utils calloc:1
							       size:sizeof(TaggedDetail_struct)];
	details[numDetails]->type = stateGroup;
	details[numDetails]->data.state.state = stateGroup->StartState();
	//NSLog(@"StateGroup %s has start state %d", stateGroup->Desc(), details[numDetails]->data.state.state);
	details[numDetails]->data.state.start = NULL; //< The detail starts at the begginig of time
	details[numDetails]->data.state.end = NULL;   //< The detail ends at the end of time
	
	[stateMap addDetail:&details[numDetails]->data.state forStateGroup:stateGroup];
	
	++numDetails;
    }
    
    
    [self read:events_to_read eventsFrom:logBuffer->log];
    
    // assert( numEvents == events_size );
    
    return self;
}

double cur_interval_height = 0.0;
- (double)nextIntervalHeight:(struct Interval_Detail *)i
{
    return cur_interval_height += 20.0;
}

- (void)readBlock:(LogBuffer_t *)logBuffer
	numEvents:(int)n
{
    int eventsToRead = min(n, logBuffer->next);
    
    assert( numEvents + eventsToRead < events_size );
    
    // events = (event *) realloc(events, sizeof(event) * (events_size = events_size + eventsToRead));
    
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
	ArgType t = eventDesc->GetArgType(i);
	if (t == EVENT_ID || t == NEW_ID)
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
        d->dsts = (Dependent_Dst **) [Utils calloc:(d->dsts_array_size = DSTS_ARRAY_INC)
					      size:sizeof(Dependent_Dst *)];
    }
    else if (d->n_dsts == d->dsts_array_size)
    {
        // Grow the array
        d->dsts = (Dependent_Dst **) [Utils realloc:d->dsts
					       size:sizeof(Dependent_Dst *) * (d->dsts_array_size = d->dsts_array_size + DSTS_ARRAY_INC)];
    }
    else
    {
	assert (d->n_dsts < d->dsts_array_size);
    }
}
    
    
/// Main event reading logic
/// Assume: events is big enough to hold all the events
/// Assume: details may not be big enough to hold all the details
- (void)read:(int)n eventsFrom:(LogEvent_t *)array
{
    assert ( numEvents + n < events_size );
    for (int i = 0; i < n; ++i, ++numEvents)
    {
	// Add the event
	//event temp1;
	//temp1.timestamp = events[numEvents].timestamp;
	event * e = events;
	event *cur_event = &events[numEvents];

	cur_event->timestamp = GetTimestamp(&array[i].timestamp, header);
	memcpy(&(cur_event->value), &array[i], sizeof(LogEvent_t));
	
	// Recompute start and end times
	if (cur_event->timestamp >= end) end = cur_event->timestamp;
	if (cur_event->timestamp <= start) start = cur_event->timestamp;
	
	if (i == 0)
	{
	    // cur_event is the first in its block; check that we havn't seen
	    // it before, and add it to the array of first events
	    for (NSNumber *n in first_event_times)
	    {
		if (n.unsignedLongLongValue == cur_event->timestamp)
		{
		    NSLog(@" ************ VProc.mm: DUPLICATE BLOCK DETECTED IN LOGFILE");
		    return;
		}
	    }
	    NSNumber *n = [NSNumber numberWithUnsignedLongLong:cur_event->timestamp];
	    [first_event_times addObject:n];
	}

	
	// Add the associated details
	EventDesc *eventDesc = logDesc->FindEventById(cur_event->value.event);

	std::vector<StateGroup *> *stateGroups = logDesc->StateGroups(eventDesc);
    	std::vector<IntervalGroup *> *intervalGroups = logDesc->IntervalGroups(eventDesc);
    	std::vector<DependentGroup *> *dependentGroups = logDesc->DependentGroups(eventDesc);

    	// No more details will be added than the sum of the lengths of these vectors
	int n_more_details = 0;
	int n_state_groups = (stateGroups == NULL) ? 0 : stateGroups->size();
	int n_interval_groups = (intervalGroups == NULL) ? 0 : intervalGroups->size();
	int n_dependent_groups = (dependentGroups == NULL) ? 0 : dependentGroups->size();
	n_more_details += n_state_groups;
	n_more_details += n_interval_groups;
	n_more_details += n_dependent_groups;
	if (numDetails + n_more_details > details_size)
	{
	    details = (TaggedDetail_struct * *)
		[Utils realloc:details
			  size:(details_size = details_size + n_more_details + DSTS_ARRAY_INC) *
				    sizeof(TaggedDetail_struct *)];
	}

	
#pragma mark Simples
	    ///////////////// SIMPLES     ////////////////////


	    // FIXME XXX
	
	
	
#pragma mark States
	    ///////////////// STATES      ////////////////////
	// NSLog(@"VProc.mm: number of state groups %d", n_state_groups);
	for (int i = 0; i < n_state_groups; ++i)
	    {
		StateGroup *stateGroup = stateGroups->at(i);
		struct State_Detail *mostRecentStateDetail =
		    [stateMap getDetailForStateGroup:stateGroup];
		if (mostRecentStateDetail == NULL)
		    [Exceptions raise:@"States were not properly initialized"];

		details[numDetails] = (TaggedDetail_struct *) [Utils calloc:1
								       size:sizeof(struct TaggedDetail_struct)];
		details[numDetails]->type = stateGroup;
		details[numDetails]->eventDesc = eventDesc;
		struct State_Detail *next_stateDetail = &(details[numDetails]->data.state);

		next_stateDetail->state = stateGroup->NextState
		    (mostRecentStateDetail->state, eventDesc);

		// NSLog(@"VProc.mm: mostRecentStateDetail = %#x", mostRecentStateDetail);
		mostRecentStateDetail->end = cur_event;
		if (mostRecentStateDetail->start != NULL)
		{
		    if (mostRecentStateDetail->start->timestamp > cur_event->timestamp)
		    {
			NSLog(@"********** BAD: ON VPROC %d AN EVENT ENDED BEFORE IT STARTED.\n start at %qu end at %qu\n IGNORING. starttime",
			      vpId, mostRecentStateDetail->start->timestamp, mostRecentStateDetail->end->timestamp);
			// assert ( 0 );
		    }
		    
		}
		//NSLog(@"VProc.mm: found state event at time %qu", cur_event->timestamp);
		if (cur_event->timestamp == 0)
		{
		    NSLog(@"0 timestampped event %x", cur_event);
		}
		next_stateDetail->start = cur_event;
		next_stateDetail->end = NULL;
		[stateMap addDetail:&details[numDetails]->data.state forStateGroup:stateGroup];
		
		
		++numDetails;
	    }

#pragma mark Intervals
	    ///////////////// INTERVALS   ////////////////////
	    for (int i = 0; i < n_interval_groups; ++i)
	    {
		IntervalGroup *intervalGroup = intervalGroups->at(i);
		if (eventDesc == intervalGroup->Start())
		{
		    details[numDetails] = (TaggedDetail_struct *) [Utils calloc:1
									   size:sizeof(struct TaggedDetail_struct)];
		    details[numDetails]->type = intervalGroup;
		    details[numDetails]->eventDesc = eventDesc;
		    struct Interval_Detail *next_intervalDetail =
			&(details[numDetails++]->data.interval);
		    next_intervalDetail->start = cur_event;
		    next_intervalDetail->end = NULL;
		    next_intervalDetail->height = 0; //< To be adjusted later

		    [intervalMap addDetail:next_intervalDetail
			  forIntervalGroup:intervalGroup];
		}
		else
		{
		    assert (eventDesc == intervalGroup->End());
		    struct Interval_Detail *mostRecentIntervalDetail =
			[intervalMap getDetailForIntervalGroup:intervalGroup];

		    if (mostRecentIntervalDetail == NULL)
		    {
			NSLog(@"VProc.mm: found an interval end with no corresponding start, ignoring");
			continue;
		    }

		    mostRecentIntervalDetail->end = cur_event;

		    mostRecentIntervalDetail->height = [self nextIntervalHeight:mostRecentIntervalDetail];

		    
		    [intervalMap addDetail:NULL
			forIntervalGroup:intervalGroup];
		}
	    }

#pragma mark Dependents
	    ///////////////// DEPENDENTS  ////////////////////
	    for (int i = 0; i < n_dependent_groups; ++i)
	    {

		DependentGroup *dependentGroup = dependentGroups->at(i);
		uint64_t ident = GetDependentId(cur_event, eventDesc);
			
		TaggedDetail_struct *d = [dependentMap getDetailForIdentifier:ident];
		//NSLog(@"Found a dependent with id = %#qx", ident);
		if (d == NULL)
		{
		    // Initialize this detail
		    d = (TaggedDetail_struct *) [Utils calloc:1
							 size:sizeof(struct TaggedDetail_struct)];
		    d->type = dependentGroup;
		    d->eventDesc = eventDesc;
		    // put d in state 1 as described in Detail.h
		    d->data.dependent.n_dsts = 0;
		    d->data.dependent.dsts_array_size = 0;
		    d->data.dependent.dsts = NULL;
		    [self addEvent:cur_event
		     withEventDesc:eventDesc
		 andDependentGroup:dependentGroup
			  toDetail:&d->data.dependent];
		    [dependentMap addDetail:d forIdentifier:ident];
		}
		else
		{
		    // This detail already exists, simply modify it
		    [self addEvent:cur_event
		     withEventDesc:eventDesc
		 andDependentGroup:dependentGroup
			  toDetail:&d->data.dependent];
		}
		

	    }
    }
}

- (void) addEvent:(event *)e
    withEventDesc:(EventDesc *)eventDesc
andDependentGroup:(DependentGroup *)g
	 toDetail:(struct Dependent_Detail *)d
{

    if (eventDesc == g->Src())
    {
	//NSLog(@"VProc.mm: found dependent 0x%x source adding event 0x%x", d, e);
	assert(d->src == 0); //< src should be uninitialized
	d->src = e;
	d->vpId = vpId;
    }
    else
    {
        assert (eventDesc == g->Dst());

	//NSLog(@"VProc.mm: found dependent 0x%x destination", d);
        [self MakeDetailBigEnoughToHoldNewEvent:d];
	d->dsts[d->n_dsts] = (Dependent_Dst *) [Utils calloc:1
							size:sizeof(Dependent_Dst)];
	d->dsts[d->n_dsts]->vpId = vpId;
	d->dsts[d->n_dsts++]->value = e;


    }
    return;
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
a dependent with id
	[ret appendString:@"\n"];
	[ret appendString:@"\t\t\t"];
	[ret appendString:DynamicEventDescription((*events)[i])];
    }
    [ret appendString:@" >>"];
    return ret;
}
*/

@end
