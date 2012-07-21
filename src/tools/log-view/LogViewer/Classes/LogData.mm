/** \file LogData.mm
 \author Korei Klein
 \date 7/10/09

 */

#import "LogData.h"
#import "log-file.h"
#import "VProc.h"
#import <sys/stat.h>
#import "Detail.h"
#import "log-desc.hxx"
#import "Utils.h"
#import "Exceptions.h"
#import "OutlineViewDataSource.h"
#import "Box.h"
#import "event-desc.hxx"
#import "VProcMaps.h"
#import <strings.h>
// #include "LogDataUtils.h"

#include <fcntl.h>
#include <sys/mman.h>


////////////// UTILS ///////////////////// {{{
#pragma mark Utils

/// Determine if two events have the same value.
/// If they have the same value, then they must be
/// the same event or they must represent the same event.
int event_value_equal(LogEvent_t *e1, LogEvent_t *e2)
{
    if (!strncmp( (const char *) (&e1->data), (const char *) (&e2->data), 5)) return 1;
    else return 0;
}

/// Contains information about how many details need to be allocated
/**
 * Each instance of this structure corresponds to a single vproc vp
 * Each instance of this structure will exist in one of three states
 *	0. counting details:
 *	    in this state the following properties hold:
 *		current == 0
 *		stateGroups is <= than the number of stateGroup details vp will eventually hold
 *		intervalGroups is <= than the number of intervalGroup details vp will eventually hold
 *	1. initializing details:
 *	    in this state the following properties hold:
 *		stateGroups == the number of stateGroup details vp will eventually hold
 *		intervalGroups == the number of intervalGroup details vp will eventually hold
 *		current == the number of details initialized so far
 *	2. final:
 *	    current = stateGroups + intervalGroups
 *	    all details have been initialized
 *	    this structure will never be modified again
 */
typedef struct Detail_Size_Info
    {
	uint64_t current;
	size_t stateGroups;
	size_t intervalGroups;
    } Detail_Size_Info;

/// precondition: info points to an allocated data structure, which may contain garbage.
/// postcondition: info poins to an allocated data structure which contains info for a new detail of size 0
void init_detail_size_info(struct Detail_Size_Info *info)
{
    info->current = 0;
    info->stateGroups = 0;
    info->intervalGroups = 0;
}

/// increments the Detail_Size_Info structure to reflect the fact that there are now more
/// stateGroups and intervalGroups
void detail_size_add(struct Detail_Size_Info *info, int stateGroups, int intervalGroups)
{
    info->stateGroups += stateGroups;
    info->intervalGroups += intervalGroups;
}


/// return the number of details initailzed so far
uint64_t detail_size_current(struct Detail_Size_Info *info)
{
    return info->current;
}

/// update current to reflect the fact that a new detail was initialized
void detail_size_incr_current(struct Detail_Size_Info *info)
{
    info->current++;
    if (info->current > info->stateGroups + info->intervalGroups)
    {
	[Exceptions raise:
	@"LogData.mm: detail_size_incr_current: someone tried to create more details than the total of stateGroups + intervalGroups"];
    }
}


/// returns the total number of details the corresponding vproc will hold
/// note: info must also be in state 1
size_t detail_size_total(struct Detail_Size_Info *info)
{
    size_t res = 0;

    res += info->stateGroups;
    res += info->intervalGroups;

    return res;
}



/// Structure to contain the maps that a single vproc will use to connect pairs of
/// events in the logfile with each other to create details
typedef struct VProc_Maps
    {
	StateMap *stateMap;
	IntervalMap *intervalMap;
    } VProc_Maps;

/// precondition: maps points to an allocated data structure, which may contain garbage
/// postcondition: maps points to an allocated data structure, which contains empty freshly initialized maps
void init_vproc_maps(struct VProc_Maps *maps)
{
    maps->stateMap = [[StateMap alloc] init];
    maps->intervalMap = [[IntervalMap alloc] init];
}

/// Ordered minimum
int min (int a, int b)
{
    if ( a < b ) return a;
    else return b;
}



/** convert a timestamp to nanoseconds */
static inline uint64_t GetTimestamp (LogTS_t *ts, LogFileHeader_t *Hdr)
{
    if (Hdr->tsKind == LOGTS_MACH_ABSOLUTE)
	return ts->ts_mach;
    else if (Hdr->tsKind == LOGTS_TIMESPEC)
	return ts->ts_val.sec * 1000000000 + ts->ts_val.frac;
    else /* Hdr->tsKind == LOGTS_TIMEVAL */
	return ts->ts_val.sec * 1000000000 + ts->ts_val.frac * 1000;
}

/// Copy an event from from to to
static inline void copy_event(event *to, LogEvent_t *from, LogFileHeader_t *header)
{
    to->timestamp = GetTimestamp(&from->timestamp, header);
    memcpy(&to->value, from, sizeof(LogEvent_t));
}

/// Find the identifier for this dependent event
/** This function makes two very serious assumptions:
 0. Every dependent event's identifier is the first of its arguments whose
 type is EVENT_ID
 1. such an argument exists

 If assumption 1 is not met, this function will raise an exception.
 If assumption 0 is not met, but assumption 1 is, this function will not return
    the identifier for this dependent event.
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
	@"LogData.mm: GetDependentId: arguments did not meet a necessary assumption"];
	return -1;
}


/**
 This class will be used to create the allStates array.
 Create a StateInitializationVisitor and pass it to
 LogFileDesc::{pre,post}orderWalk to create an NSMutablearray of
 all the states that exist in the given LogFileDesc.
 */
class StateInitializationVisitor : public LogDescVisitor {
public:
    StateInitializationVisitor() { this->_states = [[NSMutableArray alloc] init]; }
    void VisitGroup (EventGroup *) { }
    void VisitStateGroup (StateGroup *grp) {
	[this->_states addObject:[Box box:grp]];
    }
    void VisitIntervalGroup (IntervalGroup *grp) { }
    void VisitDependentGroup (DependentGroup *grp) { }
    
    NSMutableArray *States () { return this->_states; }
    
private:
    NSMutableArray *_states;
};



/////////////////////// END UTILS //////////////////////// }}}


@implementation LogData

@synthesize desc;
@synthesize dependentMap;
@synthesize allStates;

- (NSArray *)dependentDetails
{
    // An array of boxed TaggedDetail_struct *
    return dependentDetails;
}




/// global variable for use solely in nextIntervalHeight
double cur_interval_height = 0.0;

/// Configuration algorithm for determining the next height
/// of an interval.
- (double)nextIntervalHeight:(struct Interval_Detail *)i
{
    return cur_interval_height += 20.0;
}


// Main method for reading data from a .mlg logFile interpreted according to a LogFileDesc.
- (LogData *)initWithFilename:(NSString *)filenameVal
	    andLogFileDesc:(struct LogFileDesc *)logDesc
{
    if (![super init]) return nil;
    filename = filenameVal;
    firstTime = -1;
    lastTime = 0;

    // Initialize IVars {{{
    {
	vProcs = [[NSMutableArray alloc] init];

	
	/* Define this macro if you want to perform some checks to see if there are
	 * duplicate blocks in the log file caused by a bug in the Manticore logger.
	 * This bug seems to have been fixed, and the checks are very slow, but
	 * the code is useful to have around. */
#ifdef GO_WAY_SLOW_TO_CHECK_FOR_DUPLICATE_BLOCK_PROBLEM
    	first_event_times = [[NSMutableArray alloc] init];
#endif
    	dependentMap = [[DependentMap alloc] init];

    	// NSLog(@"LogData.mm: About to compute allStates");

    	// Compute allStates {{{
    	{
    	    StateInitializationVisitor *v = new StateInitializationVisitor();
    	    logDesc->PreOrderWalk(v);
    	    allStates = v->States();
    	    delete v;
    	}
    	// }}}
    }
    // }}}

    int LogBufSzB = LOGBLOCK_SZB;
    const char *cFileName = [filename cStringUsingEncoding:NSASCIIStringEncoding];


    // get the file size
    off_t fileSize;
    // {{{
    {
        struct stat st;
        if (stat(cFileName, &st) < 0)
	    [Exceptions raise:@"LogFile: could not stat the given file"];
        fileSize = st.st_size;
    }
    // }}}

  // mmap the file
    // {{{
    int fileDes = open (cFileName, O_RDONLY);
    if (fileDes < 0) [Exceptions raise:@"could not open the given file"];

    char *MappedFile = (char *) (mmap (NULL, fileSize, PROT_READ, MAP_PRIVATE, fileDes, 0));
    if (MappedFile == (char *)-1) {
	perror("Mmap failure");
	[Exceptions raise:@"could not mmap the given file"];
    }
    // }}}
    ///////////////////// Read in the header and check for well formedness ///////
    ///////////////////// Recompute LogBufSzB if necessary ///////////////////////
    // {{{
    header = (LogFileHeader_t *) [Utils calloc:1 size:sizeof(LogFileHeader_t)];
    memcpy(header, MappedFile, sizeof(LogFileHeader_t));

    // check the header
    if (header->magic != LOG_MAGIC) [Exceptions raise:@"bogus magic number"];
    if (header->hdrSzB != sizeof(LogFileHeader_t)) [Exceptions raise:@"bad header size"];
    if (header->majorVersion != LOG_VERSION_MAJOR) ; // Don't worry about versions yet
    if (header->bufSzB != (uint32_t) LogBufSzB)
    {
        // recompute block size
        NSLog(@"using block size %d instead of %d", header->bufSzB, LogBufSzB);
        LogBufSzB = header->bufSzB;
    }
    // }}}

    LogBuffer_t *buffersStart = (LogBuffer_t *) (MappedFile + LogBufSzB);

    // -1 is to compensate for the fact that the header of the block takes up exactly one
    // LogEvent_t of data at the beggining of the block
    int NEventsPerBuf = (LogBufSzB / sizeof(LogEvent_t)) - 1;
    // -1 to compensate for the fact that the file header takes up one block
    int numBufs = (fileSize / LogBufSzB) - 1;
    if (numBufs <= 0) [Exceptions raise:@"LogFile: There are no buffers in the logfile"];
    // Maximum number of events in the entire log file
    int MaxNumEvents = NEventsPerBuf * numBufs;

    //////////////////////////// Create the products of PASS #1 //////////////////// {{{


    event *events = (event *) ([Utils calloc:MaxNumEvents size:(sizeof(event))]);
    int cur_event = 0;

    int n_dependent_sources = 0;
    int n_dependent_dsts = 0;

    uint64_t *events_per_vproc = (uint64_t *) calloc(header->nVProcs, sizeof(uint64_t));

    struct Detail_Size_Info *detail_size_info_per_vproc = (struct Detail_Size_Info *) calloc(header->nVProcs, sizeof(struct Detail_Size_Info));

    DependentSizeMap *dependentSizeMap = [[DependentSizeMap alloc] init];

    // }}}
    // ************************************************  PASS #1 Read in the events, and compute sizes and first/last times  ****************************** //{{{
    /* preconditions:
	0. numBufs is the number of buffers in the log file
	1. buffersStart points to a large portion of memory which is mmapped to the
	    portion of the log file starting at the first LogBuffer_t and ending at the end of the file
	2. header is initialized to contain all the header information associated with the log file
	3. events_per_vproc is an array of header->nVProcs 0s
	4. n_dependent_sources is 0
	5. n_dependent_dsts is 0
	6. cur_event is 0
	7. detail_size_info_per_vproc is an array of header->nVProcs newly initialized Detail_Size_Info structures
	8. dependentSizeMap is a new DependentSizeMap
	9. first >= the timestamp of the first event in the log file
	10. lastTime <= the timestamp of the last event in the log file
	11. logDesc is the LogFileDesc this method is using to interpret the log file
	12. events is an array of events. The array is larger than the number of events in the log file
       prostconditions:
	    0. all events have been processed (see LOOP INVARIANTS below)
    */
    for (int i = 0;  i < numBufs;  i++) {
        LogBuffer_t *log = &(buffersStart[i]);
        assert (log->vpId < header->nVProcs);
        assert (log->next < MaxNumEvents);

	// there are --by definition of the log file format-- J events in each LogBuffer t
	// where J is the minimum of log->next and NEventsPerBuf
	int numEventsInBuf = min(log->next, NEventsPerBuf);
	for (int j = 0;  j < numEventsInBuf;  ++j, ++cur_event) {
	  /*  LOOP INVARIANTS:
		0. the first cur_event events in the log file (and no more) have been "processed"
		1. cur event is the first index in events which does not contain an event
		2. all events in the first i LogBuffer_ts in the log file (and no more) have been processed
		3. the first j events in the ith LogBuffer_t in the log file (and no more) have been processed
		4. firstTime is the largest uint64_t which is <= the minimum timestamp on events which have been processed
		5. lastTime is the smallest uint64_t which is >= the maximum timestamp on events which have been processed
		6. when the rth event in the log file is processed, the rth element of events_per_vproc will contain its data and timestamp
		7. for every vproc v, detail_size_info_per_vproc[v] contains at least [OPTIMIZE: exactly] the number of state and interval details
			which can be drawn for the events which have been processed (not including initial state details)
		8. for each dependent identifier id, dependentSizeMap at id is the number of destination dependent details corresponding to id
		9. n_dependent_src and n_dependent_dsts are the number of source and destination details which can be drawn for the
			events which have been processed
	*/
	    events_per_vproc[log->vpId]++;
	    copy_event(&events[cur_event], &log->log[j], header);

	    // Recompute first and last times as necessary
	    // {{{
	    {
		uint64_t ts = events[cur_event].timestamp;
		if (ts < firstTime) firstTime = ts;
		if (ts > lastTime) lastTime = ts;
	    }
	    // }}}

	    EventDesc *eventDesc = logDesc->FindEventById(events[cur_event].value.event);

	    std::vector<StateGroup *> *stateGroups = logDesc->StateGroups(eventDesc);
	    std::vector<IntervalGroup *> *intervalGroups = logDesc->IntervalGroups(eventDesc);
	    std::vector<DependentGroup *> *dependentGroups = logDesc->DependentGroups(eventDesc);
	    int n_state_groups = (stateGroups == NULL) ? 0 : stateGroups->size();
	    int n_interval_groups = (intervalGroups == NULL) ? 0 : intervalGroups->size();
	    int n_dependent_groups = (dependentGroups == NULL) ? 0 : dependentGroups->size();

	    // OPTIMIZE: the details_size is being incremented by too much here
	    // there is not a new detail per event per group it is in, but
	    // there is a new detail per pair of (event,group) pairs which define a detail
	    detail_size_add(&detail_size_info_per_vproc[log->vpId], n_state_groups, n_interval_groups);


	    for (int k = 0; k < n_dependent_groups; ++k)
	    {
		DependentGroup *g = dependentGroups->at(k);
		if (eventDesc == g->Src())
		{
		    ++n_dependent_sources;
		}
		else
		{
		    assert (eventDesc == g->Dst());
		    ++n_dependent_dsts;

		    uint64_t ident = GetDependentId(&events[cur_event], eventDesc);
		    [dependentSizeMap incrementCountForIdentifier:ident];
		  //  NSLog(@"dependentSizeMap should increment count for %qu", ident);
		}
	    }
	}
    } // }}}
    // *********************************** Create the products of PASS #2 ********************** // {{{

    // assign to each vproc a state and interval map
    struct VProc_Maps vproc_maps[header->nVProcs];
    for (unsigned int i = 0; i < header->nVProcs; ++i) init_vproc_maps(&vproc_maps[i]);

    // create an array for each vproc to use as an index into the events belonging to it
    event **vproc_events[header->nVProcs];
    for (unsigned int i = 0; i < header->nVProcs; ++i) vproc_events[i] = (event **)
	([Utils calloc:events_per_vproc[i] size:sizeof(event *)]);

    // create an array for each vproc to hold its details
    // {{{
    struct TaggedDetail_struct *vproc_details[header->nVProcs];
    for (unsigned int i = 0; i < header->nVProcs; ++i) {
	// vproc_details[i] must be large enough to hold all details
	// (including initial state details, not including dependents) which must be drawn for vproc i
	vproc_details[i] = (TaggedDetail_struct *)
	    ([Utils calloc:allStates.count + detail_size_total(&detail_size_info_per_vproc[i])
		      size:sizeof(TaggedDetail_struct)]);
	
	    // Partially initialize the above array by creating a new state detail for each element of allStates
	    // This initialization must be done because even if there are no events in the log file, each vproc
	    // is still associated with a single state detail s for each state group g, where s is the initial state of g
	    // and s lasts from the begginning of time to the end of time.
	{
	    StateMap *stateMap = vproc_maps[i].stateMap;
	    for (Box *stateGroupBox in allStates) {
		StateGroup *g = (StateGroup *) ( [stateGroupBox unbox] );

		uint64_t j = detail_size_current(&detail_size_info_per_vproc[i]);
		detail_size_incr_current(&detail_size_info_per_vproc[i]);
		struct TaggedDetail_struct *cur_detail = &vproc_details[i][j];

		cur_detail->type = g;
		cur_detail->eventDesc = NULL;
		struct State_Detail *cur_state_detail = &cur_detail->data.state;

		cur_state_detail->state = g->StartState();
		cur_state_detail->start = cur_state_detail->end = NULL;

		[stateMap addDetail:cur_state_detail forStateGroup:g];
	    }
	}
    }
    // }}}

    // create arrays to hold the dependent source and dependent destination details
    struct TaggedDetail_struct *dependent_sources = (TaggedDetail_struct *)
	([Utils calloc:n_dependent_sources size:sizeof(TaggedDetail_struct)]);
    uint64_t cur_dependent_src = 0;
    struct Dependent_Dst *dependent_dsts = (Dependent_Dst *)
	([Utils calloc:n_dependent_dsts size:sizeof(struct Dependent_Dst)]);
    uint64_t cur_dependent_dst = 0;


    // }}}
    /**************************************************  PASS #2 **************************************************/ // {{{
    /* ************************************************  Read in the single VProc details,
							and read in the dependent details filling in the source ******************************/
    int evtIndex = 0;
    for (int i = 0; i < numBufs; ++i) {
        LogBuffer_t *log = (LogBuffer_t *) (buffersStart + i);

	int numEventsInBuf = min(log->next, NEventsPerBuf);
	for (int j = 0; j < numEventsInBuf; ++j, ++evtIndex) {
	  /* LOOP INVARIANTS:
		0. the first event_idv events in the log file (and no more) have been "processed"
		1. evtIndex is the index of the first event in events which has not been processed
		2. all events in the first i LogBuffer_ts in the log file (and no more) have been processed
		3. the first j events in the ith LogBuffer_t in the log file (and no more) have been processed
		4. for each vproc v, detail_size_info_per_vproc[v]'s current value c is the number of details that have
		    been added to v.
		    also, c is the least index c such that vproc_details[v][c] is empty
		5. when a state or interval detail d has been added, there will be a Detail structure in vproc_details[v]
			where v is the vproc which should draw d,
			(the Detail structure for d will contain all the relevant information about d)
		6. for each vproc v, vproc_maps[v] contains a StateMap s and an IntervalMap im such that:
		    a. s(g) is the latest state for StateGroup g for which a detail has been added
		    b. im(g) is
			I. NULL if the last event processed for IntervalGroup g was an end event
			II. d, where d is a state detail whose start is initialized, and whose end is NULL, and whose height is 0
				such that the start of d is the last event processed for IntervalGroup g
		7. {cur_dependent_src,cur_dependent_dst} is the first index in {dependent_sources,dependent_dsts} which does not
		    contain a dependent {source,destination} detail, it is also the number of {source,destination} detail which have been added
		8. if a destination detail d has been added, it will exists in dependent_dsts with its value and vpId fields initialized.

		     ************ PAY ATTENTION !! IMPORTANT !!!!! ****************
		9. if a dependent source detail d has been added then:
		    a dependent source D will exists in dependent_sources
		    D's array size will be the number of destinations it has
		    D's n_dsts will be 0
		    D's array of destinations will NOT be filled in
		    all other fields of D should be properly initialized
		10. dependentSizeMap(k) is the number of destinations associated with the dependent detail with identifier k
	  */
	    LogEvent_t *e = &log->log[j];
	    // evtIndex and e will remain synchronized so that
	    // events[evtIndex] will correspond to e
	    event *evt = &events[evtIndex];
	    assert(event_value_equal(&evt->value, e));

#ifdef GO_WAY_SLOW_TO_CHECK_FOR_DUPLICATE_BLOCK_PROBLEM
	    uint64_t ts = evt->timestamp;
	    // workaround for duplicate block problem
	    bool do_break = false;
	    if (j == 0) {
		for (NSNumber *n in first_event_times) {
		    if (n.unsignedLongLongValue == ts) {
			NSLog(@"**************** LogData.mm: DUPLICATE BLOCK DETECTED IN LOGFILE");
			exit(-1);
			--evtIndex;
			do_break = true;
		    }
		}
	    }
	    NSNumber *n = [NSNumber numberWithUnsignedLongLong:ts];
	    [first_event_times addObject:n];
#endif
	    
	    evt->timestamp -= firstTime;


	    struct VProc_Maps *maps = &vproc_maps[log->vpId];
	    EventDesc *eventDesc = logDesc->FindEventById(e->event);

	    std::vector<StateGroup *> *stateGroups = logDesc->StateGroups(eventDesc);
	    std::vector<IntervalGroup *> *intervalGroups = logDesc->IntervalGroups(eventDesc);
	    std::vector<DependentGroup *> *dependentGroups = logDesc->DependentGroups(eventDesc);

	    int n_state_groups = (stateGroups == NULL) ? 0 : stateGroups->size();
	    int n_interval_groups = (intervalGroups == NULL) ? 0 : intervalGroups->size();
	    int n_dependent_groups = (dependentGroups == NULL) ? 0 : dependentGroups->size();


	    StateMap *stateMap = maps->stateMap;
	    IntervalMap *intervalMap = maps->intervalMap;

		/////////////////////////////// STATES /////////////////////////////
		// {{{
	    for (int k = 0; k < n_state_groups; ++k) {
		StateGroup *g = stateGroups->at(k);
		struct State_Detail *mostRecentStateDetail =
		    [stateMap getDetailForStateGroup:g];

		if (mostRecentStateDetail == NULL)
		    [Exceptions raise:@"States were not properly initialized"];

		uint64_t cur_detail_off = detail_size_current(&detail_size_info_per_vproc[log->vpId]);
		detail_size_incr_current(&detail_size_info_per_vproc[log->vpId]);

		struct TaggedDetail_struct *cur_detail = &vproc_details[log->vpId][cur_detail_off];
		cur_detail->type = g;
		cur_detail->eventDesc = eventDesc;
		struct State_Detail *next_stateDetail = &(cur_detail->data.state);

		next_stateDetail->state = g->NextState(mostRecentStateDetail->state, eventDesc);

		// NSLog(@"VProc.mm: mostRecentStateDetail = %#x", mostRecentStateDetail);

		// Check for well formedness
		// {{{
		mostRecentStateDetail->end = evt;
		if (mostRecentStateDetail->start != NULL)
		{
		    if (mostRecentStateDetail->start->timestamp > evt->timestamp)
		    {
			NSLog(@"********** BAD: ON VPROC %d AN EVENT ENDED BEFORE IT STARTED.\n start at %qu end at %qu\n IGNORING. starttime",
			      log->vpId, mostRecentStateDetail->start->timestamp, mostRecentStateDetail->end->timestamp);
			// assert ( 0 );
		    }
		}

		//NSLog(@"VProc.mm: found state event at time %qu", evt->timestamp);
		if (evt->timestamp == 0)
		{
		    NSLog(@" ************** 0 timestamped event %x", evt);
		}
		// }}}

		mostRecentStateDetail->end = evt;
		next_stateDetail->start = evt;
		next_stateDetail->end = NULL;
		[stateMap addDetail:next_stateDetail forStateGroup:g];
	    } // }}}
		/////////////////////////////// INTERVALS //////////////////////////
		// {{{
	    for (int k = 0; k < n_interval_groups; ++k) {
		IntervalGroup *g = intervalGroups->at(k);

		if (eventDesc == g->Start())
		{
		    // Make a new detail
		    uint64_t cur_detail_off = detail_size_current(&detail_size_info_per_vproc[log->vpId]);
		    struct TaggedDetail_struct *cur_detail = &vproc_details[log->vpId][cur_detail_off];
		    detail_size_incr_current(&detail_size_info_per_vproc[log->vpId]);

		    // Sanity check
		    // {{{
		    {
			struct Interval_Detail *mostRecentIntervalDetail =
			    [intervalMap getDetailForIntervalGroup:g];
			if (mostRecentIntervalDetail != NULL)
			{
			    [Exceptions raise:
				@"found an interval start event before the previous interval had ended"];
			}
		    }
		    // }}}

		    // Initialize the new detail
		    cur_detail->type = g;
		    cur_detail->eventDesc = eventDesc;
		    struct Interval_Detail *next_intervalDetail =
			&(cur_detail->data.interval);
		    next_intervalDetail->start = evt;
		    next_intervalDetail->end = NULL;
		    next_intervalDetail->height = 0; //< To be adjusted later

		    [intervalMap addDetail:next_intervalDetail
			  forIntervalGroup:g];
		}
		else {
		    assert (eventDesc == g->End());
		    struct Interval_Detail *mostRecentIntervalDetail =
			[intervalMap getDetailForIntervalGroup:g];

		    if (mostRecentIntervalDetail == NULL)
		    {
			NSLog(@"VProc.mm: found an interval end with no corresponding start, ignoring");
			continue;
		    }

		    mostRecentIntervalDetail->end = evt;

		    mostRecentIntervalDetail->height = [self nextIntervalHeight:mostRecentIntervalDetail];

		    [intervalMap addDetail:NULL
			forIntervalGroup:g];
		}
	    } // }}}
		/////////////////////////////// DEPENDENTS /////////////////////////////
		// {{{
	    for (int k = 0; k < n_dependent_groups; ++k) {
	       DependentGroup *g = dependentGroups->at(k);
	       uint64_t ident = GetDependentId(evt, eventDesc);

	       if (eventDesc == g->Src()) {
		    struct TaggedDetail_struct *d = &dependent_sources[cur_dependent_src++];
		    d->type = g;
		    d->eventDesc = eventDesc;
		    d->data.dependent.dsts_array_size = [dependentSizeMap countForIdentifier:ident];
		    d->data.dependent.dsts = (Dependent_Dst **)
				([Utils calloc:d->data.dependent.dsts_array_size
						    size:sizeof(struct Dependent_Dst *)]);
		    // NSLog(@"added a dsts array to a source %qu, it has size %d", ident ,d->data.dependent.dsts_array_size);
		    d->data.dependent.n_dsts = 0;

		    d->data.dependent.vpId = log->vpId;
		    d->data.dependent.src = evt;
		    [dependentMap addDetail:d forIdentifier:ident];
	       }
	       else {
		    assert ( eventDesc == g->Dst());
		    struct Dependent_Dst *dd = &dependent_dsts[cur_dependent_dst++];
		    dd->vpId = log->vpId;
		    dd->value = evt;
	       }
	    }
	    // }}}

	}
    } // }}}
    /************************************************* PASS #3 For each dependent source event, fill its array of destinations. This is not a pass over the logfile *****************/ // {{{
    // cur_dependent_dst is the number of dependent destinations to be drawn
    // after this loop, each dependent source d should have a detail D in dependent_sources where
    // D->n_dsts == D->dsts_array_size, and all D's dependent dsts
    // should be filled in.  d should contain all information relevant to d
    for (unsigned int i = 0; i < cur_dependent_dst; ++i) {
	struct Dependent_Dst *dst = &dependent_dsts[i];
	EventDesc *eventDesc = logDesc->FindEventById(dst->value->value.event);

	uint64_t ident = GetDependentId(dst->value, eventDesc);
	struct TaggedDetail_struct *source = [dependentMap getDetailForIdentifier:ident];
	source->data.dependent.dsts[source->data.dependent.n_dsts++] = dst;
    }
    // }}}
    // Sanity check // {{{
    {
	for (unsigned int i = 0; i < cur_dependent_src; ++i)
	{
	    struct TaggedDetail_struct *src = &dependent_sources[i];
	    if (src->data.dependent.n_dsts != src->data.dependent.dsts_array_size)
	    {
		NSLog(@"there array has %d dsts, the array size is %d", src->data.dependent.n_dsts, src->data.dependent.dsts_array_size);
		[Exceptions raise:
		    @"LogData.mm: after the first three passes through the log file,\
		    \n\tthere was a dependent source which did not have all its\
		    \n\tdestinations filled in"];
	    }
	}
    }
    // }}}
    /*********************************************** CLEANUP Store all recently gathered data in the array of VProcs and in dependentDetails *********/ // {{{
    {
	for (unsigned int i = 0; i < header->nVProcs; ++i)
	{
	    uint64_t numDetails = detail_size_current(&detail_size_info_per_vproc[i]);
	    // create an index for vproc i which indexes into its array of details
	    Detail *details_index = (Detail *)
		([Utils calloc:numDetails
			  size:sizeof(Detail)]);
	    for (unsigned int j = 0; j < numDetails; ++j)
	    {
		details_index[j] = &vproc_details[i][j];
	    }
	    // NSLog(@"LogData: creating a VProc with numEvents = %qu numDetails = %qu", events_per_vproc[i], numDetails);
	    VProc *vp = [[VProc alloc] initWithVpId:i
					       events:vproc_events[i] numEvents:events_per_vproc[i]
					      details:details_index numDetails:numDetails
					      logDesc:logDesc
					      start:firstTime end:lastTime
					      header:header];
	    [vProcs addObject:vp];
	}
	
	dependentDetails = [[NSMutableArray alloc] init];
	for (int i = 0; i < n_dependent_sources; ++i)
	{
	    [dependentDetails addObject:[Box box: &dependent_sources[i]]];
	}
    }
    // }}}

    return self;
}


- (LogData *)init
{
    [Exceptions raise:@"wrong initialization of LogData"];
    return self;
}






@synthesize filename;
@synthesize vProcs;
@synthesize start;
@synthesize firstTime;
@synthesize lastTime;




#pragma mark fields of header

@synthesize date;
@synthesize clockName;

- (uint64_t)magic
{
    return header->magic;
}
- (uint32_t)majorVersion
{
    return header->majorVersion;
}
- (uint32_t)minorVersion
{
    return header->minorVersion;
}
- (uint32_t)patchVersion
{
    return header->patchVersion;
}
- (uint32_t)hdrSzB
{
    return header->hdrSzB;
}
- (uint32_t)bufSzB
{
    return header->bufSzB;
}
- (uint32_t)tsKind
{
    return header->tsKind;
}
/*
- (LogTS_t)startTime
{
    return header->startTime;
}
 */

- (uint32_t)resolution
{
    return header->resolution;
}
- (uint32_t)nVProcs
{
    return header->nVProcs;
}
- (uint32_t)nCPUs
{
    return header->nCPUs;
}

#pragma mark Description

- (NSString *)description
{
    return @"<<< LogFile object: ... >>>";
}

@end



