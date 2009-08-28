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
#include "LogDataUtils.h"

#include <fcntl.h>
#include <sys/mman.h>


#define STATIC_INLINE static inline



class StateInitialzationVisitor : public LogDescVisitor {
public:
    StateInitialzationVisitor() { this->_states = [[NSMutableArray alloc] init]; }
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


int min (int a, int b)
{
    if ( a < b ) return a;
    else return b;
}



@implementation LogData

@synthesize desc;
@synthesize dependentMap;
@synthesize allStates;

- (NSArray *)dependentDetails
{
    // An array of boxed TaggedDetail_struct *
    return dependentDetails;
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

STATIC_INLINE void copy_event(event *to, LogEvent_t *from, LogFileHeader_t *header)
{
    to->timestamp = GetTimestamp(&from->timestamp, header);
    memcpy(&to->value, from, sizeof(LogEvent_t));
}


STATIC_INLINE int vector_size(std::vector<DependentGroup *> *dependentGroups)
{
    if (dependentGroups == 0) return 0;
    else return dependentGroups->size();
}


double cur_interval_height = 0.0;
- (double)nextIntervalHeight:(struct Interval_Detail *)i
{
    return cur_interval_height += 20.0;
}

/// Find the identifier for this dependent event
// * This function makes a very serious assumption:
// * 1. Every dependent event's identifier is the first of its arguments whose
// type is EVENT_ID, and such an argument exists
//
// If this assumption is not met, the function will raise an exception.
//
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

- (LogData *)initWithFilename:(NSString *)filenameVal
	    andLogFileDesc:(struct LogFileDesc *)logDesc
{
    if (![super init]) return nil;
    filename = filenameVal;
    firstTime = -1;
    lastTime = 0;

    first_event_times = [[NSMutableArray alloc] init];

    dependentMap = [[DependentMap alloc] init];

    // Compute allStates {{{
    {
	StateInitialzationVisitor *v = new StateInitialzationVisitor();
	logDesc->PreOrderWalk(v);
	allStates = v->States();
	delete v;
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

    char *fileMem; // mmap the file
    // {{{
    int fileDes = open (cFileName, O_RDONLY);
    fileMem = (char *) (mmap (NULL, fileSize, PROT_READ, MAP_FILE, fileDes, 0));
    if (fileMem < 0) [Exceptions raise:@"could not mmap the given file"];
    // }}}
    ///////////////////// Read in the header and check for well formedness ///////
    ///////////////////// Recompute LogBufSzB if necessary ///////////////////////
    // {{{
    header = (LogFileHeader_t *) [Utils calloc:1 size:sizeof(LogFileHeader_t)];
    memcpy(header, fileMem, sizeof(LogFileHeader_t));


    // check the header
    if (header->magic != LOG_MAGIC) [Exceptions raise:@"bogus magic number"];
    if (header->hdrSzB != sizeof(LogFileHeader_t)) [Exceptions raise:@"bad header size"];
    if (header->majorVersion != LOG_VERSION_MAJOR) ; // Don't worry about versions yet
    if (header->bufSzB != LogBufSzB)
    {
        // recompute block size
        NSLog(@"using block size %d instead of %d", header->bufSzB, LogBufSzB);
        LogBufSzB = header->bufSzB;
    }
    // }}}

    LogBuffer_t *buffersStart = (LogBuffer_t *) (fileMem + LogBufSzB);

    // -1 is to compensate for the fact that the header of the block takes up exactly one
    // LogEvent_t of data at the beggining of the block
    int NEventsPerBuf = (LogBufSzB / sizeof(LogEvent_t)) - 1;
    int numBufs = (fileSize / LogBufSzB) - 1;
    if (numBufs <= 0) [Exceptions raise:@"LogFile: There are no buffers in the logfile"];

    //////////////////////////// Create the products of PASS #1 //////////////////// {{{

    // Maximum number of events in the entire log file
    int MaxNumEvents = NEventsPerBuf * numBufs;

    event *events = (event *) ([Utils calloc:MaxNumEvents size:(sizeof(event))]);
    int cur_event = 0;

    int num_dependent_sources = 0;
    int num_dependent_dsts = 0;

    uint64_t events_per_vproc[header->nVProcs];
    for (int i = 0; i < header->nVProcs; ++i) events_per_vproc[i] = 0;

    struct Detail_Size_Info details_size_per_vproc[header->nVProcs];
    for (int i = 0; i < header->nVProcs; ++i) init_detail_size_info(&details_size_per_vproc[i]);

    DependentSizeMap *dependentSizeMap = [[DependentSizeMap alloc] init];

    // }}}
    // ************************************************  PASS #1 Read in the events, and compute sizes and first/last times  ****************************** //{{{
    for (int i = 0;  i < numBufs;  i++)
    {
        LogBuffer_t *log = (LogBuffer_t *) (buffersStart + i * LogBufSzB);
        assert (log->vpId < header->nVProcs);
        assert (log->next < MaxNumEvents);

	for (int j = 0; j < min(log->next, NEventsPerBuf); ++j, ++cur_event)
	{
	    events_per_vproc[log->vpId]++;
	    copy_event(&events[cur_event], &log->log[j], header);

	    // Recompute first and last times
	    // {{{
	    {
		uint64_t ts = events[cur_event].timestamp;
		if (ts < firstTime) firstTime = ts;
		if (ts > lastTime) lastTime = ts;
	    }
	    // }}}

	    EventDesc *eventDesc = logDesc->FindEventById(events[cur_event].value.event);

	    detail_size_add(&details_size_per_vproc[cur_event],
		logDesc->StateGroups(eventDesc)->size(),
		logDesc->IntervalGroups(eventDesc)->size());
	    std::vector<DependentGroup *> *dependentGroups = logDesc->DependentGroups(eventDesc);
	    int n_dependent_groups = vector_size(dependentGroups);

	    for (int i = 0; i < n_dependent_groups; ++i)
	    {
		DependentGroup *g = dependentGroups->at(i);
		if (eventDesc == g->Src())
		{
		    ++num_dependent_sources;
		}
		else
		{
		    assert (eventDesc == g->Dst());
		    ++num_dependent_dsts;

		    uint64_t ident = GetDependentId(&events[cur_event], eventDesc);
		    [dependentSizeMap incrementCountForIdentifier:ident];
		}
	    }
	}
    } // }}}
    // *********************************** Create the products of PASS #2 ********************** // {{{

    struct VProc_Maps vproc_maps[header->nVProcs];
    for (int i = 0; i < header->nVProcs; ++i) init_vproc_maps(&vproc_maps[i]);

    /* create an array for each vproc to use as an index into its events */
    event **vproc_events[header->nVProcs];
    for (int i = 0; i < header->nVProcs; ++i) vproc_events[i] = (event **)
	([Utils calloc:events_per_vproc[i] size:sizeof(event *)]);

    /* create an array for each vproc to hold its details */
    // {{{
    struct TaggedDetail_struct *vproc_details[header->nVProcs];
    for (int i = 0; i < header->nVProcs; ++i)
    {
	vproc_details[i] = (TaggedDetail_struct *)
	    ([Utils calloc:allStates.count + detail_size_total(&details_size_per_vproc[i])
		      size:sizeof(TaggedDetail_struct)]);
	
	    // Initialize the above array by creating a new state detail for each element of allStates
	{
	    StateMap *stateMap = vproc_maps[i].stateMap;
	    for (Box *stateGroupBox in allStates)
	    {
		StateGroup *g = (StateGroup *) ( [stateGroupBox unbox] );

		uint64_t j = detail_size_current(&details_size_per_vproc[i]);
		detail_size_incr_current(&details_size_per_vproc[i]);
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

    // /* create an array for the number of destination which each source will have */
    // int n_dsts_per_src[num_dependent_sources];
    // for (int i = 0; i < num_dependent_sources; ++i) n_dsts_per_src[i] = 0;

    struct TaggedDetail_struct *dependent_sources = (TaggedDetail_struct *)
	([Utils calloc:num_dependent_sources size:sizeof(TaggedDetail_struct)]);
    uint64_t cur_dependent_src = 0;
    struct Dependent_Dst *dependent_dsts = (Dependent_Dst *)
	([Utils calloc:num_dependent_dsts size:sizeof(struct Dependent_Dst)]);
    uint64_t cur_dependent_dst = 0;


    // }}}
    /**************************************************  PASS #2 **************************************************/ // {{{
    /* ************************************************  Read in the single VProc details,
							and read in the dependent details filling in the source,
							and annotate each dependent source with the number of dsts ******************************/
    int event_idx = 0;
    for (int i = 0; i < numBufs; ++i)
    {
        LogBuffer_t *log = (LogBuffer_t *) (buffersStart + i * LogBufSzB);
	for (int j = 0; j < min(log->next, NEventsPerBuf); ++j, event_idx++)
	{
	    LogEvent_t *e = &log->log[j];
	    // event_idx and e will remain synchronized so that
	    // events[event_idx] will correspond to e
	    event *evt = &events[event_idx];

	    uint64_t ts = evt->timestamp;
	    // workaround for duplicate block problem
	    int do_break = false;
	    if (j = 0)
	    {
		for (NSNumber *n in first_event_times)
		{
		    if (n.unsignedLongLongValue == ts)
		    {
			NSLog(@"**************** LogData.mm: DUPLICATE BLOCK DETECTED IN LOGFILE");
			do_break = true;
		    }
		}
	    }
	    if (do_break) break;
	    NSNumber *n = [NSNumber numberWithUnsignedLongLong:ts];
	    [first_event_times addObject:n];

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
	    for (int k = 0; k < n_state_groups; ++k)
	    {
		StateGroup *g = stateGroups->at(k);
		struct State_Detail *mostRecentStateDetail =
		    [stateMap getDetailForStateGroup:g];

		if (mostRecentStateDetail == NULL)
		    [Exceptions raise:@"States were not properly initialized"];

		uint64_t cur_detail_off = detail_size_current(&details_size_per_vproc[log->vpId]);
		detail_size_incr_current(&details_size_per_vproc[log->vpId]);

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
		    NSLog(@"0 timestampped event %x", evt);
		}
		// }}}

		mostRecentStateDetail->end = evt;
		next_stateDetail->start = evt;
		next_stateDetail->end = NULL;
		[stateMap addDetail:next_stateDetail forStateGroup:g];
	    } // }}}
		/////////////////////////////// INTERVALS //////////////////////////
		// {{{
	    for (int k = 0; k < n_interval_groups; ++k)
	    {
		IntervalGroup *g = intervalGroups->at(k);

		if (eventDesc == g->Start())
		{
		    // Make a new detail
		    uint64_t cur_detail_off = detail_size_current(&details_size_per_vproc[log->vpId]);
		    struct TaggedDetail_struct *cur_detail = &vproc_details[log->vpId][cur_detail_off];
		    detail_size_incr_current(&details_size_per_vproc[log->vpId]);

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
		else
		{
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
	    for (int k = 0; k < n_dependent_groups; ++k)
	    {
	       DependentGroup *g = dependentGroups->at(k);
	       uint64_t ident = GetDependentId(evt, eventDesc);

	       if (eventDesc == g->Src())
	       {
		    struct TaggedDetail_struct *d = &dependent_sources[cur_dependent_src++];
		    d->type = g;
		    d->eventDesc = eventDesc;
		    d->data.dependent.dsts_array_size = [dependentSizeMap countForIdentifier:ident];
		    d->data.dependent.dsts = (Dependent_Dst **)
				([Utils calloc:d->data.dependent.dsts_array_size
						    size:sizeof(struct Dependent_Dst *)]);
		    d->data.dependent.n_dsts = 0;

		    d->data.dependent.vpId = log->vpId;
		    d->data.dependent.src = evt;
		    [dependentMap addDetail:d forIdentifier:ident];
	       }
	       else
	       {
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
    for (int i = 0; i < num_dependent_dsts; ++i)
    {
	struct Dependent_Dst *dst = &dependent_dsts[i];
	EventDesc *eventDesc = logDesc->FindEventById(dst->value->value.event);

	uint64_t ident = GetDependentId(dst->value, eventDesc);
	struct TaggedDetail_struct *source = [dependentMap getDetailForIdentifier:ident];
	source->data.dependent.dsts[source->data.dependent.n_dsts++] = dst;
    }
    // }}}
    // Sanity check // {{{
    {
	for (int i = 0; i < num_dependent_sources; ++i)
	{
	    struct TaggedDetail_struct *src = &dependent_sources[i];
	    if (src->data.dependent.n_dsts != src->data.dependent.dsts_array_size)
	    {
		[Exceptions raise:
		    @"LogData.mm: after the first three passes through the log file, \
		    there was a dependent source which did not have all its \
		    destinations filled in"];
	    }
	}
    }
    // }}}
    /*********************************************** CLEANUP Store all recently gathered data in VProcs and in dependentDetails *********/ // {{{
    {
	for (int i = 0; i < header->nVProcs; ++i)
	{
	    uint64_t numDetails = detail_size_total(&details_size_per_vproc[i]);
	    Detail *details_vproc_format = (Detail *)
		([Utils calloc:numDetails
			  size:sizeof(Detail)]);
	    for (int j = 0; j < numDetails; ++j)
	    {
		details_vproc_format[j] = &vproc_details[i][j];
	    }
	    
	    VProc *vp = [[VProc alloc] initWithVpId:i
					       events:vproc_events[i] numEvents:events_per_vproc[i]
					      details:details_vproc_format numDetails:numDetails
					      logDesc:logDesc
					      start:firstTime end:lastTime
					      header:header];
	    [vProcs addObject:vp];
	}
	
	dependentDetails = [[NSMutableArray alloc] init];
	for (int i = 0; i < num_dependent_sources; ++i)
	{
	    [dependentDetails addObject:[Box box: &dependent_sources[i]]];
	}
    }
    // }}}
    return self;
}


/*

- (LogData *)initWithFilename:(NSString *)filenameVal
	       andLogFileDesc:(struct LogFileDesc *)logDesc
{
    if (![super init]) return nil;
    filename = filenameVal;

    firstTime = -1;
    lastTime = 0;

    dependentMap = [[DependentMap alloc] init];

    StateInitialzationVisitor *v = new StateInitialzationVisitor();
    logDesc->PreOrderWalk(v);

    allStates = v->States();

    delete v;

    int LogBufSzB = LOGBLOCK_SZB;

    NSFileHandle *f = [NSFileHandle fileHandleForReadingAtPath:filename];
    if (!f) [Exceptions raise:@"LogFile: file access error"];

    // read the header
    NSData *fileHeader = [f readDataOfLength:LOGBLOCK_SZB];
    // Protect this header, it could be garbage collected when fileHeader is.
    header = (LogFileHeader_t *) malloc(fileHeader.length);
    memcpy(header, fileHeader.bytes, fileHeader.length);

    // NSLog(@"starttime for file is at %qu", GetTimestamp(&header->startTime, header));

    // check the header
    if (header->magic != LOG_MAGIC) [Exceptions raise:@"bogus magic number"];
    if (header->hdrSzB != sizeof(LogFileHeader_t)) [Exceptions raise:@"bad header size"];
    if (header->majorVersion != LOG_VERSION_MAJOR) ; // Don't worry about versions yet
    if (header->bufSzB != LogBufSzB)
    {
        // recompute block size
        NSLog(@"using block size %d instead of %d", header->bufSzB, LogBufSzB);
        LogBufSzB = header->bufSzB;
        // reset input file pointer
        [f seekToFileOffset:LogBufSzB];
    }

    // get the file size
    off_t fileSize;
    {
        struct stat st;
        if (stat([filename cStringUsingEncoding:NSASCIIStringEncoding], &st) < 0)
    	[Exceptions raise:@"LogFile: could not stat the given file"];
        fileSize = st.st_size;
    }

    // -1 is to compensate for the fact that the header of the block takes up exactly one
    // LogEvent_t of data at the beggining of the block
    int NEventsPerBuf = (LogBufSzB / sizeof(LogEvent_t)) - 1;
    int numBufs = (fileSize / LogBufSzB) - 1;
    if (numBufs <= 0) [Exceptions raise:@"LogFile: There are no buffers in the logfile"];

    // Maximum number of events in the entire log file
    int MaxNumEvents = NEventsPerBuf * numBufs;

    VProc *vProcs_c_array[header->nVProcs];  // To be converted into an NSMutableArray later
    for (int i = 0; i < header->nVProcs; ++i)
	vProcs_c_array[i] = NULL;

    // read in the blocks
    for (int i = 0;  i < numBufs;  i++)
    {
        LogBuffer_t *log = (LogBuffer_t *) [[f readDataOfLength:LogBufSzB] bytes];
        assert (log->vpId < header->nVProcs);

	
        assert (log->next < MaxNumEvents);

        if (vProcs_c_array[log->vpId] == NULL)
        {
	    // Create a new VProc to put int the c_array
	    vProcs_c_array[log->vpId] = [[VProc alloc]
		initWithLog:log
		andLogDesc:logDesc
		    header:header
		 numEvents:NEventsPerBuf
		 allStates:allStates
	      dependentMap:dependentMap];
        }
        else
	{
    	   // This vProc exists, simply add the block
	    assert( vProcs_c_array[log->vpId].events != 0 );
	   [vProcs_c_array[log->vpId] readBlock:log numEvents:NEventsPerBuf];
        }
    }


    // Check that vProcs_c_array is fully initialized
    for (int i = 0; i < header->nVProcs; ++i)
    {
        if (vProcs_c_array[i] == NULL)
    	[Exceptions raise:@"Did not find enough vProcs in the log file"];
    }

    BOOL foundFirstAndLastTimes = NO;
    vProcs = [[NSMutableArray alloc] initWithCapacity:header->nVProcs];
    for (int i = 0; i < header->nVProcs; ++i)
    {
	VProc *vp = vProcs_c_array[i];
	[vProcs addObject:vp];
	if (vp.numEvents !=0)
	{
	    if (vp.start <= firstTime) firstTime = vp.start;
	    if (vp.end >= lastTime) lastTime = vp.end;
	    foundFirstAndLastTimes = YES;
	}
    }
    if (!foundFirstAndLastTimes)
	[Exceptions raise:
	 @"Could not calculate first and last times based on log. There were no events."];

    return self;
}
*/

- (LogData *)init
{
    [Exceptions raise:@"wrong initialization of LogData"];
    return self;
}






@synthesize filename;
@synthesize vProcs;

- (uint64_t)start
{
    return start;
}
- (uint64_t)firstTime
{
    return firstTime;
}
- (uint64_t)lastTime
{
    return lastTime;
}


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
    /*
    NSMutableString *ret = [NSMutableString stringWithFormat:
	@"<<< LogFile object: version (%d,%d,%d), CPUs: %d, nVProcs: %d, bufSize %d, headerSize %d\n \tvProcs:\n", self.majorVersion, self.minorVersion, self.patchVersion,
	  self.nCPUs, self.nVProcs, self.bufSzB, self.hdrSzB];
    NSLog(ret);
    for (VProc *vp in vProcs)
    {
	[ret appendString:@"\t\t"];
	[ret appendString:[vp description]];
    }
    [ret appendString:@" >>>\n"];
    return ret;
     */
}

  


/*
#pragma mark Filtering

BOOL containsEventDescAndIsDisabled(ObjCGroup *g, EventDesc *eventDesc)
{
    if (g.cppGroup->containsEvent(eventDesc) && (g.enabled.intValue == 0))
	return YES;
    else
    {
	if (g.kind != EVENT_GROUP)
	    return NO; free
	else
	{
	    InternalGroup *G = (InternalGroup *)g;
	    for (int i = 0; i < G.numKids; ++i)
	    {
		if (containsEventDescAndIsDisabled([G kid:i], eventDesc))
		    return YES;
	    }
	    return NO;
	}
    }
}
*/


#pragma mark Testing
- (IBAction)test:(id)sender
{

}

/*
+ (NSArray *)readableTypes
{
    return [NSArray arrayWithObjects:@"mlg", nil];
}
 */




@end

