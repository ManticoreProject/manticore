/** \file LogFile.mm
 \author Korei Klein
 \date 7/10/09

 */



#import "LogFile.h"
#import "log-file.h"
#import "VProc.hxx"
#import <sys/stat.h>
#import "DynamicEventRep.hxx"
#import "log-desc.hxx"

// class LogFileDesc; // Why does this not work?

/// Load a log file into a LogFileDesc c++ class
extern LogFileDesc *LoadLogDesc(const char *, const char *);


@implementation LogFile

@synthesize desc;

void fileError(void)
{
    @throw @"LogFile: file access error";
}

/// convert a timestamp to nanoseconds
static inline uint64_t GetTimestamp (LogTS_t *ts, LogFileHeader_t *header)
{
    if (header->tsKind == LOGTS_MACH_ABSOLUTE)
	return ts->ts_mach;
    else if (header->tsKind == LOGTS_TIMESPEC)
	return ts->ts_val.sec * 1000000000 + ts->ts_val.frac;
    else // header->tsKind == LOGTS_TIMEVAL
	return ts->ts_val.sec * 1000000000 + ts->ts_val.frac * 1000;
}

/** This function's implementation is based heavily on that of LoadLogFile from
 * log-dump.cxx
 */
- (LogFile *)initWithFilename:(NSString *)filenameVal andLogFileDesc:(struct LogFileDesc *)descVal
{
    if (![super init])
	return nil;

    desc = descVal;
    filename = filenameVal;
    size_t LogBufSzB = LOGBLOCK_SZB;

    NSFileHandle *f = [NSFileHandle fileHandleForReadingAtPath:filename];
    if (!f)
	fileError();

    // read the header
    NSData *fileHeader = [f readDataOfLength:LOGBLOCK_SZB];
    header = (LogFileHeader_t *)[fileHeader bytes];

    // check the header
    if (header->magic != LOG_MAGIC)
	@throw @"bogus magic number";
    if (header->hdrSzB != sizeof(LogFileHeader_t))
	@throw @"bad header size";
    if (header->majorVersion != LOG_VERSION_MAJOR)
	@throw @"wrong version";
    if (header->bufSzB != LogBufSzB)
    {
	// recompute block size
	NSLog(@"using block size %d instead of %d", header->bufSzB, LogBufSzB);
	LogBufSzB = header->bufSzB;
	// reset input file pointer
	[f seekToFileOffset:LogBufSzB];
    }
    
    // We will continue to adjust lastTime and firstTime when we find new events
    lastTime = start = GetTimestamp(&header->startTime, header);
    firstTime = - 1;  //< The largest possible uint64_t
    
    // get the file size
    off_t fileSize;
    {
	struct stat st;
	if (stat([filename cStringUsingEncoding:NSASCIIStringEncoding], &st) < 0)
	    @throw @"LogFile: could not stat the given file";
	fileSize = st.st_size;
    }

    // -1 is to compensate for the fact that the header of the block takes up exactly one
    // LogEvent_t of data at the beggining of the block
    int NEventsPerBuf = (LogBufSzB / sizeof(LogEvent_t)) - 1;
    int numBufs = (fileSize / LogBufSzB) - 1;
    if (numBufs <= 0)
	@throw @"LogFile: There are no buffers in the logfile";
    // Maximum number of events in the entire log file
    int MaxNumEvents = NEventsPerBuf * numBufs;

    VProc *vProcs_c_array[header->nVProcs];  // To be converted into an NSMutableArray later
    for (int i = 0; i < header->nVProcs; ++i)
	vProcs_c_array[i] = NULL;

    /* read in the events */
    for (int i = 0;  i < numBufs;  i++) {
	VProc *cur_vProc;
	LogBuffer_t *log = (LogBuffer_t *) [[f readDataOfLength:LogBufSzB] bytes];
	assert (log->vpId < header->nVProcs);

	// log->next has and only has exactly one of these two properties:
	// 1.  log->next is equal to the number of events in this block
	// 2.  log->next is greater than the number of events in this block, and this block
	//	contains the maximum number of events that will fit into a block.
	if (log->next > NEventsPerBuf)
	    log->next = NEventsPerBuf;	
	DynamicEvent (*events)[];
	// log->next is now equal to the number of events in this block of the log
	int numEventsInBlock = log->next;
	assert (numEventsInBlock < MaxNumEvents);
	
	// This will be initialized to the index of the first spot
	// in the events array which does not have an event in it
	int firstFreeEvent;
	
	if (vProcs_c_array[log->vpId] == NULL)
	{
	    // This vProc does not yet exist, create it, and initialize it for this block
	    cur_vProc = vProcs_c_array[log->vpId] = [[VProc alloc] initWithVpId:log->vpId];
	    
	    firstFreeEvent = 0;
	    events = cur_vProc.events =
		(DynamicEvent (*)[]) malloc(numEventsInBlock * sizeof(DynamicEvent));
	    cur_vProc.numEvents = numEventsInBlock;
	}
	else {
	    // This vProc exists, but its events, and numEvents fields must
	    // be modified to accomodate the block we are now reading in
	    cur_vProc = vProcs_c_array[log->vpId];
	    firstFreeEvent = cur_vProc.numEvents;
	    cur_vProc.numEvents += numEventsInBlock;
	    events = cur_vProc.events = (DynamicEvent (*)[])
	      realloc(cur_vProc.events, sizeof(DynamicEvent) * cur_vProc.numEvents);
	}

	// Add each event to the events array
	for (int j = 0;  j < numEventsInBlock;  j++) {

	    LogEvent_t *logEvent = &(log->log[j]);
	    DynamicEvent *dynamicEvent = &(*events)[j + firstFreeEvent];

	    memcpy(&dynamicEvent->value, logEvent, sizeof(logEvent));
	    dynamicEvent->timestamp = GetTimestamp(&logEvent->timestamp, header);
	    dynamicEvent->desc  = desc->FindEventById(logEvent->event);

	   // NSLog(@"Found event with description %s", dynamicEvent->desc->Description());
	    
	    if (dynamicEvent->timestamp > lastTime)
		lastTime = dynamicEvent->timestamp;
	    if (dynamicEvent->timestamp < firstTime)
	    {
		firstTime = dynamicEvent->timestamp;
		NSLog(@"LogFile set firstTime to %qu", firstTime);
	    }
	    // FIXME initialize references
	}
    }

    // Check that vProcs_c_array is fully initialized
    for (int i = 0; i < header->nVProcs; ++i)
    {
	if (vProcs_c_array[i] == NULL)
	    @throw @"Did not find enough vProcs in the log file";
    }

    vProcs = [[NSMutableArray alloc] initWithCapacity:header->nVProcs];

    // Convert the vProcs_c_array into vProcs
    for (int i = 0; i < header->nVProcs; ++i)
	[vProcs addObject:vProcs_c_array[i]];

    return self;
}


- (LogFile *)initWithFilename:(NSString *)filenameVal
	 andEventDescFilename:(NSString *)eventDesc
	   andLogDescFilename:(NSString *)logDesc
{
    LogFileDesc *descVal = LoadLogDesc([logDesc UTF8String], [eventDesc UTF8String]);
    if (descVal == NULL)
	@throw @"Could not load the two log description files";
    return [self initWithFilename:filenameVal
		   andLogFileDesc:descVal];
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
}


@end
