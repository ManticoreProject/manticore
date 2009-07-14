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


void fileError(void)
{
    exit(1);
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
- (LogFile *)initWithFilename:(NSString *)filenameVal andLogFileDesc:(struct LogFileDesc *)desc
{
    if (![super init])
	return nil;
    
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

    // get the file size
    off_t fileSize;
    {
	struct stat st;
	if (stat([filename cStringUsingEncoding:NSASCIIStringEncoding], &st) < 0)
	    @throw @"Could not stat the given file";
	fileSize = st.st_size;
    }

    // XXX Why can't the log->next field be used here??
    // And is this - 1 correct?  What if LogBufSzB % sizeof(LogEvent_t) == 0 ?
    int NEventsPerBuf = (LogBufSzB / sizeof(LogEvent_t)) - 1;
    int numBufs = (fileSize / LogBufSzB) - 1;
    if (numBufs <= 0)
	@throw @"There are no buffers in the logfile";
    int MaxNumEvents = NEventsPerBuf * numBufs;

    VProc *vProcs_c_array[header->nVProcs];  // To be converted into an NSMutableArray later
    for (int i = 0; i < header->nVProcs; ++i)
	vProcs_c_array[i] = NULL;

    vProcs = [[NSMutableArray alloc] initWithCapacity:header->nVProcs];

    /* read in the events */
    for (int i = 0;  i < numBufs;  i++) {
	VProc *cur_vProc;
	LogBuffer_t *log = (LogBuffer_t *) [[f readDataOfLength:LogBufSzB] bytes];
	assert (log->vpId < header->nVProcs);

	if (vProcs_c_array[log->vpId] == NULL)
	    cur_vProc = vProcs_c_array[log->vpId] = [[VProc alloc] initWithVpId:log->vpId];
	else
	    cur_vProc = vProcs_c_array[log->vpId];

	// XXX Necessary??? see the XXX above about log->next and NEventsPerBuf.
	if (log->next > NEventsPerBuf)
	    log->next = NEventsPerBuf;

	DynamicEvent (*events)[] = cur_vProc.events = (DynamicEvent (*)[]) malloc(log->next * sizeof(DynamicEvent));
	// XXX ? Is it correct to think that log->next can be used this way ?
	int numEvents = cur_vProc.numEvents = log->next;
	assert (numEvents < MaxNumEvents);
	for (int j = 0;  j < log->next;  j++) {

	    LogEvent_t *logEvent = &(log->log[j]);
	    DynamicEvent *dynamicEvent = &(*events)[j];

	    memcpy(&dynamicEvent->value, logEvent, sizeof(logEvent));
	    dynamicEvent->timestamp = GetTimestamp(&logEvent->timestamp, header);
	    dynamicEvent->desc  = desc->FindEventById(logEvent->event);
	    dynamicEvent->references.src = NULL; // FIXME references must be properly initialized

	}
    }

    // Check that vProcs_c_array is fully initialized
    for (int i = 0; i < header->nVProcs; ++i)
    {
	if (vProcs_c_array[i] == NULL)
	    @throw @"Did not find enough vProcs in the log file";
    }

    // Convert the vProcs_c_array into vProcs
    for (int i = 0; i < header->nVProcs; ++i)
	[vProcs addObject:vProcs_c_array[i]];

    return self;
}


- (LogFile *)initWithFilename:(NSString *)filenameVal
	 andEventDescFilename:(NSString *)eventDesc
	   andLogDescFilename:(NSString *)logDesc
{
    return [self initWithFilename:filenameVal
		   andLogFileDesc:LoadLogDesc([logDesc UTF8String], [eventDesc UTF8String])];
}



@synthesize filename;
@synthesize vProcs;


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

@end
