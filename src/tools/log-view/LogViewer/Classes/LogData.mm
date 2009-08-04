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


/* I do not know c++ !

class Visitor : public LogDescVisitor {
public:
    Visitor () { }
    void VisitGroup (EventGroup *) { }
    void VisitStateGroup (StateGroup *grp)
    {
    }
    void VisitIntervalGroup (IntervalGroup *grp) { }
    void VisitDependentGroup (DependentGroup *grp) { }
    NSMutableArray *Result() {
	for (int i = 0; i < res.size(); ++i)
	{
	    [a addObject:
	}
    }

private:
    std::vector<StateGroup *> *res;

}
*/


@implementation LogData

@synthesize desc;
@synthesize dependentMap;

- (NSArray *)dependentDetails
{
    return dependentMap.toArray;
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


- (LogData *)initWithFilename:(NSString *)filenameVal
	       andLogFileDesc:(struct LogFileDesc *)logDesc
{
    if (![super init]) return nil;
    filename = filenameVal;
    
    firstTime = -1;
    lastTime = 0;
    
    dependentMap = [[DependentMap alloc] init];
    
    allStates = [[NSMutableArray alloc] init];
    // XXX make this work !!!!!!!
    // Visitor visitor(allStates);
    // logDesc->PreOrderWalk(&visitor);
    
    int LogBufSzB = LOGBLOCK_SZB;
    
    NSFileHandle *f = [NSFileHandle fileHandleForReadingAtPath:filename];
    if (!f) [Exceptions raise:@"LogFile: file access error"];

    // read the header
    NSData *fileHeader = [f readDataOfLength:LOGBLOCK_SZB];
    // Protect this header, it could be garbage collected when fileHeader is.
    header = (LogFileHeader_t *) malloc(fileHeader.length);
    memcpy(header, fileHeader.bytes, fileHeader.length);

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

    /* read in the blocks */
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
	    return NO;
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
