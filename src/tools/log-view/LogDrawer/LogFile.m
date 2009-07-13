/** \file LogFile.m
 \author Korei Klein
 \date 7/10/09

 */



#import "LogFile.h"
#import "log-file.h"

@implementation LogFile


void fileError(void)
{

}

/** This function's implementation is based heavily on that of LoadLogFile from
 * log-dump.cxx
 */
- (LogFile *)initWithLogFile:(NSString *)logFileName
{
    size_t LogBufSzB = LOGBLOCK_SZB;
    if (![super init])
	return nil;
    NSFileHandle *f = [NSFileHandle fileHandleForReadingAtPath:logFileName];
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
	@throw @"unimplemented";

    }
    // int NEventsPerBuf = (LogBufSzB / sizeof(LogEvent_t)) - 1;



    return self;
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
- (LogTS_t)startTime
{
    return header->startTime;
}
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
