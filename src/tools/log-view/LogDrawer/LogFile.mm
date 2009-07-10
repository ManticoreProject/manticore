//
//  LogFile.h
//  LogDrawer
//
//  Created by Korei Klein on 7/10/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "log-file.h"


@interface LogFile : NSObject {
    NSString *filename; ///< Name of the represented log file
    LogFileHeader header; ///< the header of the log file, as defined in log-file.h
    NSMutableArray *vProcs; ///< an array containing header.nVProcs VProcs
}

@property (read) NSString	*filename;
@property (read) NSMutableArray *vProcs;


@property (read) uint64_t	magic;		///< to identify log files
@property (read) uint32_t	majorVersion;
@property (read) uint32_t	minorVersion;
@property (read) uint32_t	patchVersion;
@property (read) uint32_t	hdrSzB;		///< size of the header struct
@property (read) uint32_t	bufSzB;		///< buffer size (usually == sizeof(struct_logbuf))
@property (read) char *		date;		///< the date of the run (as reported by ctime(3))
@property (read) uint32_t	tsKind;		///< timestamp format
@property (read) LogTS_t	startTime;	///< start time for run
@property (read) char *		clockName;	///< a string describing the clock
@property (read) uint32_t	resolution;	///< clock resolution in nanoseconds
@property (read) uint32_t	nVProcs;	///< number of vprocs in system
@property (read) uint32_t	nCPUs;		///< number of CPUs in system

@end



@implementation LogFile



- (uint64_t)magic
{
    return magic;
}
- (uint32_t)majorVersion
{
    return majorVerson;
    - (uint32_t)minorVersion
    {
	return minorVersion;
    }
    - (uint32_t)patchVersion
    {
	return patchVersion;
    }
    - (uint32_t)hdrSzB
    {
	return hdrSzB;
    }
    - (uint32_t)bufSzB
    {
	return bufSzB
    }
    - (char *)date
    {
	return date;
    }
    - (uint32_t)tsKind
    {
	return tsKind;
    }
    - (LogTS_t)startTime
    {
	return startTime;
    }
    - (char *)clockName
    {
	return clockName;
    }
    - (uint32_t)resolution
    {
	return resolution;
    }
    - (uint32_t)nVProcs
    {
	return nVProcs;
    }
    - (uint32_t)nCPUs
    {
	return nCPUs;
    }
    
    @end
