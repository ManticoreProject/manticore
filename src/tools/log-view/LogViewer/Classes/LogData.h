/** \file LogData.h
 * \author Korei Klein
 * \date 7/10/09

 Define the internal representation of a logfile.

 */

#import <Cocoa/Cocoa.h>
#import "VProcMaps.h"
struct LogFileDesc;

/// Represents the data in a logfile
/**
 This class will act as the file's owner in the Document Architucture.
 To Display the file, it will use a Logview.
 **/
@interface LogData : NSObject {
    NSString *filename; ///< Name of the represented log file
    struct struct_logfilehdr *header; ///< the header of the log file, as defined in log-file.h

    NSMutableArray *vProcs; ///< an array containing header.nVProcs VProcs sorted by vpId
    DependentMap *dependentMap;

    NSMutableArray *dependentDetails;

    struct LogFileDesc *desc; ///< The description of this file

#ifdef GO_WAY_SLOW_TO_CHECK_FOR_DUPLICATE_BLOCK_PROBLEM
    NSMutableArray *first_event_times;
#endif

    uint64_t start; ///< The start of the log, according to the log file
    uint64_t firstTime; ///< The time the first event in the log file was logged
    uint64_t lastTime; ///< The time the last event in the log file was logged

    NSString *date;
    NSString *clockName;

    NSMutableArray *allStates;

}

/// Main initialization method for LogData.
/**
 * This method reads data from the .mlg log-file filename whose
 * interpretation is given by logDesc and initializes self to be
 * a LogData object representing that data.
 *
 * This method is large.
 * As such, its implementation is broken into many passes, clearly delimited by comments.
 */
- (LogData *)initWithFilename:(NSString *)filename
	       andLogFileDesc:(struct LogFileDesc *)logDesc;


@property (readonly) uint64_t start;
@property (readonly) uint64_t firstTime;
@property (readonly) uint64_t lastTime;


@property (readonly) NSArray *allStates;
@property (readonly) NSArray *dependentDetails;
@property (readonly) struct LogFileDesc *desc;
@property (readonly) NSString	*filename;
@property (readonly) NSArray *vProcs;
@property (readonly) DependentMap *dependentMap;



#pragma mark Header Properties

@property (readonly) NSString *date;
@property (readonly) NSString *clockName;


@property (readonly) uint64_t	magic;		///< to identify log files
@property (readonly) uint32_t	majorVersion;   ///< version info
@property (readonly) uint32_t	minorVersion;   ///< version info
@property (readonly) uint32_t	patchVersion;   ///< version info
@property (readonly) uint32_t	hdrSzB;		///< size of the header struct
@property (readonly) uint32_t	bufSzB;		///< buffer size (usually == sizeof(struct_logbuf))
//@property (readonly) char *		date;		///< the date of the run (as reported by ctime(3))
@property (readonly) uint32_t	tsKind;		///< timestamp format
// @property (readonly) union _LogTS_t	startTime;	///< start time for run
// @property (readonly) char *		clockName;	///< a string describing the clock
@property (readonly) uint32_t	resolution;	///< clock resolution in nanoseconds
@property (readonly) uint32_t	nVProcs;	///< number of vprocs in system
@property (readonly) uint32_t	nCPUs;		///< number of CPUs in system

@end


