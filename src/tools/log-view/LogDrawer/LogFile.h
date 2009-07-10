/** LogFile.h
 * \author Korei Klein
 * \date 7/10/09
 */

#import <Cocoa/Cocoa.h>
#import "log-file.h"

/// Represents the data in a logfile
@interface LogFile : NSObject {
    NSString *filename; ///< Name of the represented log file
    LogFileHeader_t *header; ///< the header of the log file, as defined in log-file.h
    NSMutableArray *vProcs; ///< an array containing header.nVProcs VProcs

    // These variables are to provide more convinient representations of some
    // things already found in the header
    // They must therefore be properly initialized when file is read

    NSString *date; ///< Cache of header.date is string format as reported by ctime(3)
    NSString *clockName; ///< Cache of header.clockName;
}

/// Create a LogFile object from the fileName of the logfile it should represent
- (LogFile *)initWithLogFile:(NSString *)logFileName;

@property (readonly) NSString	*filename;
@property (readonly) NSMutableArray *vProcs;

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
@property (readonly) LogTS_t	startTime;	///< start time for run
// @property (readonly) char *		clockName;	///< a string describing the clock
@property (readonly) uint32_t	resolution;	///< clock resolution in nanoseconds
@property (readonly) uint32_t	nVProcs;	///< number of vprocs in system
@property (readonly) uint32_t	nCPUs;		///< number of CPUs in system

@end


