/* log-file.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file describes the layout of log files and is used by both the runtime
 * to generate logs and by tools used to analyse log files.
 *
 * NOTE: programs that read log files should be able to handle files that use
 * different values of the bufSzB.  In the future, this feature will allow us
 * to tailor this parameter for the least overhead on different systems.
 */

#ifndef _LOG_FILE_H_
#define _LOG_FILE_H_

#include "manticore-config.h"
#include <stdint.h>
#include <sys/time.h>
#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

/* a 64-bit time value represented as seconds and fraction pair */
typedef struct {
    uint32_t		sec;	/* whole seconds */
    uint32_t		frac;	/* fractional seconds (either uSec or nSec) */
} TimeValue_t;

#define LOGBLOCK_SZB	(8*1024)
#define LOGBUF_SZ	((LOGBLOCK_SZB/sizeof(LogEvent_t))-1)
#define LOG_MAGIC	0x6D616E7469636F72ll	// "manticor"
#define DFLT_LOG_FILE	"LOG"

enum {				    // different formats of timestamps.
    LOGTS_TIMEVAL,			// struct timeval returned by gettimeofday
    LOGTS_TIMESPEC,			// struct timespec returned by clock_gettime
    LOGTS_MACH_ABSOLUTE			// uint64_t returned by mach_absolute_time
};

typedef union {			    // union of timestamp reps.
    TimeValue_t		ts_val;		// either LOGTS_TIMEVAL or LOGTS_TIMESPEC
    uint64_t		ts_mach;	// LOGTS_MACH_ABSOLUTE
} LogTS_t;

/* WARNING: the following struct needs to have the same layout in 64-bit
 * and 32-bit builds, so be careful about alignment and make sure that
 * the total size is a multiple of 8.
 */
typedef struct struct_logfilehdr {
    uint64_t		magic;		// to identify log files
    uint32_t		majorVersion;
    uint32_t		minorVersion;
    uint32_t		patchVersion;
    uint32_t		hdrSzB;		// size of this struct
    uint32_t		bufSzB;		// buffer size (usually == sizeof(struct_logbuf))
    char		date[32];	// the date of the run (as reported by ctime(3))
    uint32_t		tsKind;		// timestamp format
    LogTS_t		startTime;	// start time for run
    char		clockName[32];	// a string describing the clock
    uint32_t		resolution;	// clock resolution in nanoseconds
    uint32_t		nVProcs;	// number of vprocs in system
    uint32_t		nCPUs;		// number of CPUs in system
    uint32_t		_pad;		// padding
/* other stuff */
} LogFileHeader_t;

typedef struct struct_log_event {
    LogTS_t		timestamp;	// time stamp (8 bytes)
    uint32_t		event;		// event code
    uint32_t		data[5];	// upto 20 bytes of extra data
} LogEvent_t;

struct struct_logbuf {
    int32_t		vpId;		// ID of vproc that owns this buffer
    int32_t		next;		// next entry to use in the log[] array
    int32_t		seqNum;		// sequence number of this buffer
    char		pad[sizeof(LogEvent_t) - 12];
    LogEvent_t		log[LOGBUF_SZ];
};

#ifndef _MANTICORE_RT_H_
typedef struct struct_logbuf LogBuffer_t;
#endif

/* define the predefined log-event codes */
#include "log-events.h"

#ifdef __cplusplus
}
#endif

#endif /* !_LOG_FILE_H_ */
