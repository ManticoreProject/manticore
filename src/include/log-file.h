/* log-file.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file describes the layout of log files and is used by both the runtime
 * to generate logs and by tools used to analyse log files.
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

typedef struct {
    uint64_t		magic;		// to identify log files
    uint32_t		version;	// version stamp
    uint32_t		bufSzB;		// buffer size
    LogTS_t		startTime;	// start time for run
    uint32_t		tsKind;		// timestamp format
    char		clockName[32];	// a string describing the clock
    uint32_t		resolution;	// clock resolution in nanoseconds
    uint32_t		nVProcs;	// number of vprocs in system
    uint32_t		nCPUs;		// number of CPUs in system
/* other stuff */
} LogFileHeader_t;

typedef struct {
    LogTS_t		timestamp;	// time stamp (8 bytes)
    uint32_t		event;		// event code
    uint32_t		data[5];	// upto 20 bytes of extra data
} LogEvent_t;

struct struct_logbuf {
    int32_t		vpId;		// ID of vproc that owns this buffer
    int32_t		next;		// next entry to use in the log[] array
    char		pad[sizeof(LogEvent_t) - 8];
    LogEvent_t		log[LOGBUF_SZ];
};

/* define the predefined log-event codes */
#include "log-events.h"

#ifdef __cplusplus
}
#endif

#endif /* !_LOG_FILE_H_ */
