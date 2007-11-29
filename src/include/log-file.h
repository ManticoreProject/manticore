/* log-file.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file describes the layout of log files and is used by both the runtime
 * to generate logs and by the 
 */

#ifndef _LOG_FILE_H_
#define _LOG_FILE_H_

#include "manticore-config.h"
#include <stdint.h>
#include <sys/time.h>
#include <time.h>

#ifndef HAVE_CLOCK_GETTIME
struct timespec {
    uint32_t		tv_sec;
    uint32_t		tv_usec;
};
#endif

#define LOGBLOCK_SZB	(8*1024)
#define LOGBUF_SZ	((BLOCK_SZB/sizeof(LogEvent_t))-1)
#define LOG_MAGIC	0x6D616E7469636F72	// "manticor"

enum {				    // different formats of timestamps, but they should
				    // all be 64-bit quantities.
    LOGTS_TIMEVAL,			// struct timeval returned by gettimeofday
    LOGTS_TIMESPEC,			// struct timespec returned by clock_gettime
    LOGTS_MACH_ABSOLUTE			// uint64_t returned by mach_absolute_time
};

typedef union {			    // union of timestamp reps.
    struct timespec	ts_timespec;	// LOGTS_TIMEVAL
    struct timeval	ts_timeval;	// LOGTS_TIMESPEC
    struct uint64_t	ts_mach;	// LOGTS_MACH_ABSOLUTE
} LogTS_t;

typedef struct {
    uint64_t		magic;		// to identify log files
    uint32_t		version;	// version stamp
    uint32_t		bufSzB;		// buffer size
    uint32_t		tsKind;		// timestamp format
    char		clockName[24];	// a string describing the clock
    uint32_t		resolution;	// clock resolution in nanoseconds
    uint32_t		nVProcs;	// number of vprocs in system
/* other stuff */
} LogFileHeader_t;

typedef struct {
    TimeStamp_t		timeStamp;	// time stamp
    uint32_t		tid;		// thread ID
    uint32_t		event;		// event code
    uint32_t		data[4];	// upto 16 bytes of extra data
} LogEvent_t;

typedef struct {
    uint32_t		vpId;		// ID of vproc that owns this buffer
    uint32_t		next;		// next entry to use in the log[] array
    char		pad[sizeof(LogEvent_t) - 8];
    LogEvent_t		log[LOGBUF_SZ];
} LogBuffer_t;

/* define the predefined log-event codes */
#define DEF_EVENT(NAME, SZ, DESC)	NAME,
enum {
#include "log-events.h"
};
#undef DEF_EVENT

#endif /* !_LOG_FILE_H_ */
