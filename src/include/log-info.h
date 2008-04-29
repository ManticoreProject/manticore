/* log-info.h
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file defines additional attributes of logging events that can be used
 * in the analysis of events.
 */

#ifndef _LOG_INFO_H_
#define _LOG_INFO_H_

#include "manticore-config.h"

/* Different kinds of event */
typedef enum {
    LOG_EVENT,		/* an independent event */
    LOG_START,		/* the start of an interval; the next event code will be the */
			/* end of the interval */
    LOG_END		/* the end of an interval; the previous event code will be the */
			/* start of the interval */
} LogEventKind_t;

/* Different formats of log messages.  A log message can have upto 20 bytes of
 * data; values can be 32-bit integers or floats and 64-bit integers or floats.
 * We also allow 32-bit unused values.  The 64-bit data values must be aligned
 * on 8-byte boundaries.  We represent the "type" signature of a log message as
 * a 6-character string (5 32-bit fields plus the '\0' terminator).  The field
 * values have the following interpretation:
 *
 *	u	-- unused (padding)
 *	i	-- 32-bit integer
 *	f	-- 32-bit float
 *	I	-- half of a 64-byte integer
 *	F	-- half of a 64-byte float
 */

typedef char LogMsgType_t[6];

typedef enum {
    LOG_DATA_UNUSED = 'u',
    LOG_DATA_INT32 = 'i',
    LOG_DATA_INT64 = 'I',
    LOG_DATA_FLOAT32 = 'f',
    LOG_DATA_FLOAT64 = 'F'
};

#endif /* !_LOG_INFO_H_ */
