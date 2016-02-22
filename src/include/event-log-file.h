/* event-log-file.h
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

#ifndef _EVENT_LOG_FILE_H_
#define _EVENT_LOG_FILE_H_

#include "manticore-config.h"
#include <stdint.h>
#include <sys/time.h>
#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

#define LOGBLOCK_SZB	(8*1024)
    
typedef struct _EventsBuf{
    uint8_t * pos;
    uint8_t *marker;
    uint8_t * end;
    uint8_t begin[LOGBLOCK_SZB];
} EventsBuf;

/* define the predefined log-event codes */
#include "log-events.h"

#ifdef __cplusplus
}
#endif

#endif /* !_EVENT_LOG_FILE_H_ */
