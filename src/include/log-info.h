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

#endif /* !_LOG_INFO_H_ */
