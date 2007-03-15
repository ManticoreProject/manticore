/* scheduler.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_

#include "manticore-rt.h"

typedef struct {	      /* Items on the scheduler-action stack */
    Value_t	act;		/* the scheduler action continuation */
    Value_t	link;		/* points to the next item in the stack */
} SchedActStkItem_t;

#endif /* !_SCHEDULER_H_ */
