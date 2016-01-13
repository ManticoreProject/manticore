/* log.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 */

#if !defined(_LOG_H_) && defined(ENABLE_LOGGING)
#define _LOG_H_

#include "manticore-rt.h"
#include "log-file.h"
#include "event-log-file.h"

extern void InitLogFile (const char *name, int nvps, int ncpus);
extern void InitLog (VProc_t *vp);
extern void SwapLogBuffers (VProc_t *vp, LogBuffer_t *curBuf);
extern void FinishLog ();

extern void InitEventLogFile (const char *name, int nvps, int ncpus);
extern void InitEventLog (VProc_t *vp);
extern void SwapEventLogBuffers (VProc_t *vp, LogBuffer_t *curBuf);
extern void FinishEventLog ();
extern void printAndClearEventBuf (VProc_t * vp);
extern uint64_t get_elapsed_time();

#endif /* !_LOG_H_ */
