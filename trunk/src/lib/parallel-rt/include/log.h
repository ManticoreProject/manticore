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

extern void InitLogFile (const char *name, int nvps, int ncpus);
extern void InitLog (VProc_t *vp);
extern void SwapLogBuffers (VProc_t *vp, LogBuffer_t *curBuf);
extern void FinishLog ();

#endif /* !_LOG_H_ */
