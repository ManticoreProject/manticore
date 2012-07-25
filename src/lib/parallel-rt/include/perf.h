/* perf.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 */

#if !defined(_PERF_H_) && defined(ENABLE_PERF_COUNTERS)
#define _PERF_H_

#include "manticore-rt.h"
#if defined(TARGET_LINUX)
#  include "linux/perf-counter.h"
#endif

extern void ParsePerfOptions (Options_t *opts);
extern void InitPerfCounters (VProc_t *vp);
extern void ReportPerfCounters ();

extern void PERF_StartGC(PerfCntrs_t *p);
extern void PERF_StopGC(PerfCntrs_t *p);
#endif /* !_PERF_H_ */
