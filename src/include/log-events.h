/* log-events.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * NOTE: this file may be included more than once.  Clients must define a macro
 *
 *	#define DEF_EVENT(NAME, SZ, KIND, DESC)
 *
 * where NAME is the name of the symbolic constant used for the event, SZ is the
 * number of 32-bit arguments to the event, KIND is the kind of event (see log-info.h),
 * and DESC is a string describing the event.  The NAME should end in "Evt".
 */

#ifndef LOG_VERSION
#define LOG_VERSION	0x20071211	/* date of last change to this file */
#endif

DEF_EVENT(NoEvent,		0,	LOG_EVENT,  "an undefined event")

/* VProc events */
DEF_EVENT(VProcStartIdleEvt,	0,	LOG_EVENT,  "start idle vproc")
DEF_EVENT(VProcStartMainEvt,	0,	LOG_EVENT,  "start main vproc")
DEF_EVENT(VProcExitMainEvt,	0,	LOG_EVENT,  "exit main vproc")
DEF_EVENT(VProcSleepEvt,	0,	LOG_EVENT,  "vproc going to sleep")
DEF_EVENT(VProcDequeueEvt,	0,	LOG_EVENT,  "dequeue from secondary scheduling queue")
DEF_EVENT(PreemptSignalEvt,	0,	LOG_EVENT,  "preemption signal occurs")
DEF_EVENT(GCSignalEvt,		0,	LOG_EVENT,  "GC signal occurs")

/* GC events */
DEF_EVENT(MinorGCStartEvt,	0,	LOG_START,  "minor GC starts")
DEF_EVENT(MinorGCEndEvt,	0,	LOG_END,    "minor GC ends")
DEF_EVENT(MajorGCStartEvt,	0,	LOG_START,  "major GC starts")
DEF_EVENT(MajorGCEndEvt,	0,	LOG_END,    "major GC ends")
DEF_EVENT(GlobalGCInitEvt,	0,	LOG_START,  "global GC initiated")
DEF_EVENT(GlobalGCEndEvt,	0,	LOG_END,    "global GC finished")
DEF_EVENT(GlobalGCVPStartEvt,	0,	LOG_START,  "global GC starts for vproc")
DEF_EVENT(GlobalGCVPDoneEvt,	0,	LOG_END,    "global GC ends for vproc")

/* Runtime events */
DEF_EVENT(RTPreemptionEvt,      0,      LOG_EVENT,  "preemption handler")
DEF_EVENT(RTFuture1SpawnEvt,    2,      LOG_EVENT,  "spawned future1")
DEF_EVENT(RTFuture1TouchEvt,    2,      LOG_EVENT,  "touched future1")
DEF_EVENT(RTFuture1StealEvt,    2,      LOG_EVENT,  "stole future1")
