/* log-events.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * NOTE: this file may be included more than once.  Clients must define a macro
 *
 *	#define DEF_EVENT(NAME, SZ, DESC)
 *
 * where NAME is the name of the symbolic constant used for the event, SZ is the
 * number of 32-bit arguments to the event, and DESC is a string describing the
 * event.  The NAME should end in "Evt".
 */

#ifndef LOG_VERSION
#define LOG_VERSION	0x20071211	/* date of last change to this file */
#endif

DEF_EVENT(NoEvent,		0,	"an undefined event")

/* VProc events */
DEF_EVENT(VProcStartIdleEvt,	0,	"start idle vproc")
DEF_EVENT(VProcStartMainEvt,	0,	"start main vproc")
DEF_EVENT(VProcExitMainEvt,	0,	"exit main vproc")
DEF_EVENT(VProcSleepEvt,	0,	"vproc going to sleep")
DEF_EVENT(VProcDequeueEvt,	0,	"dequeue from secondary scheduling queue")
DEF_EVENT(PreemptSignalEvt,	0,	"preemption signal occurs")

/* GC events */
DEF_EVENT(MinorGCStartEvt,	0,	"minor GC starts")
DEF_EVENT(MinorGCEndEvt,	0,	"minor GC ends")
DEF_EVENT(MajorGCStartEvt,	0,	"major GC starts")
DEF_EVENT(MajorGCEndEvt,	0,	"major GC ends")
DEF_EVENT(GlobalGCInitEvt,	0,	"global GC initiated")
DEF_EVENT(GlobalGCVPStartEvt,	0,	"global GC starts for vproc")
DEF_EVENT(GlobalGCVPDoneEvt,	0,	"global GC ends for vproc")
DEF_EVENT(GlobalGCEndEvt,	0,	"global GC finished")
