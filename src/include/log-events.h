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
 * event.
 */

#ifndef LOG_VERSION
#define LOG_VERSION	0x20071128	/* date of last change to this file */
#endif

DEF_EVENT(NoEvent,		0,	"an undefined event")

/* Preemption events */
DEF_EVENT(PreemptSignal,	0,	"preemption signal occurs")

/* GC events */
DEF_EVENT(MinorGCStart,		0,	"minor GC starts")
DEF_EVENT(MinorGCEnd,		0,	"minor GC ends")
DEF_EVENT(MajorGCStart,		0,	"major GC starts")
DEF_EVENT(MajorGCEnd,		0,	"major GC ends")
