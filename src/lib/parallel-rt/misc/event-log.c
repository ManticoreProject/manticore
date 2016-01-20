/* event-log.c
 *
 * COPYRIGHT (c) 2016 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Event logging operations
 *
 * WARNING: this file is generated; do not edit!!!
 */

#include "manticore-rt.h"
#include <unistd.h>
#include <fcntl.h>
#include <strings.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#ifdef HAVE_MACH_ABSOLUTE_TIME
#  include <mach/mach_time.h>
#endif
#include "os-threads.h"
#include "vproc.h"
#include "log.h"

static int	LogFD = -1;
uint64_t start_ts;

#ifdef HAVE_MACH_ABSOLUTE_TIME
uint64_t timer_scaling_factor_numer = 1;
uint64_t timer_scaling_factor_denom = 1;
#endif

static char * EventDescArr [] = {
     [VProcStartIdleEvt] = "start idle vproc",
     [Startup] = "start main vproc",
     [VProcStartEvt] = "start a VProc",
     [VProcExitEvt] = "kill a VProc",
     [VProcExitMainEvt] = "exit main vproc",
     [VProcIdleEvt] = "vproc going idle",
     [VProcSleepEvt] = "vproc going to sleep",
     [VProcWakeupEvt] = "wakeup vproc from sleep/idle",
     [PreemptVProcEvt] = "time-slice preemption of a vproc",
     [PreemptSignalEvt] = "preemption signal received",
     [GCSignalEvt] = "global-GC signal received",
     [MinorGCStartEvt] = "minor GC starts",
     [MinorGCEndEvt] = "minor GC ends",
     [StartGC] = "Start GC (includes all three types of GCs)",
     [EndGC] = "End GC (includes all three types of GCs)",
     [MajorGCStartEvt] = "major GC starts",
     [MajorGCEndEvt] = "major GC ends",
     [RunThread] = "begin running thread",
     [StopThread] = "Stop running a thread",
     [GlobalGCInitEvt] = "global GC initiated",
     [GlobalGCEndEvt] = "global GC finished",
     [GlobalGCVPStartEvt] = "global GC starts for vproc",
     [GlobalGCVPDoneEvt] = "global GC ends for vproc",
     [PromoteStartEvt] = "object promotion starts",
     [PromoteEndEvt] = "object promotion ends",
     [ThdSpawnEvt] = "local thread spawned",
     [ThdSpawnOnEvt] = "remote thread spawned",
     [ThdStartEvt] = "thread body starts execution",
     [ThdExitEvt] = "thread exits",
     [MsgSendOfferedEvt] = "blocked message send operation",
     [MsgSendResumedEvt] = "resumed message send operation",
     [MsgRecvEvt] = "matching message receive operation",
     [MsgRecvOfferedEvt] = "blocked message receive operation",
     [MsgRecvResumedEvt] = "blocked message receive operation",
     [MsgSendEvt] = "matching message send operation",
     [WSInitEvt] = "work-stealing group initialization",
     [WSTerminateEvt] = "work-stealing group termination",
     [WSWorkerInitEvt] = "work-stealing worker initialization",
     [WSExecuteEvt] = "work stealing execution of work items",
     [WSPreemptedEvt] = "work-stealing worker has been preempted",
     [WSThiefSendEvt] = "work-stealing thief send",
     [WSThiefBeginEvt] = "work-stealing thief begins executing on the victim vproc",
     [WSThiefEndEvt] = "work-stealing thief ends executing on the victim vproc",
     [WSThiefSuccessfulEvt] = "work-stealing thief successfully stole a work item",
     [WSThiefUnsuccessfulEvt] = "work-stealing thief failed to steal a work item",
     [WSSleepEvt] = "work-stealing worker goes to sleep",
     [RopeRebalanceBeginEvt] = "A rope is being rebalanced.",
     [RopeRebalanceEndEvt] = "A rope is finished being rebalanced.",
     [EventBlock] = "A block of events associated with a single VProc",
};

static uint16_t EventSizesArr [] = {
    [VProcStartIdleEvt] = 0,
    [Startup] = 2 + 0,
    [VProcStartEvt] = 4 + 0,
    [VProcExitEvt] = 4 + 0,
    [VProcExitMainEvt] = 0,
    [VProcIdleEvt] = 0,
    [VProcSleepEvt] = 0,
    [VProcWakeupEvt] = 0,
    [PreemptVProcEvt] = 4 + 0,
    [PreemptSignalEvt] = 0,
    [GCSignalEvt] = 8 + 0,
    [MinorGCStartEvt] = 4 + 0,
    [MinorGCEndEvt] = 4 + 4 + 0,
    [StartGC] = 0,
    [EndGC] = 0,
    [MajorGCStartEvt] = 4 + 4 + 0,
    [MajorGCEndEvt] = 4 + 4 + 0,
    [RunThread] = 4 + 0,
    [StopThread] = 4 + 2 + 0,
    [GlobalGCInitEvt] = 4 + 8 + 0,
    [GlobalGCEndEvt] = 4 + 0,
    [GlobalGCVPStartEvt] = 0,
    [GlobalGCVPDoneEvt] = 4 + 0,
    [PromoteStartEvt] = 8 + 0,
    [PromoteEndEvt] = 4 + 0,
    [ThdSpawnEvt] = 8 + 0,
    [ThdSpawnOnEvt] = 8 + 0,
    [ThdStartEvt] = 8 + 0,
    [ThdExitEvt] = 0,
    [MsgSendOfferedEvt] = 8 + 0,
    [MsgSendResumedEvt] = 8 + 0,
    [MsgRecvEvt] = 8 + 0,
    [MsgRecvOfferedEvt] = 8 + 0,
    [MsgRecvResumedEvt] = 8 + 8 + 0,
    [MsgSendEvt] = 8 + 8 + 0,
    [WSInitEvt] = 8 + 0,
    [WSTerminateEvt] = 8 + 0,
    [WSWorkerInitEvt] = 8 + 8 + 0,
    [WSExecuteEvt] = 8 + 0,
    [WSPreemptedEvt] = 8 + 0,
    [WSThiefSendEvt] = 8 + 8 + 0,
    [WSThiefBeginEvt] = 8 + 8 + 0,
    [WSThiefEndEvt] = 8 + 8 + 0,
    [WSThiefSuccessfulEvt] = 8 + 8 + 0,
    [WSThiefUnsuccessfulEvt] = 8 + 8 + 0,
    [WSSleepEvt] = 8 + 0,
    [RopeRebalanceBeginEvt] = 4 + 0,
    [RopeRebalanceEndEvt] = 4 + 0,
    [EventBlock] = 8 + 2 + 4 + 0,
};

STATIC_INLINE void postWord8(EventsBuf *eb, uint8_t i)
{
    *(eb->pos++) = i;
}

STATIC_INLINE void postWord16(EventsBuf *eb, uint16_t i)
{
    postWord8(eb, (uint8_t)(i >> 8));
    postWord8(eb, (uint8_t)i);
}

STATIC_INLINE void postWord32(EventsBuf *eb, uint32_t i)
{
    postWord16(eb, (uint16_t)(i >> 16));
    postWord16(eb, (uint16_t)i);
}

STATIC_INLINE void postWord64(EventsBuf *eb, uint64_t i)
{
    postWord32(eb, (uint32_t)(i >> 32));
    postWord32(eb, (uint32_t)i);
}	      

void sanity(EventsBuf * eb, VProc_t * vp){
    int i = 0;
    int bytes = 0;
    uint8_t * ptr = eb->begin;
    uint16_t tag = ((uint16_t)ptr[0] << 8) | ptr[1];
    if(tag != EventBlock){
        if(tag == 26724 || tag == 65535){
            return; //tag is beginning of header, or end of data tag
        }
        printf("Warning: no event block at beginning\n");
    }else{
        ptr += 10 + EventSizesArr[EventBlock];
        i++;
        bytes += 10 + EventSizesArr[EventBlock];
    }

    while(ptr < eb->pos){
        uint16_t tag = ((uint16_t)ptr[0] << 8) | ptr[1];
        if(tag == 0 || tag > EventBlock){
            printf("event out of range! (%hu)\n", tag);
            return;
        }
        ptr += 10 + EventSizesArr[tag];  //go past tag and timestamp
        i++;
        bytes += 10 + EventSizesArr[tag];
    }
}

STATIC_INLINE void postEventType(EventsBuf *eb, uint16_t event_num){
     uint8_t d;
     postWord32(eb, EVENT_ET_BEGIN);
     postWord16(eb, event_num);
     postWord16(eb, EventSizesArr[event_num]);

     const char * desc = EventDescArr[event_num];
     int desclen = strlen(desc);
     postWord32(eb, desclen);
     for(d = 0; d < desclen; d++){
         postWord8(eb, desc[d]);	   
     }
     postWord32(eb, 0);
     postWord32(eb, EVENT_ET_END);
}

STATIC_INLINE void postEventTypes(EventsBuf * eb){
    postWord32(eb, EVENT_HEADER_BEGIN);
    postWord32(eb, EVENT_HET_BEGIN);
    
    uint32_t i;
    for(i = 1; i < NumLogEvents; i++){
	postEventType(eb, i);
    }
    
    postWord32(eb, EVENT_HET_END);
    postWord32(eb, EVENT_HEADER_END);

    postWord32(eb, EVENT_DATA_BEGIN);

}

STATIC_INLINE void ensureRoomForEvent(VProc_t * vp, uint32_t tag)
{
    EventsBuf *eb = vp->event_log;
    uint16_t size = EventSizesArr[tag] + 10;  //8 bytes for timestamp and 2 bytes for tag
    if(eb->pos + size > eb->end){ //not enough space
	printAndClearEventBuf(vp);
    }
}

/*! \brief set a timestamp field
 *  \param ts the address of the timestamp field
 */
STATIC_INLINE void LogTimestamp (EventsBuf * eb)
{
    uint64_t t = get_elapsed_time();
    postWord64(eb, t);
}

STATIC_INLINE void closeBlockMarker (EventsBuf *ebuf)
{
    uint8_t* save_pos;

    if (ebuf->marker)
    {
        // (type:16, time:64, size:32, end_time:64)

        save_pos = ebuf->pos;
        ebuf->pos = ebuf->marker + sizeof(uint16_t) +
                    sizeof(uint64_t);
        postWord32(ebuf, save_pos - ebuf->marker);
        LogTimestamp(ebuf);
        ebuf->pos = save_pos;
        ebuf->marker = NULL;
    }
}

STATIC_INLINE void postBlockMarker (VProc_t *vp)
{
    EventsBuf * eb = vp->event_log;

    ensureRoomForEvent(vp, EventBlock);

    closeBlockMarker(eb);

    eb->marker = eb->pos;
    postWord16(eb, EventBlock);
    LogTimestamp(eb);
    postWord32(eb,0); // these get filled in later by closeBlockMarker();
    postWord64(eb,0);
    postWord16(eb, vp->id);
}

/*! \brief generate a unique event ID
 *  \param vp the host vproc
 *  \return the new ID.
 */
STATIC_INLINE uint64_t NewEventId (VProc_t *vp)
{
    return vp->eventId++;
}


void LogEvent (VProc_t *vp, uint32_t evt)
{
    ensureRoomForEvent(vp, evt);
    postWord16(vp->event_log, (uint16_t) evt); 
    LogTimestamp (vp->event_log);

}

void LogEventi (VProc_t *vp, uint32_t evt, int32_t a0)
{
    ensureRoomForEvent(vp, evt);
    postWord16(vp->event_log, (uint16_t) evt); 
    LogTimestamp (vp->event_log);
    postWord32(vp->event_log, a0);

}

void LogEventu (VProc_t *vp, uint32_t evt, uint32_t a0)
{
    ensureRoomForEvent(vp, evt);
    postWord16(vp->event_log, (uint16_t) evt); 
    LogTimestamp (vp->event_log);
    postWord32(vp->event_log, a0);

}

uint64_t LogEventuN (VProc_t *vp, uint32_t evt, uint32_t a0)
{
    ensureRoomForEvent(vp, evt);
    postWord16(vp->event_log, (uint16_t) evt); 
    LogTimestamp (vp->event_log);
    postWord32(vp->event_log, a0);
    uint64_t newId = NewEventId(vp);
    postWord64(vp->event_log, newId);
    return newId;

}

void LogEventuu (VProc_t *vp, uint32_t evt, uint32_t a0, uint32_t a1)
{
    ensureRoomForEvent(vp, evt);
    postWord16(vp->event_log, (uint16_t) evt); 
    LogTimestamp (vp->event_log);
    postWord32(vp->event_log, a0);
    postWord32(vp->event_log, a1);

}

void LogEventW16 (VProc_t *vp, uint32_t evt, uint16_t a0)
{
    ensureRoomForEvent(vp, evt);
    postWord16(vp->event_log, (uint16_t) evt); 
    LogTimestamp (vp->event_log);
    postWord16(vp->event_log, a0);

}

void LogEventp4A (VProc_t *vp, uint32_t evt, void *a0)
{
    ensureRoomForEvent(vp, evt);
    postWord16(vp->event_log, (uint16_t) evt); 
    LogTimestamp (vp->event_log);
    postWord64(vp->event_log, (uint64_t)a0);

}

void LogEventp4I (VProc_t *vp, uint32_t evt, uint64_t a0)
{
    ensureRoomForEvent(vp, evt);
    postWord16(vp->event_log, (uint16_t) evt); 
    LogTimestamp (vp->event_log);
    postWord64(vp->event_log, (uint64_t)a0);

}

uint64_t LogEventp4N (VProc_t *vp, uint32_t evt)
{
    ensureRoomForEvent(vp, evt);
    postWord16(vp->event_log, (uint16_t) evt); 
    LogTimestamp (vp->event_log);
    uint64_t newId = NewEventId(vp);
    postWord64(vp->event_log, newId);
    return newId;

}

void LogEventiW16 (VProc_t *vp, uint32_t evt, int32_t a0, uint16_t a1)
{
    ensureRoomForEvent(vp, evt);
    postWord16(vp->event_log, (uint16_t) evt); 
    LogTimestamp (vp->event_log);
    postWord32(vp->event_log, a0);
    postWord16(vp->event_log, a1);

}

void LogEventp4II (VProc_t *vp, uint32_t evt, uint64_t a0, uint64_t a1)
{
    ensureRoomForEvent(vp, evt);
    postWord16(vp->event_log, (uint16_t) evt); 
    LogTimestamp (vp->event_log);
    postWord64(vp->event_log, (uint64_t)a0);
    postWord64(vp->event_log, (uint64_t)a1);

}

uint64_t LogEventp4NI (VProc_t *vp, uint32_t evt, uint64_t a1)
{
    ensureRoomForEvent(vp, evt);
    postWord16(vp->event_log, (uint16_t) evt); 
    LogTimestamp (vp->event_log);
    uint64_t newId = NewEventId(vp);
    postWord64(vp->event_log, newId);
    postWord64(vp->event_log, (uint64_t)a1);
    return newId;

}

void LogEventp4IW16p2u (VProc_t *vp, uint32_t evt, uint64_t a0, uint16_t a1, uint32_t a2)
{
    ensureRoomForEvent(vp, evt);
    postWord16(vp->event_log, (uint16_t) evt); 
    LogTimestamp (vp->event_log);
    postWord64(vp->event_log, (uint64_t)a0);
    postWord16(vp->event_log, a1);
    postWord32(vp->event_log, a2);

}








//Taken from GHC: https://github.com/ml9951/ghc/blob/pastm/rts/posix/GetTime.c#L86-L119
uint64_t getProcessElapsedTime(){
#if defined(HAVE_CLOCK_GETTIME)
    struct timespec ts;
    int res;

    res = clock_gettime(CLOCK_ID, &ts);
    if (res != 0) {
        Die("clock_gettime\n");
    }
    return (uint64_t)ts.tv_sec * 1000000000 + (uint64_t)ts.tv_nsec;

#elif defined(HAVE_MACH_ABSOLUTE_TIME)

    uint64_t time = mach_absolute_time();
    return (time * timer_scaling_factor_numer) / timer_scaling_factor_denom;

#else // use gettimeofday()

    struct timeval tv;
    gettimeofday(&tv, (struct timezone *) NULL);
    return (uint64_t)tv.tv_sec * 1000000000 +
           (uint64_t)tv.tv_usec * 1000;

#endif
}

uint64_t get_elapsed_time(){
    return getProcessElapsedTime() - start_ts;
}

void initElapsedTS(){
#ifdef HAVE_MACH_ABSOLUTE_TIME
    mach_timebase_info_data_t info;
    (void) mach_timebase_info(&info);
    timer_scaling_factor_numer = (uint64_t)info.numer;
    timer_scaling_factor_denom = (uint64_t)info.denom;
#endif
    start_ts = getProcessElapsedTime();
}

void printAndClearEventBuf (VProc_t * vp)
{
    EventsBuf * ebuf = vp->event_log;
    uint64_t numBytes = 0, written = 0;

    sanity(ebuf, vp);

    closeBlockMarker(vp->event_log);
    
    if (ebuf->begin != NULL && ebuf->pos != ebuf->begin)
    {
        numBytes = ebuf->pos - ebuf->begin;


	if(write(LogFD, ebuf->begin, numBytes) < 0){
	    Die("error in printAndClearEventBuf\n");
	}
	ebuf->pos = ebuf->begin;
	ebuf->marker = NULL;


        printf("[%d]: cleared %lu bytes\n", vp->id, numBytes);

	postBlockMarker(vp);
	
    }
}

/* InitLogFile:
 *
 * Initialize the log file.
 */
void InitEventLogFile (const char *name, int nvps, int ncpus)
{
    if ((LogFD = open(name, O_TRUNC|O_CREAT|O_WRONLY|O_APPEND, 0664)) < 0) {
	Die ("unable to open log file: %s", strerror(errno));
    }    
}

/* InitLog:
 *
 * Initialize the log buffers for the given vproc.
 */
void InitEventLog (VProc_t *vp)
{
    vp->event_log = NEW(EventsBuf);
    vp->event_log->pos = vp->event_log->begin;
    vp->event_log->end = vp->event_log->begin + LOGBLOCK_SZB;
    vp->event_log->marker = NULL;

    if(vp->id == 0){
	initElapsedTS();
	postEventTypes(vp->event_log);
	printAndClearEventBuf(vp);
    }else{
        postBlockMarker(vp);
    }
    
}

/* FinishLog:
 */
void FinishEventLog (VProc_t * main_vp)
{
    if (LogFD < 0){
        return;
    }

  /* flush out any remaining vproc buffers. */
    for (int i = 0;  i < NumVProcs;  i++) {
	VProc_t *vp = VProcs[i];
	printAndClearEventBuf (vp);
    }

    main_vp->event_log->pos = main_vp->event_log->begin;
    main_vp->event_log->marker = NULL;
    
    postWord16(main_vp->event_log, EVENT_DATA_END);
    printAndClearEventBuf(main_vp);

  /* close the file */
    close (LogFD);
    LogFD = -1;

} /* end of FinishLog */

