/* -----------------------------------------------------------------------------
 *
 * Support for fast binary event logging in the style of GHC eventlog.
 *
 * ---------------------------------------------------------------------------*/

#include "eventlog.h"
#include <string.h>
#include <stdio.h>
#include "manticore-rt.h"
#include "vproc.h"
#include "timer.h"

static char *event_log_filename = NULL;

// File for logging events
FILE *event_log_file = NULL;

#define EVENT_LOG_SIZE 2 * (1024 * 1024) // 2MB

static int flushCount;

// Struct for record keeping of buffer to store event types and events.
typedef struct _EventsBuf {
  signed char *begin;
  signed char *pos;
  signed char *marker;
  unsigned long size;
  unsigned short capno; // which capability this buffer belongs to, or -1
} EventsBuf;

EventsBuf *capEventBuf; // one EventsBuf for each Capability

EventsBuf eventBuf; // an EventsBuf not associated with any Capability
#ifdef THREADED_RTS
Mutex eventBufMutex; // protected by this mutex
#endif

char *EventDesc[]              = {
  [EVENT_GC_START]             = "Starting GC",
  [EVENT_GC_END]               = "Finished GC",
  [EVENT_START_TX]             = "Start Transaction",
  [EVENT_EAGER_PARTIAL_ABORT]  = "Eager partial abort for transaction",
  [EVENT_EAGER_FULL_ABORT]     = "Eager full abort for transaction",
  [EVENT_COMMIT_PARTIAL_ABORT] = "Commit time partial abort for transaction",
  [EVENT_COMMIT_FULL_ABORT]    = "Commit time full abort for transaction",
  [EVENT_COMMIT_TX]            = "Commit transaction",
  [EVENT_START_TX_WITH_INFO]   = "Start transaction with identifying information",
  [EVENT_BEGIN_COMMIT]         = "Begin Validation",
  [EVENT_STARTUP]              = "Create capabilities",
  [EVENT_BLOCK_MARKER]         = "Event block marker",
  [EVENT_RUN_THREAD]           = "Thread began running",
  [EVENT_STOP_THREAD]          = "Thread stopped running",
  [EVENT_MAJOR_GC]	       = "Major GC",
  [EVENT_GLOBAL_GC]	       = "Global GC",
  [EVENT_FAST_FORWARD]	       = "Fast Forward",
  [EVENT_REMEMBER_OBJ]	       = "Remember heap object",
  [EVENT_TS_EXTENSION]	       = "Timestamp Extension",
};

// Event type.

typedef struct _EventType {
  EventTypeNum etNum;  // Event Type number.
  nat   size;     // size of the payload in bytes
  char *desc;     // Description
} EventType;

EventType eventTypes[NUM_GHC_EVENT_TAGS];

static void initEventsBuf(EventsBuf* eb, StgWord64 size, EventCapNo capno);
static void resetEventsBuf(EventsBuf* eb);
static void printAndClearEventBuf (EventsBuf *eventsBuf);

static void postEventType(EventsBuf *eb, EventType *et);

static void postBlockMarker(EventsBuf *eb);
static void closeBlockMarker(EventsBuf *ebuf);

static StgBool hasRoomForEvent(EventsBuf *eb, EventTypeNum eNum);
static StgBool hasRoomForVariableEvent(EventsBuf *eb, nat payload_bytes);

static inline void postWord8(EventsBuf *eb, StgWord8 i)
{
    *(eb->pos++) = i;
}

static inline void postWord16(EventsBuf *eb, StgWord16 i)
{
    postWord8(eb, (StgWord8)(i >> 8));
    postWord8(eb, (StgWord8)i);
}

static inline void postWord32(EventsBuf *eb, StgWord32 i)
{
    postWord16(eb, (StgWord16)(i >> 16));
    postWord16(eb, (StgWord16)i);
}

static inline void postWord64(EventsBuf *eb, StgWord64 i)
{
    postWord32(eb, (StgWord32)(i >> 32));
    postWord32(eb, (StgWord32)i);
}

static inline void postBuf(EventsBuf *eb, StgWord8 *buf, nat size)
{
    memcpy(eb->pos, buf, size);
    eb->pos += size;
}

StgWord64 initTime = 0;

inline StgWord64 time_ns(void)
{ return TIMER_Now() - initTime; }

static inline void postEventTypeNum(EventsBuf *eb, EventTypeNum etNum)
{ postWord16(eb, etNum); }

static inline void postTimestamp(EventsBuf *eb)
{ postWord64(eb, time_ns()); }

static inline void postThreadID(EventsBuf *eb, EventThreadID id)
{ postWord32(eb,id); }

static inline void postCapNo(EventsBuf *eb, EventCapNo no)
{ postWord16(eb,no); }

static inline void postPayloadSize(EventsBuf *eb, EventPayloadSize size)
{ postWord16(eb,size); }

static inline void postEventHeader(EventsBuf *eb, EventTypeNum type)
{
    postEventTypeNum(eb, type);
    postTimestamp(eb);
}

static inline void postInt8(EventsBuf *eb, StgInt8 i)
{ postWord8(eb, (StgWord8)i); }

static inline void postInt32(EventsBuf *eb, StgInt32 i)
{ postWord32(eb, (StgWord32)i); }


void initEventLogging(const char * filename){

    initTime = TIMER_Now();
    
    StgWord8 t, c;
    char *prog;

    event_log_filename = filename;
    
    /* Open event log file for writing. */
    if ((event_log_file = fopen(event_log_filename, "wb")) == NULL) {
        Die("initEventLogging: can't open %s", event_log_filename);
    }

    /*
     * Allocate buffer(s) to store events.
     * Create buffer large enough for the header begin marker, all event
     * types, and header end marker to prevent checking if buffer has room
     * for each of these steps, and remove the need to flush the buffer to
     * disk during initialization.
     *
     * Use a single buffer to store the header with event types, then flush
     * the buffer so all buffers are empty for writing events.
     */    
    moreCapEventBufs(0,NumVProcs);

    initEventsBuf(&eventBuf, EVENT_LOG_SIZE, (EventCapNo)(-1));

    // Write in buffer: the header begin marker.
    postInt32(&eventBuf, EVENT_HEADER_BEGIN);

    // Mark beginning of event types in the header.
    postInt32(&eventBuf, EVENT_HET_BEGIN);
    for (t = 0; t < NUM_GHC_EVENT_TAGS; ++t) {

        eventTypes[t].etNum = t;
        eventTypes[t].desc = EventDesc[t];

        switch (t) {
        case EVENT_GC_START:        // (cap)
        case EVENT_GC_END:          // (cap)
            eventTypes[t].size = 0;
            break;

	case EVENT_COMMIT_PARTIAL_ABORT:
	case EVENT_EAGER_PARTIAL_ABORT:
	case EVENT_EAGER_FULL_ABORT:
        case EVENT_COMMIT_FULL_ABORT:
        case EVENT_FAST_FORWARD:
	    eventTypes[t].size = sizeof(StgWord32);
	    break;

	case EVENT_TS_EXTENSION:
        case EVENT_START_TX:
        case EVENT_COMMIT_TX:
        case EVENT_BEGIN_COMMIT:
	
	case EVENT_MAJOR_GC:
	case EVENT_GLOBAL_GC:
            eventTypes[t].size = 0;
            break;

        case EVENT_START_TX_WITH_INFO:
            eventTypes[t].size = sizeof(StgWord64);
            break;
	    
	case EVENT_STARTUP:         // (cap_count)
            eventTypes[t].size = sizeof(EventCapNo);
            break;
	    
	case EVENT_BLOCK_MARKER:
	    eventTypes[t].size = sizeof(StgWord32) + sizeof(EventTimestamp) +
                sizeof(EventCapNo);
            break;


        case EVENT_STOP_THREAD:     // (cap, thread, status)
            eventTypes[t].size = sizeof(EventThreadID)
                               + sizeof(StgWord16)
                               + sizeof(EventThreadID);
            break;

	case EVENT_RUN_THREAD:      // (cap, thread)
            eventTypes[t].size = sizeof(EventThreadID);
            break;

	case EVENT_REMEMBER_OBJ: //address of object remembered
	    eventTypes[t].size = sizeof(StgWord64);
	    break;
	    
        default:
            continue; /* ignore deprecated events */
        }

        // Write in buffer: the start event type.
        postEventType(&eventBuf, &eventTypes[t]);
    }

    // Mark end of event types in the header.
    postInt32(&eventBuf, EVENT_HET_END);

    // Write in buffer: the header end marker.
    postInt32(&eventBuf, EVENT_HEADER_END);

    // Prepare event buffer for events (data).
    postInt32(&eventBuf, EVENT_DATA_BEGIN);

    // Flush capEventBuf with header.
    /*
     * Flush header and data begin marker to the file, thus preparing the
     * file to have events written to it.
     */
    printAndClearEventBuf(&eventBuf);

    for (c = 0; c < NumVProcs; ++c) {
        postBlockMarker(&capEventBuf[c]);
    }

#ifdef THREADED_RTS
    initMutex(&eventBufMutex);
#endif
}

void
endEventLogging(void)
{
    nat c;

    // Flush all events remaining in the buffers.
    for (c = 0; c < NumVProcs; ++c) {
        printAndClearEventBuf(&capEventBuf[c]);
    }
    printAndClearEventBuf(&eventBuf);
    resetEventsBuf(&eventBuf); // we don't want the block marker

    // Mark end of events (data).
    postEventTypeNum(&eventBuf, EVENT_DATA_END);

    // Flush the end of data marker.
    printAndClearEventBuf(&eventBuf);

    if (event_log_file != NULL) {
        fclose(event_log_file);
    }
}

void
moreCapEventBufs (nat from, nat to)
{
    nat c;

    capEventBuf = malloc(to * sizeof(EventsBuf));

    for (c = from; c < to; ++c) {
        initEventsBuf(&capEventBuf[c], EVENT_LOG_SIZE, c);
    }

    // The from == 0 already covered in initEventLogging, so we are interested
    // only in case when we are increasing capabilities number
    if (from > 0) {
        for (c = from; c < to; ++c) {
           postBlockMarker(&capEventBuf[c]);
        }
    }
}

/*
 * Post an event message to the capability's eventlog buffer.
 * If the buffer is full, prints out the buffer and clears it.
 */
void postSchedEvent (VProc_t *vp,
		     EventTypeNum tag,
		     unsigned long thread)
{
    EventsBuf *eb;

    eb = &capEventBuf[vp->id];

    if (!hasRoomForEvent(eb, tag)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }

    postEventHeader(eb, tag);

    switch (tag) {
    case EVENT_RUN_THREAD:      // (cap, thread)
    {
        postThreadID(eb,thread);
        break;
    }

    case EVENT_STOP_THREAD:     // (cap, thread, status)
    {
        postThreadID(eb,thread);
        postWord16(eb,0 /* status */);
        postThreadID(eb,thread /* blocked on thread */);
        break;
    }

    default:
        Die("postSchedEvent: unknown event tag %d", tag);
    }
}

void
freeEventLogging(void)
{
    StgWord8 c;

    // Free events buffer.
    for (c = 0; c < NumVProcs; ++c) {
        if (capEventBuf[c].begin != NULL)
            free(capEventBuf[c].begin);
    }
    if (capEventBuf != NULL)  {
        free(capEventBuf);
    }
}

void
flushEventLog(void)
{
    if (event_log_file != NULL) {
        fflush(event_log_file);
    }
}

void
abortEventLogging(void)
{
    freeEventLogging();
    if (event_log_file != NULL) {
        fclose(event_log_file);
    }
}



void postStartTX(VProc_t * vp, unsigned long event){
    EventsBuf * eb = &capEventBuf[vp->id];

    if (!hasRoomForEvent(eb, EVENT_START_TX_WITH_INFO)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }

    postEventHeader(eb, EVENT_START_TX_WITH_INFO);
    postWord64(eb, event);
}

void postRememberObj(VProc_t * vp, unsigned long addr){
    EventsBuf * eb = &capEventBuf[vp->id];
    
    if (!hasRoomForEvent(eb, EVENT_REMEMBER_OBJ)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }

    postEventHeader(eb, EVENT_REMEMBER_OBJ);
    postWord64(eb, addr);
}

void postAbortTX(VProc_t * vp, unsigned long abortInfo, EventTypeNum tag){
    EventsBuf * eb = &capEventBuf[vp->id];
    
    if (!hasRoomForEvent(eb, tag)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }

    postEventHeader(eb, tag);
    postWord32(eb, abortInfo);
}

void
postEvent (VProc_t * vp, EventTypeNum tag)
{
    EventsBuf *eb;

    eb = &capEventBuf[vp->id];

    if (!hasRoomForEvent(eb, tag)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }

    postEventHeader(eb, tag);
}


void closeBlockMarker (EventsBuf *ebuf)
{
    StgInt8* save_pos;

    if (ebuf->marker)
    {
        // (type:16, time:64, size:32, end_time:64)

        save_pos = ebuf->pos;
        ebuf->pos = ebuf->marker + sizeof(EventTypeNum) +
                    sizeof(EventTimestamp);
        postWord32(ebuf, save_pos - ebuf->marker);
        postTimestamp(ebuf);
        ebuf->pos = save_pos;
        ebuf->marker = NULL;
    }
}


void postBlockMarker (EventsBuf *eb)
{
    if (!hasRoomForEvent(eb, EVENT_BLOCK_MARKER)) {
        printAndClearEventBuf(eb);
    }

    closeBlockMarker(eb);

    eb->marker = eb->pos;
    postEventHeader(eb, EVENT_BLOCK_MARKER);
    postWord32(eb,0); // these get filled in later by closeBlockMarker();
    postWord64(eb,0);
    postCapNo(eb, eb->capno);
}

void printAndClearEventBuf (EventsBuf *ebuf)
{
    StgWord64 numBytes = 0, written = 0;

    closeBlockMarker(ebuf);

    if (ebuf->begin != NULL && ebuf->pos != ebuf->begin)
    {
        numBytes = ebuf->pos - ebuf->begin;

        written = fwrite(ebuf->begin, 1, numBytes, event_log_file);
        if (written != numBytes) {
            Die("printAndClearEventLog: fwrite() failed, written=%lu doesn't match numBytes=%lu", written, numBytes);
            return;
        }

        resetEventsBuf(ebuf);
        flushCount++;

        postBlockMarker(ebuf);
    }
}

void initEventsBuf(EventsBuf* eb, StgWord64 size, EventCapNo capno)
{
    eb->begin = eb->pos = malloc(size);
    eb->size = size;
    eb->marker = NULL;
    eb->capno = capno;
}

void resetEventsBuf(EventsBuf* eb)
{
    eb->pos = eb->begin;
    eb->marker = NULL;
}

StgBool hasRoomForEvent(EventsBuf *eb, EventTypeNum eNum)
{
  nat size;

  size = sizeof(EventTypeNum) + sizeof(EventTimestamp) + eventTypes[eNum].size;

  if (eb->pos + size > eb->begin + eb->size) {
      return 0; // Not enough space.
  } else  {
      return 1; // Buf has enough space for the event.
  }
}

StgBool hasRoomForVariableEvent(EventsBuf *eb, nat payload_bytes)
{
  nat size;

  size = sizeof(EventTypeNum) + sizeof(EventTimestamp) +
      sizeof(EventPayloadSize) + payload_bytes;

  if (eb->pos + size > eb->begin + eb->size) {
      return 0; // Not enough space.
  } else  {
      return 1; // Buf has enough space for the event.
  }
}

void postEventStartup(EventCapNo n_caps)
{
    if (!hasRoomForEvent(&eventBuf, EVENT_STARTUP)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(&eventBuf);
    }

    // Post a STARTUP event with the number of capabilities
    postEventHeader(&eventBuf, EVENT_STARTUP);
    postCapNo(&eventBuf, n_caps);
}

void postEventType(EventsBuf *eb, EventType *et)
{
    StgWord8 d;
    nat desclen;

    postInt32(eb, EVENT_ET_BEGIN);
    postEventTypeNum(eb, et->etNum);
    postWord16(eb, (StgWord16)et->size);
    desclen = strlen(et->desc);
    postWord32(eb, desclen);
    for (d = 0; d < desclen; ++d) {
        postInt8(eb, (StgInt8)et->desc[d]);
    }
    postWord32(eb, 0); // no extensions yet
    postInt32(eb, EVENT_ET_END);
}

