/* inline-log.h
 *
 * Inline operations for recording log entries.
 */

#ifndef _INLINE_LOG_H_
#define _INLINE_LOG_H_

#include "log.h"
#include "atomic-ops.h"

// get the pointer to the next log entry
STATIC_INLINE LogEvent_t *NextLogEvent (VProc_t *vp)
{
    LogBuffer_t *buf;

    do {
	buf = vp->log;
	int index = FetchAndInc(&(buf->next));
	if (index < LOGBUF_SZ) {
	    return &(buf->log[index]);
	    break;
	}
	SwapLogBuffers (vp);
    } while (true);

}

STATIC_INLINE void LogTimestamp (LogEvent_t *ep)
{
#if HAVE_MACH_ABSOLUTE_TIME
    ep->timeStamp = mach_absolute_time();
#elif HAVE_CLOCK_GETTIME
    clock_gettime (CLOCK_REALTIME, &(ep->timestamp.ts_timespec));
#else
    gettimeofday (&(ep->timestamp.ts_timeval), 0);
#endif
}

STATIC_INLINE void LogEvent0 (VProc_t *vp, uint32_t evt)
{
    LogEvent_t *ep = NextLogEvent(vp);

    LogTimestamp (ep);
    ep->tid = tid;
    ep->event = evt;

}

STATIC_INLINE void LogEvent1 (VProc_t *vp, uint32_t evt, uint32_t a)
{
    LogEvent_t *ep = NextLogEvent(vp);

    LogTimestamp (ep);
    ep->tid = tid;
    ep->event = evt;
    ep->data[0] = a;

}

STATIC_INLINE void LogEvent2 (VProc_t *vp, uint32_t evt, uint32_t a, uint32_t b)
{
    LogEvent_t *ep = NextLogEvent(vp);

    LogTimestamp (ep);
    ep->tid = tid;
    ep->event = evt;
    ep->data[0] = a;
    ep->data[1] = b;

}

STATIC_INLINE void LogEvent3 (
    VProc_t *vp, uint32_t evt,
    uint32_t a, uint32_t b, uint32_t c)
{
    LogEvent_t *ep = NextLogEvent(vp);

    LogTimestamp (ep);
    ep->tid = tid;
    ep->event = evt;
    ep->data[0] = a;
    ep->data[1] = b;
    ep->data[2] = c;

}

STATIC_INLINE void LogEvent4 (
    VProc_t *vp, uint32_t evt,
    uint32_t a, uint32_t b, uint32_t c, uint32_t d)
{
    LogEvent_t *ep = NextLogEvent(vp);

    LogTimestamp (ep);
    ep->tid = tid;
    ep->event = evt;
    ep->data[0] = a;
    ep->data[1] = b;
    ep->data[2] = c;
    ep->data[3] = d;

}

STATIC_INLINE void LogEvent5 (
    VProc_t *vp, uint32_t evt,
    uint32_t a, uint32_t b, uint32_t c, uint32_t d, uint32_t e)
{
    LogEvent_t *ep = NextLogEvent(vp);

    LogTimestamp (ep);
    ep->tid = tid;
    ep->event = evt;
    ep->data[0] = a;
    ep->data[1] = b;
    ep->data[2] = c;
    ep->data[3] = d;
    ep->data[4] = e;

}

#endf /* !_INLINE_LOG_H_ */
