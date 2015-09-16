/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Event log format
 * 
 * The log format is designed to be extensible: old tools should be
 * able to parse (but not necessarily understand all of) new versions
 * of the format, and new tools will be able to understand old log
 * files.
 * 
 * Each event has a specific format.  If you add new events, give them
 * new numbers: we never re-use old event numbers.
 *
 * - The format is endian-independent: all values are represented in 
 *    bigendian order.
 *
 * - The format is extensible:
 *
 *    - The header describes each event type and its length.  Tools
 *      that don't recognise a particular event type can skip those events.
 *
 *    - There is room for extra information in the event type
 *      specification, which can be ignored by older tools.
 *
 *    - Events can have extra information added, but existing fields
 *      cannot be changed.  Tools should ignore extra fields at the
 *      end of the event record.
 *
 *    - Old event type ids are never re-used; just take a new identifier.
 *
 *
 * The format
 * ----------
 *
 * log : EVENT_HEADER_BEGIN
 *       EventType*
 *       EVENT_HEADER_END
 *       EVENT_DATA_BEGIN
 *       Event*
 *       EVENT_DATA_END
 *
 * EventType :
 *       EVENT_ET_BEGIN
 *       Word16         -- unique identifier for this event
 *       Int16          -- >=0  size of the event in bytes (minus the header)
 *                      -- -1   variable size
 *       Word32         -- length of the next field in bytes
 *       Word8*         -- string describing the event
 *       Word32         -- length of the next field in bytes
 *       Word8*         -- extra info (for future extensions)
 *       EVENT_ET_END
 *
 * Event : 
 *       Word16         -- event_type
 *       Word64         -- time (nanosecs)
 *       [Word16]       -- length of the rest (for variable-sized events only)
 *       ... extra event-specific info ...
 *
 *
 * To add a new event
 * ------------------
 *
 *  - In this file:
 *    - give it a new number, add a new #define EVENT_XXX below
 *  - In EventLog.c
 *    - add it to the EventDesc array
 *    - emit the event type in initEventLogging()
 *    - emit the new event in postEvent_()
 *    - generate the event itself by calling postEvent() somewhere
 *  - In the Haskell code to parse the event log file:
 *    - add types and code to read the new event
 *
 * -------------------------------------------------------------------------- */

#ifndef RTS_EVENTLOGFORMAT_H
#define RTS_EVENTLOGFORMAT_H

#include "vproc.h"

typedef signed   char            StgInt8;
typedef unsigned char            StgWord8;
typedef signed   short           StgInt16;
typedef unsigned short           StgWord16;
typedef signed   int             StgInt32;
typedef unsigned int             StgWord32;
typedef signed   long          StgInt64;
typedef unsigned long          StgWord64;
typedef unsigned long nat;
typedef int StgBool;
typedef StgWord32 EventThreadID;
typedef StgWord16 EventCapNo;
typedef StgWord16 EventPayloadSize; /* variable-size events */
typedef StgWord16 EventTypeNum;
typedef StgWord64 EventTimestamp; /* in nanoseconds */


void moreCapEventBufs (nat from, nat to);
void initEventLogging(const char * filename);
void postEventStartup(EventCapNo n_caps);
void endEventLogging(void);
void postEvent(VProc_t * vp, EventTypeNum typ);
void postSchedEvent (VProc_t *vp,
		     EventTypeNum tag,
		     unsigned long thread);
StgWord64 time_ns(void);

/*
 * Markers for begin/end of the Header.
 */
#define EVENT_HEADER_BEGIN    0x68647262 /* 'h' 'd' 'r' 'b' */
#define EVENT_HEADER_END      0x68647265 /* 'h' 'd' 'r' 'e' */

#define EVENT_DATA_BEGIN      0x64617462 /* 'd' 'a' 't' 'b' */
#define EVENT_DATA_END        0xffff

/*
 * Markers for begin/end of the list of Event Types in the Header.
 * Header, Event Type, Begin = hetb
 * Header, Event Type, End = hete
 */
#define EVENT_HET_BEGIN       0x68657462 /* 'h' 'e' 't' 'b' */
#define EVENT_HET_END         0x68657465 /* 'h' 'e' 't' 'e' */

#define EVENT_ET_BEGIN        0x65746200 /* 'e' 't' 'b' 0 */
#define EVENT_ET_END          0x65746500 /* 'e' 't' 'e' 0 */

/*
 * Types of event
 */
#define EVENT_GC_START             1 /* ()                     */
#define EVENT_GC_END               2 /* ()                     */
#define EVENT_START_TX             3
#define EVENT_EAGER_PARTIAL_ABORT  4
#define EVENT_EAGER_FULL_ABORT     5
#define EVENT_COMMIT_PARTIAL_ABORT 6
#define EVENT_COMMIT_FULL_ABORT    7
#define EVENT_COMMIT_TX            8
#define EVENT_START_TX_WITH_INFO   9
#define EVENT_BEGIN_COMMIT         10
#define EVENT_BLOCK_MARKER         11 /* (size, end_time, capability) */
#define EVENT_STARTUP              12 /* (num_capabilities)     */
#define EVENT_RUN_THREAD           13 /* (thread)               */
#define EVENT_STOP_THREAD          14 /* (thread, status, blockinfo) */
/*No EVENT_MINOR_GC, EVENT_GC_START implies a minor GC*/
#define EVENT_MAJOR_GC             15
#define EVENT_GLOBAL_GC            16
#define EVENT_FAST_FORWARD         17
#define EVENT_REMEMBER_OBJ         18
#define EVENT_TS_EXTENSION         19

/*
 * The highest event code +1 that ghc itself emits. Note that some event
 * ranges higher than this are reserved but not currently emitted by ghc.
 * This must match the size of the EventDesc[] array in EventLog.c
 */
#define NUM_GHC_EVENT_TAGS        20

/*
 * Status values for EVENT_STOP_THREAD
 *
 * 1-5 are the StgRun return values (from includes/Constants.h):
 *
 * #define HeapOverflow   1
 * #define StackOverflow  2
 * #define ThreadYielding 3
 * #define ThreadBlocked  4
 * #define ThreadFinished 5
 * #define ForeignCall                  6
 * #define BlockedOnMVar                7
 * #define BlockedOnBlackHole           8
 * #define BlockedOnRead                9
 * #define BlockedOnWrite               10
 * #define BlockedOnDelay               11
 * #define BlockedOnSTM                 12
 * #define BlockedOnDoProc              13
 * #define BlockedOnCCall               -- not used (see ForeignCall)
 * #define BlockedOnCCall_NoUnblockExc  -- not used (see ForeignCall)
 * #define BlockedOnMsgThrowTo          16
 */

#endif /* RTS_EVENTLOGFORMAT_H */
