/* gen-asm-offsets.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate offsets for assembly access to runtime data structures.
 */

#ifdef NO_INLINE
#undef NO_INLINE /* because the unused inline functions refer to globals */
#endif

#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>
#include "manticore-rt.h"
#include <stdio.h>
#include <string.h>
#include "vproc.h"
#include "value.h"
#include "request-codes.h"
#include "../vproc/scheduler.h"
#include "log-file.h"
#include "crc.h"

/* since LOGBUF_SZ is defined in log-file.h, we have to be tricky */
static int LogBufSz = LOGBUF_SZ;
#undef LOGBUF_SZ

/* print the definition of a symbol */
#define PR_DEFINE(symb, val)							\
	printf("#define " #symb " %#0" PRIx64 "\n", (uint64_t)val)

/* print a value definition and record it in the CRC buffer */
#define PR_VALUE(tag, var, value)						\
	do {									\
	    bp = AddCRCData (bp, #tag, value);					\
	    PR_DEFINE(var, value);						\
	} while (0)

/* print a field offset and record it in the CRC buffer */
#define PR_OFFSET(obj, var, lab)						\
	do {									\
	    uint32_t _offset = (int)((Addr_t)&(obj.lab) - (Addr_t)&obj);	\
	    PR_VALUE(lab, var, _offset);					\
	} while (0)

/* print a VProc_t field offset and record it in the CRC buffer */
#define VP_OFFSET(obj, symb, lab, local)		PR_OFFSET(obj, symb, lab)

/* print a field offset without recording it in the CRC buffer */
#define PR_OFFSET_NOCRC(obj, var, lab)						\
	do {									\
	    uint32_t _offset = (int)((Addr_t)&(obj.lab) - (Addr_t)&obj);	\
	    PR_DEFINE(var, _offset);						\
	} while (0)

int main ()
{
    VProc_t		vp;
    LogBuffer_t		logBuffer;
    LogEvent_t		logEvent;
    SchedActStkItem_t	actcons;
    RdyQItem_t		rdyq;
    unsigned char	buf[8*1024];
    char		*bp = (char *)buf;

    printf ("#ifndef _ASM_OFFSETS_H_\n");
    printf ("#define _ASM_OFFSETS_H_\n");

    printf ("\n#ifdef NOT_C_SOURCE\n");

    printf ("\n/* offsets into the VProc_t structure */\n");
#include "vproc-offsets-ins.c"

    printf("\n/* mask to get address of VProc from alloc pointer */\n");
    PR_VALUE(vpMask, VP_MASK, ~((Addr_t)VP_HEAP_SZB-1));

    printf ("\n/* offsets into the log buffer */\n");
    PR_VALUE(logBufferSz, LOGBUF_SZ, LogBufSz);
    PR_OFFSET(logBuffer, LOGBUF_NEXT_OFFSET, next);
    PR_OFFSET(logBuffer, LOGBUF_START_OFFSET, log);

    printf ("\n/* offsets into a log event */\n");
    PR_VALUE(logEventSzB, LOG_EVENT_SZB, sizeof(LogEvent_t));
    PR_OFFSET(logEvent, LOG_EVENT_KIND_OFFSET, event);
    PR_OFFSET(logEvent, LOG_EVENT_DATA_OFFSET, data);

    printf("\n/* constants for the scheduler-action stack elements */\n");
    PR_DEFINE(ACTCONS_HDR, VEC_HDR(2));
    PR_DEFINE(ACTCONS_SZB, sizeof(SchedActStkItem_t) + WORD_SZB);
    PR_OFFSET_NOCRC(actcons, ACTCONS_ACT, act);
    PR_OFFSET_NOCRC(actcons, ACTCONS_LINK, link);

    printf("\n/* constants for the ready queue elements */\n");
    PR_DEFINE(RDYQ_HDR, VEC_HDR(3));
    PR_DEFINE(RDYQ_SZB, sizeof(RdyQItem_t) + WORD_SZB);
    PR_OFFSET_NOCRC(rdyq, RDYQ_FIBER, fiber);
    PR_OFFSET_NOCRC(rdyq, RDYQ_TID, tid);
    PR_OFFSET_NOCRC(rdyq, RDYQ_LINK, link);
    
    printf("\n/* Stack-frame size */\n");
    PR_DEFINE(FRAME_SZB, FRAME_SZB);

    printf("\n/* request codes for when Manticore returns to C */\n");
    PR_DEFINE(REQ_GC, REQ_GC);
    PR_DEFINE(REQ_Return, REQ_Return);
    PR_DEFINE(REQ_UncaughtExn, REQ_UncaughtExn);
    PR_DEFINE(REQ_Sleep, REQ_Sleep);

    printf("\n/* common Manticore unboxed values */\n");
    PR_DEFINE(M_UNIT, M_UNIT);
    PR_DEFINE(M_FALSE, M_FALSE);
    PR_DEFINE(M_TRUE, M_TRUE);
    PR_DEFINE(M_NIL, M_NIL);

    printf ("\n#endif /* NOT_C_SOURCE */\n");

    printf ("\n/* magic number */\n");
    printf ("#define RUNTIME_MAGIC %#0x\n", CRC32(buf, bp - (char *)buf));

    printf ("\n#endif\n");

   return 0;
}
