/* gen-basis-offsets.c
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate offsets into the runtime-system data structures that are used
 * in the basis HLOps.
 */

#include <stdlib.h>
#include <stdbool.h>
#include "manticore-rt.h"
#include <stdio.h>
#include "vproc.h"
#include "log-file.h"
#include "crc.h"

/* since LOGBUF_SZ is defined in log-file.h, we have to be tricky */
static int LogBufSz = LOGBUF_SZ;
#undef LOGBUF_SZ

/* print the definition of a symbol */
#define PR_DEFINE(symb, val)							\
	printf("#define " #symb " %#0lx\n", (uint64_t)val)

/* print a value definition and record it in the CRC buffer */
#define PR_VALUE(tag, var, value)						\
	do {									\
	    bp = AddCRCData (bp, #tag, value);					\
	    PR_DEFINE(var, value);						\
	} while (0)

/* print a field offset and record it in the CRC buffer */
#define PR_OFFSET(obj, symb, lab)						\
	do {									\
	    uint32_t _offset = (int)((Addr_t)&(obj.lab) - (Addr_t)&obj);	\
	    PR_VALUE(lab, symb, _offset);					\
	} while (0)

/* print a VProc_t field offset and record it in the CRC buffer */
#define VP_OFFSET(obj, symb, lab, local)	PR_OFFSET(obj, symb, lab)

int main ()
{
    VProc_t		vp;
    LogBuffer_t		logBuffer;
    LogEvent_t		logEvent;
    unsigned char	buf[8*1024];
    char		*bp = (char *)buf;

    printf ("/* runtime-offsets.def\n");
    printf (" *\n");
    printf (" * WARNING: this file is generated; do not edit!!!\n");
    printf (" */\n");
    printf ("\n#ifndef _RUNTIME_OFFSETS_DEF_\n");
    printf ("\n#define _RUNTIME_OFFSETS_DEF_\n");

#include "vproc-offsets-ins.c"

  /* NOTE: the mask value is not used in the basis, but we need to include it to get the
   * CRC right.
   */
    printf ("\n/* mask to get address of VProc from alloc pointer */\n");
    PR_VALUE(vpMask, VP_MASK, ~((Addr_t)VP_HEAP_SZB-1));

    printf ("\n/* offsets into the log buffer */\n");
    PR_VALUE(logBufferSz, LOGBUF_SZ, LogBufSz);
    PR_OFFSET(logBuffer, LOGBUF_NEXT_OFFSET, next);
    PR_OFFSET(logBuffer, LOGBUF_START_OFFSET, log);

    printf ("\n/* offsets into a log event */\n");
    PR_VALUE(logEventSzB, LOG_EVENT_SZB, sizeof(LogEvent_t));
    PR_OFFSET(logEvent, LOG_EVENT_KIND_OFFSET, event);
    PR_OFFSET(logEvent, LOG_EVENT_DATA_OFFSET, data);

    printf ("\n/* magic number */\n");
    printf ("#define MAGIC %#0x\n", CRC32(buf, bp - (char *)buf));

    printf ("#endif /* !_RUNTIME_OFFSETS_DEF_ */\n");

    return 0;
}
