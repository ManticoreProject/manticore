/* gen-runtime-constants.c
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate constant values that the runtime systems shares with the 
 * code generator.
 */

#include <stdlib.h>
#include <stdbool.h>
#include "manticore-rt.h"
#include <stdio.h>
#include <string.h>
#include "vproc.h"
#include "value.h"
#include "log-file.h"
#include "crc.h"

/* print the definition of a symbol */
#define PR_DEFINE(symb, val)							\
	printf("    val " #symb " : IntInf.int = %#0lx\n", (uint64_t)val)

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
#define VP_OFFSET(obj, xxx, lab, local)		PR_OFFSET(obj, lab, lab)

int main ()
{
    VProc_t		vp;
    LogBuffer_t		logBuffer;
    LogEvent_t		logEvent;
    unsigned char	buf[8*1024];
    char		*bp = (char *)buf;

    printf ("structure RuntimeConstants : RUNTIME_CONSTANTS =\n");
    printf ("  struct\n");

    printf ("\n  (* word size and alignment *)\n");
    PR_DEFINE(wordSzB, sizeof (Word_t));
    PR_DEFINE(wordAlignB, sizeof (Word_t));
    PR_DEFINE(boolSzB, sizeof (Word_t));
    PR_DEFINE(extendedAlignB, sizeof (double));

    printf ("\n  (* stack size and heap size info *)\n");
    PR_DEFINE(spillAreaSzB, SPILL_SZB);
    PR_DEFINE(spillAreaOffB, SAVE_AREA+PAD_SZB);
    PR_DEFINE(maxObjectSzB, ((sizeof (Word_t)*8)-MIXED_TAG_BITS)*sizeof(Word_t));

    printf ("\n  (* offsets into the VProc_t structure *)\n");
#include "vproc-offsets-ins.c"

    printf ("\n  (* mask to get address of VProc from alloc pointer *)\n");
    PR_VALUE(vpMask, VP_MASK, ~((Addr_t)VP_HEAP_SZB-1));

    printf ("\n  (* offsets into the log buffer *)\n");
    PR_VALUE(logBufferSzB, logBufferSzB, sizeof(LogBuffer_t));
    PR_OFFSET(logBuffer, logBufNext, next);
    PR_OFFSET(logBuffer, logBufLog, log);

    printf ("\n  (* offsets into a log event *)\n");
    PR_VALUE(logEventSzB, logEventSzB, sizeof(LogEvent_t));
    PR_OFFSET(logEvent, logEventEvent, event);
    PR_OFFSET(logEvent, logEventData, data);

    printf ("\n  (* magic number *)\n");
    PR_DEFINE(magic, CRC32(buf, bp - (char *)buf));

    printf ("\n  end (* RuntimeConstants *)\n");

}
