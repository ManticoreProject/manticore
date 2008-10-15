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
#include "manticore-rt.h"
#include <stdio.h>
#include <string.h>
#include "vproc.h"
#include "value.h"
#include "request-codes.h"
#include "../vproc/scheduler.h"

extern uint32_t CRC32 (void *buf, int nBytes);

#define PR_OFFSET(obj, symb, lab, local)					\
	do {									\
	    uint32_t _offset = (int)((Addr_t)&(obj.lab) - (Addr_t)&obj);	\
	    strncpy((char *)(buf+len), #lab, sizeof(#lab));			\
	    len += sizeof(#lab);						\
	    buf[len++] = ((_offset >> 8) & 0xff);				\
	    buf[len++] = _offset & 0xff;					\
	    printf("#define " #symb " %d\n", _offset);				\
	} while (0)

#define PR_DEFINE(symb)			\
	printf("#define " #symb " %d\n", symb)

int main ()
{
    VProc_t		vp;
    SchedActStkItem_t	actcons;
    RdyQItem_t		rdyq;
    unsigned char	buf[2048];
    int			len = 0;

    printf ("#ifndef _ASM_OFFSETS_H_\n");
    printf ("#define _ASM_OFFSETS_H_\n");

    printf ("\n/* offsets into the VProc_t structure */\n");
#include "vproc-offsets-ins.c"

    printf("\n/* mask to get address of VProc from alloc pointer */\n");
    printf("#define VP_MASK %#08lx\n", ~((Addr_t)VP_HEAP_SZB-1));

    printf ("\n/* magic number */\n");
    printf ("#define RUNTIME_MAGIC %#0x\n", CRC32(buf, len));

    printf("\n#ifdef NOT_C_SOURCE\n");

    printf("\n/* constants for the scheduler-action stack elements */\n");
    printf("#define ACTCONS_HDR %d\n", VEC_HDR(2));
    printf("#define ACTCONS_SZB %d\n", sizeof(SchedActStkItem_t) + WORD_SZB);
    PR_OFFSET(actcons, ACTCONS_ACT, act);
    PR_OFFSET(actcons, ACTCONS_LINK, link);

    printf("\n/* constants for the ready queue elements */\n");
    printf("#define RDYQ_HDR %d\n", VEC_HDR(3));
    printf("#define RDYQ_SZB %d\n", sizeof(RdyQItem_t) + WORD_SZB);
    PR_OFFSET(rdyq, RDYQ_FIBER, fiber);
    PR_OFFSET(rdyq, RDYQ_TID, tid);
    PR_OFFSET(rdyq, RDYQ_LINK, link);
    
    printf("\n/* Stack-frame size */\n");
    PR_DEFINE(FRAME_SZB);

    printf("\n/* request codes for when Manticore returns to C */\n");
    PR_DEFINE(REQ_GC);
    PR_DEFINE(REQ_Return);
    PR_DEFINE(REQ_UncaughtExn);
    PR_DEFINE(REQ_Sleep);

    printf("\n/* common Manticore unboxed values */\n");
    PR_DEFINE(M_FALSE);
    PR_DEFINE(M_TRUE);
    PR_DEFINE(M_UNIT);
    PR_DEFINE(M_NIL);

    printf ("\n#endif /* NOT_C_SOURCE */\n");
    printf ("\n#endif\n");

   return 0;
}
