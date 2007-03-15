/* gen-asm-offsets.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate offsets for assembly access to runtime data structures.
 */

#include <stdlib.h>
#include "manticore-rt.h"
#include <stdio.h>
#include "vproc.h"
#include "value.h"
#include "request-codes.h"
#include "../vproc/scheduler.h"

#define PR_OFFSET(obj, symb, lab)	\
	printf("#define " #symb " %d\n", (int)((Addr_t)&(obj.lab) - (Addr_t)&obj))
#define PR_DEFINE(symb)			\
	printf("#define " #symb " %d\n", symb)

int main ()
{
    VProc_t		vp;
    SchedActStkItem_t	actcons;

    printf ("#ifndef _ASM_OFFSETS_H_\n");
    printf ("#define _ASM_OFFSETS_H_\n");

    printf ("\n/* offsets into the VProc_t structure */\n");
    PR_OFFSET(vp, IN_MANTICORE, inManticore);
    PR_OFFSET(vp, ATOMIC, atomic);
    PR_OFFSET(vp, SIG_PENDING, sigPending);
    PR_OFFSET(vp, ALLOC_PTR, allocPtr);
    PR_OFFSET(vp, LIMIT_PTR, limitPtr);
    PR_OFFSET(vp, STD_ARG, stdArg);
    PR_OFFSET(vp, STD_EP, stdEnvPtr);
    PR_OFFSET(vp, STD_CONT, stdCont);
    PR_OFFSET(vp, STD_EXH, stdExnCont);
    PR_OFFSET(vp, ACTION_STK, actionStk);

    printf("\n/* mask to get address of VProc from alloc pointer */\n");
    printf("#define VP_MASK %#08lx\n", ~((Addr_t)VP_HEAP_SZB-1));

    printf("\n/* offsets for the scheduler-action stack */\n");
    printf("#define ACTCONS_HDR %d\n", VEC_HDR(2));
    PR_OFFSET(actcons, ACTCONS_ACT_OFF, act);
    PR_OFFSET(actcons, ACTCONS_LINK_OFF, link);

    printf("\n/* request codes for when Manticore returns to C */\n");
    PR_DEFINE(REQ_GC);
    PR_DEFINE(REQ_Return);
    PR_DEFINE(REQ_UncaughtExn);

    printf("\n/* common Manticore unboxed values */\n");
    PR_DEFINE(M_FALSE);
    PR_DEFINE(M_TRUE);
    PR_DEFINE(M_UNIT);
    PR_DEFINE(M_NIL);

    printf ("\n#endif\n");

   return 0;
}
