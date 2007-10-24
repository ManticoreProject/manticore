/* vproc-offsets-ins.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file is inserted into both the gen-runtime-constants.c and gen-asm-offsets.c
 * programs to print the vproc offsets.
 */

    PR_OFFSET(vp, IN_MANTICORE, inManticore);
    PR_OFFSET(vp, ATOMIC, atomic);
    PR_OFFSET(vp, SIG_PENDING, sigPending);
    PR_OFFSET(vp, CURRENT_TID, currentTId);
    PR_OFFSET(vp, VP_ACTION_STK, actionStk);
    PR_OFFSET(vp, VP_RDYQ_HD, rdyQHd);
    PR_OFFSET(vp, VP_RDYQ_TL, rdyQTl);
    PR_OFFSET(vp, VP_ENTRYQ, entryQ);
    PR_OFFSET(vp, VP_SECONDQ_HD, secondaryQHd);
    PR_OFFSET(vp, VP_SECONDQ_TL, secondaryQTl);
    PR_OFFSET(vp, STD_ARG, stdArg);
    PR_OFFSET(vp, STD_EP, stdEnvPtr);
    PR_OFFSET(vp, STD_CONT, stdCont);
    PR_OFFSET(vp, STD_EXH, stdExnCont);
    PR_OFFSET(vp, ALLOC_PTR, allocPtr);
    PR_OFFSET(vp, LIMIT_PTR, limitPtr);
    PR_OFFSET(vp, NURSERY_BASE, nurseryBase);
    PR_OFFSET(vp, OLD_TOP, oldTop);
    PR_OFFSET(vp, GLOB_TOSPACE, globToSpace);
    PR_OFFSET(vp, GLOB_NEXT_W, globNextW);
    PR_OFFSET(vp, GLOB_LIMIT, globLimit);
