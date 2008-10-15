/* vproc-offsets-ins.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file is inserted into both the gen-runtime-constants.c and gen-asm-offsets.c
 * programs to print the vproc offsets.
 *
 *	PR_OFFSET(vp, name, label, local)
 *
 * The fields are:
 *
 *	vp		a VProc_t object used to compute the offsets
 *	name		the name of the symbolic constant
 *	label		the label of the field in the VProc_t struct
 *	local		true if this field is only modified by the local code
 *			It should be false if other vprocs or the signal handler
 *			can modify it.  I.e., local means non volatile.
 *	
 */

    PR_OFFSET(vp, IN_MANTICORE, inManticore, true);
    PR_OFFSET(vp, ATOMIC, atomic, true);
    PR_OFFSET(vp, SIG_PENDING, sigPending, false);
    PR_OFFSET(vp, CURRENT_FG, currentFG, true);
    PR_OFFSET(vp, VP_ACTION_STK, actionStk, false);
    PR_OFFSET(vp, VP_SCHED_CONT, schedCont, false);
    PR_OFFSET(vp, VP_RDYQ_HD, rdyQHd, true);
    PR_OFFSET(vp, VP_RDYQ_TL, rdyQTl, true);
    PR_OFFSET(vp, VP_ENTRYQ, entryQ, false);
    PR_OFFSET(vp, VP_SECONDQ_HD, secondaryQHd, false);
    PR_OFFSET(vp, VP_SECONDQ_TL, secondaryQTl, false);
    PR_OFFSET(vp, STD_ARG, stdArg, true);
    PR_OFFSET(vp, STD_EP, stdEnvPtr, true);
    PR_OFFSET(vp, STD_CONT, stdCont, true);
    PR_OFFSET(vp, STD_EXH, stdExnCont, true);
    PR_OFFSET(vp, ALLOC_PTR, allocPtr, true);
    PR_OFFSET(vp, LIMIT_PTR, limitPtr, false);
    PR_OFFSET(vp, NURSERY_BASE, nurseryBase, true);
    PR_OFFSET(vp, OLD_TOP, oldTop, true);
    PR_OFFSET(vp, GLOB_NEXT_W, globNextW, true);
    PR_OFFSET(vp, GLOB_LIMIT, globLimit, true);
    PR_OFFSET(vp, LOG, log, true);
    PR_OFFSET(vp, VPROC_ID, id, false);
    PR_OFFSET(vp, VP_IDLE, idle, false);
