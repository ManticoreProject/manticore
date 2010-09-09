/* vproc-offsets-ins.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file is inserted into both the gen-runtime-constants.c and gen-asm-offsets.c
 * programs to print the vproc offsets.
 *
 *	VP_OFFSET(vp, name, label, local)
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

    VP_OFFSET(vp, ATOMIC, atomic, true);
    VP_OFFSET(vp, SIG_PENDING, sigPending, false);
    VP_OFFSET(vp, VP_SLEEPING, sleeping, false);
    VP_OFFSET(vp, CURRENT_FLS, currentFLS, true);
    VP_OFFSET(vp, VP_ACTION_STK, actionStk, false);
    VP_OFFSET(vp, VP_SCHED_CONT, schedCont, false);
    VP_OFFSET(vp, VP_DUMMYK, dummyK, false);
    VP_OFFSET(vp, VP_WAKEUP_CONT, wakeupCont, true);
    VP_OFFSET(vp, VP_SHUTDOWN_CONT, shutdownCont, true);
    VP_OFFSET(vp, VP_RDYQ_HD, rdyQHd, true);
    VP_OFFSET(vp, VP_RDYQ_TL, rdyQTl, true);
    VP_OFFSET(vp, STD_ARG, stdArg, true);
    VP_OFFSET(vp, STD_EP, stdEnvPtr, true);
    VP_OFFSET(vp, STD_CONT, stdCont, true);
    VP_OFFSET(vp, STD_EXH, stdExnCont, true);
    VP_OFFSET(vp, ALLOC_PTR, allocPtr, true);
    VP_OFFSET(vp, EVENT_ID, eventId, true);
    VP_OFFSET(vp, LOG, log, true);
    VP_OFFSET(vp, NURSERY_BASE, nurseryBase, true);
    VP_OFFSET(vp, OLD_TOP, oldTop, true);
    VP_OFFSET(vp, GLOB_NEXT_W, globNextW, true);
    VP_OFFSET(vp, GLOB_LIMIT, globLimit, true);
    VP_OFFSET(vp, VPROC_ID, id, true);
    VP_OFFSET(vp, VP_LANDING_PAD, landingPad, false);
    VP_OFFSET(vp, LIMIT_PTR, limitPtr, true);
    
    VP_OFFSET(vp, PROXYTABLEENTRIES, proxyTableentries, true);
    VP_OFFSET(vp, MAXPROXY, maxProxy, true);
    VP_OFFSET(vp, PROXYTABLE, proxyTable, true);

