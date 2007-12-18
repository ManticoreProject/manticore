/* apply.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include "vproc.h"
#include "gc.h"
#include "value.h"
#include "request-codes.h"
#include "scheduler.h"

extern RequestCode_t ASM_Apply (VProc_t *vp, Addr_t cp, Value_t arg, Value_t ep, Value_t rk, Value_t ek);
extern int ASM_Return;
extern int ASM_UncaughtExn;
extern int ASM_Resume;

/* \brief run a Manticore function f applied to arg.
 * \param vp the host vproc
 * \param f the Manticore function to apply
 * \param arg the Manticore value to apply \arg{f} to
 * \return the result of the application.
 */
Value_t ApplyFun (VProc_t *vp, Value_t f, Value_t arg)
{
  /* get the code and environment pointers for f */
    Addr_t cp = ValueToAddr (ValueToClosure(f)->cp);
    Value_t ep = ValueToClosure(f)->ep;

    RunManticore (vp, cp, arg, ep);

    return vp->stdArg;

} /* end of ApplyFun */


/* \brief Run Manticore code.
 * \param vp the host vproc
 * \param codeP the address of the code to run
 * \param arg the value of the standard argument register
 * \param envP the value of the standard environment-pointer register
 */
void RunManticore (VProc_t *vp, Addr_t codeP, Value_t arg, Value_t envP)
{
  /* allocate the return and exception continuation objects
   * in the VProc's heap.
   */
    Value_t retCont = AllocUniform(vp, 1, PtrToValue(&ASM_Return));
    Value_t exnCont = AllocUniform(vp, 1, PtrToValue(&ASM_UncaughtExn));

    while (1) {
#ifndef NDEBUG
	if (DebugFlg)
	    SayDebug("[%2d] ASM_Apply(-, %p, %p, %p, %p, %p)\n",
		vp->id, codeP, arg, envP, retCont, exnCont);
#endif
	RequestCode_t req = ASM_Apply (vp, codeP, arg, envP, retCont, exnCont);
	switch (req) {
	  case REQ_GC:
	  /* check to see if we actually need to do a GC, since this request
	   * might be from a pending signal.
	   */
	    if ((vp->limitPtr < vp->allocPtr) || vp->globalGCPending) {
	      /* request a minor GC; the protocol is that
	       * the stdCont register holds the return address (which is
	       * not in the heap) and that the stdEnvPtr holds the GC root.
	       */
		Value_t *roots[16], **rp;
		rp = roots;
		*rp++ = &(vp->stdEnvPtr);
		*rp++ = &(vp->actionStk);
		*rp++ = &(vp->rdyQHd);
		*rp++ = &(vp->rdyQTl);
		*rp++ = &(vp->currentFG);
		*rp++ = 0;
		MinorGC (vp, roots);
	    }
	  /* check for pending signals */
	    else if (vp->sigPending == M_TRUE) {
/* FIXME: this code assumes that the signal is always preemption */
	      
  	    /* Unload the vproc's entry queue */
		VProcPushEntries (vp, VProcGetEntryQ (vp));

		Value_t resumeK = AllocUniform(vp, 3,
			    PtrToValue(&ASM_Resume),
			    vp->stdCont,
			    vp->stdEnvPtr);
		SchedActStkItem_t *item = ValueToSchedActStkItem(vp->actionStk);
		assert (item != ValueToSchedActStkItem(M_NIL));
		vp->actionStk = item->link;
		envP = item->act;
		codeP = ValueToAddr(ValueToCont(envP)->cp);
		arg = resumeK;
		retCont = M_UNIT;
		exnCont = M_UNIT;
		vp->atomic = M_TRUE;
		vp->sigPending = M_FALSE;
	    }
	    else {
	      /* setup the return from GC */
	      /* we need to invoke the stdCont to resume after GC */
		codeP = ValueToAddr (vp->stdCont);
		envP = vp->stdEnvPtr;
	      /* clear the dead registers */
		arg = M_UNIT;
		retCont = M_UNIT;
		exnCont = M_UNIT;
	    }
	    break;
	  case REQ_Return:	/* returning from a function call */
	    return;
	  case REQ_UncaughtExn:	/* raising an exception */
	    Die ("uncaught exception\n");
	  case REQ_Sleep:	/* make the VProc idle */
	    VProcSleep(vp);
	    envP = vp->stdCont;
	    codeP = ValueToAddr(ValueToCont(envP)->cp);
	    arg = M_UNIT;
	    retCont = M_UNIT;
	    exnCont = M_UNIT;
	    break;
	}
    }

} /* end of RunManticore */
