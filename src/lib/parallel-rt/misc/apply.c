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

extern RequestCode_t ASM_Apply (VProc_t *vp, Addr_t cp, Value_t arg, Value_t ep, Value_t rk, Value_t ek);
extern int ASM_Return;
extern int ASM_UncaughtExn;

/* Run a Manticore function f applied to arg.  If the function
 * returns, then return the result.
 */
Value_t RunManticore (VProc_t *vp, Value_t f, Value_t arg)
{
  /* get the code and environment pointers for f */
    Addr_t cp = ValueToAddr (ValueToClosure(f)->cp);
    Value_t ep = ValueToClosure(f)->ep;

    while (1) {
      /* allocate the return and exception continuation objects
       * in the VProc's heap.
       */
	Value_t retCont = AllocUniform(vp, 1, PtrToValue(&ASM_Return));
	Value_t exnCont = AllocUniform(vp, 1, PtrToValue(&ASM_UncaughtExn));
	RequestCode_t req = ASM_Apply (vp, cp, arg, ep, retCont, exnCont);
	switch (req) {
	  case REQ_GC: {/* request a minor GC */
		Value_t *roots[16], **rp;
		rp = roots;
		*rp++ = &(vp->stdArg);
		*rp++ = &(vp->stdEnvPtr);
		*rp++ = &(vp->stdCont);
		*rp++ = &(vp->stdExnCont);
		*rp++ = 0;
		MinorGC (vp, roots);
	      /* we need to invoke the stdCont to resume after GC */
		cp = ValueToAddr (ValueToCont(vp->stdCont)->cp);
		ep = M_UNIT;
		retCont = M_UNIT;  /* unused in throw to standard cont. */
		exnCont = M_UNIT;  /* unused in throw to standard cont. */
	    } break;
	  case REQ_Return:	/* returning from a function call */
	    return vp->stdArg;
	  case REQ_UncaughtExn:	/* raising an exception */
	    Die ("uncaught exception\n");
	  case REQ_Sleep:	/* make the VProc idle */
	    VProcSleep(vp);
/* FIXME: need to run the idle fiber! */
	    break;
	}
    }

}
