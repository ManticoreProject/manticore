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
#include "heap.h"
#include "event-log.h"
#include "os-memory.h"

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

volatile uint64_t numGCs = 0;


#ifndef DIRECT_STYLE
/* CPS version, which is basically a trampoline */

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
    Value_t retCont = WrapWord(vp, (Word_t)&ASM_Return);
    Value_t exnCont = WrapWord(vp, (Word_t)&ASM_UncaughtExn);

    while (1) {
#ifndef NDEBUG
	if (DebugFlg)
	    SayDebug("[%2d] ASM_Apply(%p, %p, %p, %p, %p, %p)\n",
                 vp->id, (void*)vp, (void*)codeP, (void*)arg, (void*)envP, (void*)retCont, (void*)exnCont);
#endif
	if (ShutdownFlg && !(vp->shutdownPending == M_TRUE)) {
	  /* schedule a continuation that will cleanly shut down the runtime */
	    envP = vp->shutdownCont;
	    codeP = ValueToAddr(ValueToCont(envP)->cp);
	    arg = M_UNIT;
	    retCont = M_UNIT;
	    exnCont = M_UNIT;
	    vp->atomic = M_TRUE;
	    vp->sigPending = M_FALSE;
	    vp->shutdownPending = M_TRUE;  // schedule the shutdown continuation just once
	}

	LogRunThread(vp, 0);
	RequestCode_t req = ASM_Apply (vp, codeP, arg, envP, retCont, exnCont);
	LogStopThread(vp, 0, req); //thread id and stop status, TODO: thread id is not currently used

	Addr_t oldLimitPtr = SetLimitPtr(vp, LimitPtr(vp));

	switch (req) {
	  case REQ_GC:
	  /* check to see if we actually need to do a GC, since this request
	   * might be from a pending signal.
	   */
	    if ((LimitPtr(vp) < vp->allocPtr) || vp->globalGCPending) {
	      /* request a minor GC */
		MinorGC (vp);
	    }
	  /* check for asynchronous signals */
	    if (oldLimitPtr == 0) {
#ifndef NDEBUG
	      if (DebugFlg)
		SayDebug("Asynchronous signal arrived at vproc %d\n", vp->id);
#endif
	      /* an asynchronous signal has arrived */
	        vp->sigPending = M_TRUE;
	    }

	  /* is there a pending signal that we can deliver? */
	    if ((vp->sigPending == M_TRUE) && (vp->atomic == M_FALSE)) {
            // TODO(kavon): replace this alloc with a specialized version for this retk.
		Value_t resumeK = AllocNonUniform (vp, 3,
                                           INT(PtrToValue(&ASM_Resume)),
                                           INT(PtrToValue(vp->stdCont)),
                                           PTR(vp->stdEnvPtr));
	      /* pass the signal to scheduling code in the BOM runtime */
		envP = vp->schedCont;
		codeP = ValueToAddr(ValueToCont(envP)->cp);
		arg = resumeK;
		retCont = M_UNIT;
		exnCont = M_UNIT;
		vp->atomic = M_TRUE;
		vp->sigPending = M_FALSE;
		LogPreemptSignal(vp);
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
	  /* shutdown the runtime
	   * in the future we should create a new request code to handle shutdown.
	   */
	    ShutdownFlg = true;
	    for (int i = 0; i < NumVProcs; i++) {
	      /* force each vproc to check for shutdown */
		VProc_t *wvp = VProcs[i];
		VProcSendSignal(vp, wvp, wvp->currentFLS, wvp->dummyK);
		VProcPreempt (vp, wvp);
	    }
	    break;
	  case REQ_UncaughtExn:	/* raising an exception */
	    Die ("uncaught exception\n");
	  case REQ_Sleep:	/* make the VProc idle */
	    {
	       Value_t status = M_TRUE;
	       Time_t timeToSleep = *((Time_t*)(vp->stdArg));
	       if (timeToSleep == 0)    /* convention: if timeToSleep == 0, sleep indefinitely */
		   VProcSleep(vp);
	       else
		   status = VProcNanosleep(vp, timeToSleep);
	       assert (vp->wakeupCont != M_NIL);
	       envP = vp->wakeupCont;
	       codeP = ValueToAddr (ValueToCont(envP)->cp);
	       arg = AllocNonUniform (vp, 1, PTR(status));
	       retCont = M_UNIT;
	       exnCont = M_UNIT;
	       vp->wakeupCont = M_NIL;
	    }
	    break;
	  default:
	    Die("unknown signal %d\n", req);
	}
    }

} /* end of RunManticore */






#else /* DIRECT_STYLE */

/* direct-style version below */
extern void ASM_Apply_StdDS_NoRet (
    VProc_t *vp,
    Addr_t cp, 
    Value_t ep,
    Value_t exh,
    Value_t arg);

extern void ASM_Apply_StdDS_WithStk (
    VProc_t *vp,
    Addr_t cp, 
    Value_t ep,
    Value_t exh,
    Value_t arg,
    Value_t stkPtr);
    
extern int ASM_DS_Return;
// extern int ASM_UncaughtExn;
// extern int ASM_Resume;

/* \brief Run Manticore code. Assumption is that this function is called
 *         only when bootstrapping the vproc with the initial function.
 *
 * \param vp the host vproc
 * \param codeP the address of the code to run
 * \param arg the value of the standard argument register
 * \param envP the value of the standard environment-pointer register
 */
void RunManticore (VProc_t *vp, Addr_t codeP, Value_t arg, Value_t envP)
{
  /* allocate the top-level exception handler in the heap */
    Value_t exnCont = WrapWord(vp, (Word_t)&ASM_UncaughtExn);
  
  /* allocate & initialize the main function's stack */
  const size_t size = 8192;
  StackInfo_t* info;
  void* stkPtr = AllocStack(size, &info);
  
  uint64_t* ptrToRetAddr = (uint64_t*)stkPtr;
  *ptrToRetAddr = (uint64_t)&ASM_DS_Return;
  
  /* NOTE probably need to put this stack in the allocated list of the vp */
  
  /* apply the given function  */
  LogRunThread(vp, 0);
  ASM_Apply_StdDS_WithStk(vp, codeP, envP, exnCont, arg, stkPtr);
  
  Die("unexpected return to RunManticore");
    
} /* end RunManticore */

/* 
 * This function doesn't return normally. 
 *
 * If we perform a context switch to some other stack,
 * we place the captured stack into a closure that will
 * effectively perform a longjmp when invoked.
 *
 * That closure is placed in the scheduling queue,
 * and then another very similar closure is retrieved 
 * from the queue and invoked. 
 */
VProc_t* RequestService(VProc_t *vp, RequestCode_t req) {
    LogStopThread(vp, 0, req);
    
    Value_t envP, arg, exnCont;
    Addr_t codeP;
    FunClosure_t* shutdownClos;
    
    Addr_t oldLimitPtr = SetLimitPtr(vp, LimitPtr(vp));
    
    switch (req) {
        case REQ_GC:
        
        Die("requesting GC is not supported right now.");
        
        /* check to see if we actually need to do a GC, since this request
         * might be from a pending signal.
         */
          if ((LimitPtr(vp) < vp->allocPtr) || vp->globalGCPending) {
              /* request a minor GC */
                MinorGC (vp);
          }
          
          
          /* check for asynchronous signals */
    	    if (oldLimitPtr == 0) {
    #ifndef NDEBUG
    	      if (DebugFlg)
    		SayDebug("Asynchronous signal arrived at vproc %d\n", vp->id);
    #endif
    	      /* an asynchronous signal has arrived */
    	        vp->sigPending = M_TRUE;
    	    }

    	  /* is there a pending signal that we can deliver? */
    	    if ((vp->sigPending == M_TRUE) && (vp->atomic == M_FALSE)) {
    		/* package up the current stack and pass 
               it to the scheduler fn in BOM */
    	    }
            else {
              /* return to the current stack from GC */
            }
          
        break;  
        /********************/
        case REQ_Return:
                shutdownClos = ValueToClosure(vp->shutdownCont);
                // yes, the two lines below look fishy.
                // FunClosure_t uses {cp, ep}, but
                // the codegen uses {ep, cp}
        	    envP = shutdownClos->cp;
                codeP = ValueToAddr(shutdownClos->ep);
        	    arg = M_UNIT;
        	    exnCont = M_UNIT;
        	    vp->atomic = M_TRUE;
        	    vp->sigPending = M_FALSE;
        	    vp->shutdownPending = M_TRUE;
                
        	    ASM_Apply_StdDS_NoRet(vp, codeP, envP, exnCont, arg);
                
                Die("Should have never gotten here. 123");
                break;
                
        case REQ_Sleep:
        default:
            Die("unknown signal %d\n", req);
            break;
            
        case REQ_UncaughtExn:
            Die ("uncaught exception\n");
            break;
        
    }
}


#endif /* DIRECT_STYLE */
