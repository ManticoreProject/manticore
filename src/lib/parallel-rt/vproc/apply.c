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
#include "inline-log.h"
#include "os-memory.h"

extern RequestCode_t ASM_Apply (VProc_t *vp, Addr_t cp, Value_t arg, Value_t ep, Value_t rk, Value_t ek);
extern int ASM_Return;
extern int ASM_UncaughtExn;
extern int ASM_Resume;
extern int ASM_Error;       // for linked-frames.
extern int ASM_ExecuteStack; // for linked-frames.
extern int ASM_ContLauncher1; // for linked-frames.

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


#if !defined(DIRECT_STYLE) || defined(LINKSTACK)
/* CPS and LINKSTACK version, which is basically a trampoline */

/* \brief Run Manticore code.
 * \param vp the host vproc
 * \param codeP the address of the code to run
 * \param arg the value of the standard argument register
 * \param envP the value of the standard environment-pointer register
 */
void RunManticore (VProc_t *vp, Addr_t codeP, Value_t arg, Value_t envP)
{
    /* TODO:
        - we need some sort of "unit" continuation, that is not heap allocated.
        - some of the code after ASM_Apply returns needs to be updated.
    */

    /* allocate the return and exception continuation objects
     * in the VProc's heap.
     */
#ifdef LINKSTACK
    Value_t retCont = CreateBaseFrame(vp, (Word_t)&ASM_Return);
    FunClosure_t* closObj;
#else
    Value_t retCont = WrapWord(vp, (Word_t)&ASM_Return);
#endif

    Value_t exnCont = WrapWord(vp, (Word_t)&ASM_UncaughtExn);

    while (1) {
#ifndef NDEBUG
        if (DebugFlg)
            SayDebug("[%2d] ASM_Apply(%p, %p, %p, %p, %p, %p)\n",
                 vp->id, (void*)vp, (void*)codeP, (void*)arg, (void*)envP, (void*)retCont, (void*)exnCont);
#endif
        if (ShutdownFlg && !(vp->shutdownPending == M_TRUE)) {
          /* schedule a continuation that will cleanly shut down the runtime */

#ifdef LINKSTACK
            vp->stdCont = M_UNIT;  // for safety, kill the old stack.
            closObj = ValueToClosure(vp->shutdownCont);

            envP = closObj->ep;
            codeP = ValueToAddr(closObj->cp);
            retCont = CreateBaseFrame(vp, (Word_t)&ASM_Error);
#else
            envP = vp->shutdownCont;
            codeP = ValueToAddr(ValueToCont(envP)->cp);
            retCont = M_UNIT;
#endif
            arg = M_UNIT;
            exnCont = M_UNIT;
            vp->atomic = M_TRUE;
            vp->sigPending = M_FALSE;
            vp->shutdownPending = M_TRUE;  // schedule the shutdown continuation just once
        }

        // LogRunThread(vp, 0); // FIXME logging is a mess right now
        RequestCode_t req = ASM_Apply (vp, codeP, arg, envP, retCont, exnCont);
        // LogStopThread(vp, 0, req); //thread id and stop status, TODO: thread id is not currently used

        Addr_t oldLimitPtr = SetLimitPtr(vp, LimitPtr(vp));

        switch (req) {
          case REQ_GC:
          /* check to see if we actually need to do a GC, since this request
           * might be from a pending signal.
           */
            if ((LimitPtr(vp) <= vp->allocPtr) || vp->globalGCPending) {
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

#ifdef LINKSTACK
                Value_t resumeK = CreateLinkStackCont (vp,
                                           PtrToValue(&ASM_ContLauncher1),
                                           PtrToValue(vp->stdCont));

                closObj = ValueToClosure(vp->schedCont);

                envP = closObj->ep;
                codeP = ValueToAddr(closObj->cp);
                retCont = CreateBaseFrame(vp, (Word_t)&ASM_Error);

#else

                // CPS
                Value_t resumeK = AllocNonUniform (vp, 3,
                                           INT(PtrToValue(&ASM_Resume)),
                                           INT(PtrToValue(vp->stdCont)),
                                           PTR(vp->stdEnvPtr));
              /* pass the signal to scheduling code in the BOM runtime */
                envP = vp->schedCont;
                codeP = ValueToAddr(ValueToCont(envP)->cp);
                retCont = M_UNIT;
#endif

                arg = resumeK;
                exnCont = M_UNIT;
                vp->atomic = M_TRUE;
                vp->sigPending = M_FALSE;
                LogPreemptSignal(vp);
            }
            else {
              /* setup the return from GC */
#ifdef LINKSTACK
                codeP = (Addr_t)&ASM_ExecuteStack;
                envP = M_UNIT;
                retCont = vp->stdCont;
#else
              /* we need to invoke the stdCont to resume after GC */
                codeP = ValueToAddr (vp->stdCont);
                envP = vp->stdEnvPtr;
                retCont = M_UNIT;
#endif
              /* clear the dead registers */
                arg = M_UNIT;
                exnCont = M_UNIT;
            }
            break;
          case REQ_Return:        /* returning from a function call */
          /* shutdown the runtime
           * in the future we should create a new request code to handle shutdown.
           */
            ShutdownFlg = true;
            for (int i = 0; i < NumVProcs; i++) {
              /* force each vproc to check for shutdown */
                VProc_t *wvp = VProcs[i];
            #ifdef LINKSTACK
                // Allocate in my heap and then promote
                Value_t dummyK = NewStack(vp, wvp->dummyK);
                dummyK = PromoteObj(vp, dummyK);
            #else
                Value_t dummyK = wvp->dummyK;
            #endif
                VProcSendSignal(vp, wvp, wvp->currentFLS, dummyK);
                VProcPreempt (vp, wvp);
            }
            break;
          case REQ_UncaughtExn:        /* raising an exception */
            Die ("uncaught exception\n");

          case REQ_Sleep:        /* make the VProc idle */
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

#ifdef LINKSTACK
             retCont = CreateBaseFrame(vp, (Word_t)&ASM_Error);
#else
             retCont = M_UNIT;
#endif

             exnCont = M_UNIT;
             vp->wakeupCont = M_NIL;
            }
            break;
          default:
            Die("unknown signal %d\n", req);
        }
    }

} /* end of RunManticore */



////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////


#else /* DIRECT_STYLE && !(LINKSTACK) */

// This version is for contiguous and segmented stacks, since
// both of these are designed to run RTS services, FFI calls, and
// manticore code on the same stack.

extern void ASM_DS_Apply (
    VProc_t *vp,
    Addr_t cp,
    Value_t ep,
    Value_t exh,
    Value_t arg,
    Value_t stkPtr);

extern void ASM_Resume_Stack (
    VProc_t *vp,
    Value_t stk,
    Value_t ep,
    Value_t exh,
    Value_t arg);

extern int ASM_DS_EscapeThrow;
extern int ASM_DS_UncaughtExn;

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
    Value_t exnCont = WrapWord(vp, (Word_t)&ASM_DS_UncaughtExn);

  /* allocate the main function's stack */
  void* stkPtr;
  StackInfo_t* info = NewMainStack(vp, &stkPtr);

  // write 'info' to vp->stdCont to establish that it is the current stack.
  vp->stdCont = (Value_t) info;

#if defined(SEGSTACK) || defined(RESIZESTACK)
  // set stack limit
  vp->stdEnvPtr = GetStkLimit(info);
#endif

  /* apply the given function  */
  LogRunThread(vp, 0);
  ASM_DS_Apply(vp, codeP, envP, exnCont, arg, stkPtr);

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
void RequestService(VProc_t *vp, RequestCode_t req) {
    LogStopThread(vp, 0, req);

    /*
        vp->stdEp == current stack pointer
        vp->stdCont == current stack's descriptor.
    */

    Value_t envP, arg, exnCont;
    Addr_t codeP;
    FunClosure_t* closObj;
    Value_t stkPtr;

    Addr_t oldLimitPtr = SetLimitPtr(vp, LimitPtr(vp));

doShutdown:
    if (ShutdownFlg && !(vp->shutdownPending == M_TRUE)) {
        closObj = ValueToClosure(vp->shutdownCont);
        envP = closObj->ep;
        codeP = ValueToAddr(closObj->cp);
        arg = M_UNIT;
        exnCont = M_UNIT;
        vp->atomic = M_TRUE;
        vp->sigPending = M_FALSE;
        vp->shutdownPending = M_TRUE;  // schedule the shutdown continuation just once

        stkPtr = vp->stdEnvPtr;

        #if defined(SEGSTACK) || defined(RESIZESTACK)
          // set stack limit
          vp->stdEnvPtr = GetStkLimit((StackInfo_t*) vp->stdCont);
        #endif

        LogRunThread(vp, 0);
        ASM_DS_Apply(vp, codeP, envP, exnCont, arg, stkPtr);
    }

    switch (req) {
        case REQ_GC:

        /* check to see if we actually need to do a GC, since this request
         * might be from a pending signal.
         */
          if ((LimitPtr(vp) <= vp->allocPtr)
              || vp->globalGCPending
              || vp->allocdSinceGC == ~0) {
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
                Value_t resumeK = AllocStkCont(vp, (Addr_t)&ASM_DS_EscapeThrow,
                                                    vp->stdEnvPtr, // stack ptr
                                                    vp->stdCont); // stack info

                  /* pass the signal to scheduling code in the BOM runtime */

            closObj = ValueToClosure(vp->schedCont);
            envP = closObj->ep;
            codeP = ValueToAddr(closObj->cp);
            arg = resumeK;
            exnCont = M_UNIT;
            vp->atomic = M_TRUE;
            vp->sigPending = M_FALSE;
            LogPreemptSignal(vp);

            stkPtr = vp->stdEnvPtr;

            #if defined(SEGSTACK) || defined(RESIZESTACK)
              // set stack limit
              vp->stdEnvPtr = GetStkLimit((StackInfo_t*) vp->stdCont);
            #endif

            LogRunThread(vp, 0);
            ASM_DS_Apply(vp, codeP, envP, exnCont, arg, stkPtr);

                }
                else {
                 /* setup the return from GC */
                stkPtr = vp->stdEnvPtr; // actually the stack pointer
                envP = M_UNIT;
                exnCont = M_UNIT;
                arg = M_UNIT;

                #if defined(SEGSTACK) || defined(RESIZESTACK)
                  // set stack limit
                  vp->stdEnvPtr = GetStkLimit((StackInfo_t*) vp->stdCont);
                #endif

                LogRunThread(vp, 0);
                ASM_Resume_Stack (vp, stkPtr, envP, exnCont, arg);
                }

            Die("unreachable in REQ_GC");
            break;
        /********************/
        case REQ_Return:
                  /* shutdown the runtime
                   * in the future we should create a new request code to handle shutdown.
                   */
                ShutdownFlg = true;
                for (int i = 0; i < NumVProcs; i++) {
                  /* force each vproc to check for shutdown */
                    VProc_t *wvp = VProcs[i];

                    // Allocate in my heap and then promote
                    Value_t dummyK = NewStack(vp, wvp->dummyK);
                    dummyK = PromoteObj(vp, dummyK);

                    VProcSendSignal(vp, wvp, wvp->currentFLS, dummyK);
                    VProcPreempt (vp, wvp);
                }

                goto doShutdown;

                Die("unreachable in REQ_Return");
                break;

        case REQ_Sleep:
            /* make the VProc idle */
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
             exnCont = M_UNIT;
             vp->wakeupCont = M_NIL;

             stkPtr = vp->stdEnvPtr;

             #if defined(SEGSTACK) || defined(RESIZESTACK)
               // set stack limit
               vp->stdEnvPtr = GetStkLimit((StackInfo_t*) vp->stdCont);
             #endif

             LogRunThread(vp, 0);
             ASM_DS_Apply(vp, codeP, envP, exnCont, arg, stkPtr);

             Die("unreachable in REQ_Sleep");
          }
            break;

        case REQ_UncaughtExn:
            Die ("uncaught exception\n");
            break;

        default:
            Die("unknown signal %d\n", req);
            break;

    }
    Die("unreachable");
}


#endif /* DIRECT_STYLE */
