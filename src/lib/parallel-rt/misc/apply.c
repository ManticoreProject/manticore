/* apply.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include "value.h"
#include "return-codes.h"

extern Value_t ASM_Apply (VProc_t *vp, Addr_t cp, Value_t arg, Value_t ep, Value_t rk, Value_t ek)
extern int ASM_Return;
extern int ASM_UncaughtExn;


Value_t ApplyManticoreFn (VProc_t *vp, Value_t f, Value_t arg)
{
  /* allocate the return and exception continuation objects
   * in the VProc's heap.
   */
    Value_t retCont = AllocUniform(vp, 1, PtrToValue(&ASM_Return));
    Value_t exnCont = AllocUniform(vp, 1, PtrToValue(&ASM_UncaughtExn));

  /* get the code and environment pointers for f */
    Addr_t cp = ValueToAddr (ValueToClosure(f)->cp);
    Value_t ep = ValueToClosure(f)->ep;
    Value_t res = ASM_Apply (vp, cp, arg, ep, retCont, exnCont);

    return res;

}

