(* wrap-captures.sml
 *
 * COPYRIGHT (c) 2017 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * After cont classification for direct-style conversion, we wrap continuation
 * captures so they may be emitted correctly by the code generator. In particular,
 * we perform the following type of eta-expansion
 *
 *  fun outerF(.. / retK, exnK) =
 *    A
 *    cont k(x) = B   <- an Other or Goto cont.
 *    in
 *      C
 *
 *      ===>
 *
 *  fun outerF(.. / retK : cont(t), exnK) =
 *    A
 *    cont k(x : t) = B   <-- now classified as a join cont
 *    in
 *      cont retKWrap (regularRet, arg : t) =     <-- a ret cont
 *       if regularRet 
 *         then throw retK arg
 *         else throw k arg
 *      in
 *        fun manipK(k' : cont(t) / retK', exnK') = C (where [k -> k', retK -> retK', exnK -> exnK')
 *        callec ( manipK / retKWrap, exnK)
 *
 *  the callec construct generates code that calls a special shim function.
 *  The shim will allocate the following continuation to represent 'k':
 *
 *  - [ ASM_Resume | stackPtr (@retKWrap) | false ]
 *    When invoked, the closure passes the arg its given 
 *    to the stack frame pointed to, passing "false" in addition 
 *    to jump to the right block.
 *
 *  - during translation, within the expression C, any 'throw retK (val)' 
 *    is changed to 'throw retK' [true, val]'
 *  
 *
 *)

structure WrapCaptures : sig

    val transform : CPS.module -> CPS.module

  end = struct

    structure C = CPS
    structure CV = C.Var
    structure VMap = CV.Map
    structure ST = Stats
    structure Census = CPSCensus

    fun transform (m as C.MODULE{name, externs, body}) = m

  end
