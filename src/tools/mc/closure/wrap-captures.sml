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
 *  - [ ASM_Resume | stackPtr (@retKWrap) | stackInfo | false ]
 *    When invoked, the closure passes the arg its given 
 *    to the stack frame pointed to, passing "false" in addition 
 *    to jump to the right block.
 *
 *  - during translation, within the expression C, any 'throw retK (val)' 
 *    is changed to 'throw retK' [true, val]'
 *  
 *
 *  If in BOM, the code in C is not allowed to return, then perhaps a 
 *  lower effort alternative with the same results would be the following BOM to BOM
 *  rewriting manually in the basis:
 *
 *  A
 *  cont k(x : t) = B
 *  in
 *    C
 *
 *   ===>
 *
 *    fun manipK(k : cont(t)) = C
 *    in
 *      A
 *      val _ : unit = ccall SpecialExternName (manipK)
 *      B
 *      
 *    Then, we write a CFG pass similar to alloc-c-calls-fn where we determine
 *    the live vars in B and attach it to the special ccall just before codegen. 
 *    Then, during codegen, we look for such ccalls to the special extern name and
 *    implement a special lowering that uses a statepoint call to an ASM routine that
 *    allocates the closure as above, and then calls manipK with it.
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
