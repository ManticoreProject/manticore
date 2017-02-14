(* wrap-captures.sml
 *
 * COPYRIGHT (c) 2017 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * After cont classification for direct-style conversion, we wrap continuation
 * captures so they may be emitted correctly by the code generator. In particular,
 * we perform the following type of eta-expansion
 *
 *  A
 *  cont k(x) = B
 *  in
 *    C
 *
 *      ===>
 *
 *	A
 *  cont k'(x) = B
 *  in
 *    fun manipK(k) = C
 *    apply manipK(k')
 *
 *  the apply that is introduced is marked specially as well. we could have introduced
 *  a 'capture_apply' construct to make this look nicer.
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
