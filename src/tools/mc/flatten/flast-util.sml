(* flast-util.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Various utility functions for manipulating FLAST terms.
 *)

structure FLASTUtil : sig

(*
(* create a tuple expression, with singleton tuples mapping to their element expression *)
  val mkTupleExp : FLAST.exp list-> FLAST.exp

(* create an expression that applies a function *)
  val mkApplyExp : (FLAST.exp * FLAST.exp list) -> FLAST.exp
*)
end = struct

(*
  structure F = FLAST
  structure B = Basis
  structure U = FTTypeUtil

  fun mkTupleExp [e] = e
    | mkTupleExp es = F.TupleExp es

(* FIXME write TypeOf...
  fun mkApplyExp (e : F.exp, es : F.exp list) : F.exp = 
        F.ApplyExp (e, mkTupleExp(es), (U.rangeType o FTTypeOf.exp) e)
*)
  fun mkApplyExp (e : F.exp, es : F.exp list) : F.exp =
    raise Fail "todo"
*)
end
