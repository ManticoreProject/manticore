(* flast-util.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Various utility functions for manipulating FLAST terms.
 *)

structure FLASTUtil : sig

  val unitExp : FLAST.exp

(* create a tuple expression, with singleton tuples mapping to their element expression *)
  val mkTupleExp : FLAST.exp list-> FLAST.exp

(* create an expression that applies a function *)
  val mkApplyExp : (FLAST.exp * FLAST.exp list) -> FLAST.exp

end = struct

  structure F = FLAST
  structure U = FTTypeUtil

  val unitExp = F.TupleExp ([], Basis.unitTy)

  fun mkTupleExp [e] = e
    | mkTupleExp es = let
        val ts = List.map (U.interfaceTy o FTTypeOf.exp) es
        val interfaceTy = Types.TupleTy ts
        in
          F.TupleExp (es, interfaceTy)
        end

  fun mkApplyExp (e : F.exp, es : F.exp list) : F.exp = let
    val t = FTTypeOf.exp e
    in
      F.ApplyExp (e, mkTupleExp es, U.rangeOf t)
    end

end
