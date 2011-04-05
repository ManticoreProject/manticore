(* parray-op.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Operations on flattening operators.
 *
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure PArrayOp = struct

  structure A = AST
  structure B = Basis
  structure T = Types

  structure TU = TypeUtil

  fun toString (A.PA_Length ty) = "PA_Length_<" ^ TU.toString ty ^ ">"

  fun typeOf (A.PA_Length ty) = T.FunTy (ty, B.intTy)

  fun same (A.PA_Length t1, A.PA_Length t2) = TU.same (t1, t2)

(* compare : oper * oper -> order *)
(* for use in ORD_KEY-based collections *)
  local
    fun consIndex (c : A.parray_op) : int = (case c
      of A.PA_Length _ => 0
      (* end case *))
  in
    fun compare (o1 : A.parray_op, o2 : A.parray_op) : order = let
      fun cmp (o1, o2) = let
        val (i1, i2) = (consIndex o1, consIndex o2)
        in
          if i1 <> i2 then Int.compare (i1, i2)
	  else case (o1, o2)
            of (A.PA_Length t1, A.PA_Length t2) => TU.compare (t1, t2)
        end
      in
        cmp (o1, o2)
      end
  end (* local *)

  structure OperKey : ORD_KEY = struct
    type ord_key = A.parray_op
    val compare = compare
  end

  structure Map = RedBlackMapFn(OperKey)

  structure Set = RedBlackSetFn(OperKey)

end
