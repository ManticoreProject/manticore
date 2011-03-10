(* flatten-op-cmp.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure FlattenOpCmp = struct

  structure T = Types
  structure U = TypeUtil
  structure O = FlattenOps

  local
    fun domOf (T.FunTy (d, _)) = d
      | domOf _ = raise Fail "domOf"
    fun rngOf (T.FunTy (_, r)) = r
      | rngOf _ = raise Fail "rngOf"
    fun unzip ts = let
      fun lp ([], doms, rngs) = (doms, rngs)
	| lp (T.FunTy(d,r)::ts, doms, rngs) = 
            lp (ts, d::doms, r::rngs)
	| lp _ = raise Fail "unzip"
      in
        lp (List.rev ts, [], [])
      end
  in
  (* typeOfOper : oper -> T.ty *)
  (* type reconstruction for opers *)
    fun typeOfOper (q : O.oper) : T.ty =
     (case q 
       of O.ID t => t
	| O.Cat t => t
	| O.Map (_, t) => t
	| O.Unzip t => t
	| O.Compose (q1, q2) => let
	    val t1 = typeOfOper q1
	    val t2 = typeOfOper q2
	    in
	    (* check here... *)
	      if (U.same (domOf t1, rngOf t2)) then
                T.FunTy (domOf t2, rngOf t1)
	      else
                raise Fail "typeOfOper: compose mismatch"
            end
	| O.CrossCompose qs => let
	  (* no checking needed in this kind of composition *)
	    val (ds, rs) = unzip (List.map typeOfOper qs)
            in
	      T.FunTy (T.TupleTy ds, T.TupleTy rs)
	    end
     (* end case *))
  end (* local *)

(* same : oper * oper -> bool *)
  fun same (o1, o2) = (case (o1, o2)
    of (O.ID t1, O.ID t2) => U.same (t1, t2)
     | (O.Cat t1, O.Cat t2) => U.same (t1, t2)
     | (O.Map (op1, t1), O.Map (op2, t2)) => same (op1, op2) andalso U.same (t1, t2)
     | (O.Unzip t1, O.Unzip t2) => U.same (t1, t2)
     | (O.Compose (op11, op12), O.Compose (op21, op22)) =>
         same (op11, op21) andalso same (op12, op22)
     | (O.CrossCompose os1, O.CrossCompose os2) =>
         ListPair.allEq same (os1, os2)
     | _ => false)

(* compare : oper * oper -> order *)
(* for use in ORD_KEY-based collections *)
  fun compare (o1 : O.oper, o2 : O.oper) : order = let
    fun consIndex c = (case c
      of O.ID _           => 0
       | O.Cat _          => 1
       | O.Unzip _        => 2
       | O.Map _          => 3
       | O.Compose _      => 4
       | O.CrossCompose _ => 5
      (* end case *))
    fun cmp (o1, o2) = let
      val (i1, i2) = (consIndex o1, consIndex o2)
      in
        if i1 <> i2 then Int.compare (i1, i2)
	else case (o1, o2)
          of (O.ID t1, O.ID t2) => U.compare (t1, t2)
	   | (O.Cat t1, O.Cat t2) => U.compare (t1, t2)
	   | (O.Unzip t1, O.Unzip t2) => U.compare (t1, t2)
	   | (O.Map (o1, t1), O.Map (o2, t2)) =>
              (case cmp (o1, o2)
		of EQUAL => U.compare (t1, t2)
		 | neq => neq)
	   | (O.Compose (o11, o12), O.Compose (o21, o22)) =>
              (case cmp (o11, o21)
		of EQUAL => cmp (o12, o22)
		 | neq => neq)
	   | (O.CrossCompose os1, O.CrossCompose os2) => opers (os1, os2)
	   | _ => raise Fail "BUG!" (* shouldn't happen ever *)
      end
    and opers ([], []) = EQUAL
      | opers (_::_, []) = GREATER
      | opers ([], _::_) = LESS
      | opers (op1::ops1, op2::ops2) = (case cmp (op1, op2)
          of EQUAL => opers (ops1, ops2)
	   | neq => neq)
    in
      cmp (o1, o2)
    end

  structure OperKey : ORD_KEY = struct
    type ord_key = O.oper
    val compare = compare
  end

  structure Map = RedBlackMapFn(OperKey)

  structure Set = RedBlackSetFn(OperKey)

end
