(* flatten-ops.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Our flattening transformation includes type-indexed families of operators.
 * All these operators may be statically synthesized out of simple components.
 *
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure FlattenOps = struct

  structure T = Types
  structure U = TypeUtil

(* explicitly-typed operators *)
  datatype oper
    = ID of T.ty
    | Cat of T.ty
    | Unzip of T.ty
    | Map of oper * T.ty
    | Compose of oper * oper
    | CrossCompose of oper list

(* Two forms of composition: 
 * Compose represents standard sequential composition:
 *   fn (f, g) => (fn x => f (g x))
 * CrossCompose represents what I believe is called "pointwise" composition:
 *   fn (f, g) => (fn (x, y) => (f x, g, y))
 * Note Compose is sensitive to the types of the arguments given to it: 
 *   the range of its second arg must match the domain of the first.
 * CrossCompose, by contrast, produces a meaningful result unconditionally.
 *)

(* lift : ty -> ty *)
  fun lift t = T.FArrayTy (t, T.LfTy)
		
(* catTy : ty * nt_ty -> ty *)
  fun catTy (r, n) = let
    val dom = lift (T.FArrayTy (r, n))
    val rng = T.FArrayTy (r, T.NdTy n)
    in
      T.FunTy (dom, rng)
    end

(* mapTy : ty * ty * nt_ty -> ty *)
  fun mapTy (r, r', n) = 
    T.FunTy (T.FunTy (r, r'), 
	     T.FunTy (T.FArrayTy (r, n), 
		      T.FArrayTy (r', n)))

(*
(* fl : ty -> oper *)
(* Note that the flattener with argument r must be interpreted as
 * the flattener of arrays of type {r; lf}.
 *)
  fun fl (r : T.ty) : oper = let
    val repr = FlattenTypes.repr
    val arr  = lift r
    val farr = repr arr
    val f =
     (case r
        of T.FunTy (r1, r2) => ID (T.FunTy (farr, farr))
	 | T.TupleTy ts => let
	     val unzipDom = arr
	     val unzipRng = T.TupleTy (List.map (repr o lift) ts)
             val unzip = Unzip (T.FunTy (unzipDom, unzipRng))
             in
	       Compose (CrossCompose (List.map fl ts), unzip)
	     end
	 | T.FlatArrayTy (t', n) => let
             val cat = Cat (catTy (t', n))
	     val map = Map (fl t', mapTy (arr, farr, n))
             in
	       Compose (cat, map)
	     end
	 | T.ConTy (ts, c) => 
	     if U.isGround r then 
               ID (T.FunTy (farr, farr))
	     else 
	       raise Fail "todo"
	 | T.VarTy a => raise Fail "todo")
    in
      f
    end
*)

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
    fun typeOfOper (q : oper) : T.ty =
     (case q 
       of ID t => t
	| Cat t => t
	| Map (_, t) => t
	| Unzip t => t
	| Compose (q1, q2) => let
	    val t1 = typeOfOper q1
	    val t2 = typeOfOper q2
	    in
	    (* check here... *)
	      if (U.same (domOf t1, rngOf t2)) then
                T.FunTy (domOf t2, rngOf t1)
	      else
                raise Fail "typeOfOper: compose mismatch"
            end
	| CrossCompose qs => let
	  (* no checking needed in this kind of composition *)
	    val (ds, rs) = unzip (List.map typeOfOper qs)
            in
	      T.FunTy (T.TupleTy ds, T.TupleTy rs)
	    end
     (* end case *))
  end (* local *)

(* same : oper * oper -> bool *)
  fun same (o1, o2) = (case (o1, o2)
    of (ID t1, ID t2) => U.same (t1, t2)
     | (Cat t1, Cat t2) => U.same (t1, t2)
     | (Map (op1, t1), Map (op2, t2)) => same (op1, op2) andalso U.same (t1, t2)
     | (Unzip t1, Unzip t2) => U.same (t1, t2)
     | (Compose (op11, op12), Compose (op21, op22)) =>
         same (op11, op21) andalso same (op12, op22)
     | (CrossCompose os1, CrossCompose os2) =>
         ListPair.allEq same (os1, os2)
     | _ => false)

(* compare : oper * oper -> order *)
(* for use in ORD_KEY-based collections *)
  fun compare (o1 : oper, o2 : oper) : order = let
    fun consIndex c = (case c
      of ID _           => 0
       | Cat _          => 1
       | Unzip _        => 2
       | Map _          => 3
       | Compose _      => 4
       | CrossCompose _ => 5
      (* end case *))
    fun cmp (o1, o2) = let
      val (i1, i2) = (consIndex o1, consIndex o2)
      in
        if i1 <> i2 then Int.compare (i1, i2)
	else case (o1, o2)
          of (ID t1, ID t2) => U.compare (t1, t2)
	   | (Cat t1, Cat t2) => U.compare (t1, t2)
	   | (Unzip t1, Unzip t2) => U.compare (t1, t2)
	   | (Map (o1, t1), Map (o2, t2)) =>
              (case cmp (o1, o2)
		of EQUAL => U.compare (t1, t2)
		 | neq => neq)
	   | (Compose (o11, o12), Compose (o21, o22)) =>
              (case cmp (o11, o21)
		of EQUAL => cmp (o12, o22)
		 | neq => neq)
	   | (CrossCompose os1, CrossCompose os2) => opers (os1, os2)
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
    type ord_key = oper
    val compare = compare
  end

  structure Map = RedBlackMapFn(OperKey)

  structure Set = RedBlackSetFn(OperKey)

end
