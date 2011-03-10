(* flatten-op.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Operations on flattening operators.
 *
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure FlattenOp : sig

  val typeOf  : AST.fl_op -> Types.ty

  val same    : AST.fl_op * AST.fl_op -> bool
  val compare : AST.fl_op * AST.fl_op -> order

  val toString : AST.fl_op -> string

  structure Map : ORD_MAP where type Key.ord_key = AST.fl_op
  structure Set : ORD_SET where type Key.ord_key = AST.fl_op

end = struct

  structure A = AST
  structure T = Types
  structure U = TypeUtil

(* toString *)
  local
    fun sub conStr ty = let
      val s = U.toString ty
      in
        case String.substring (s, 0, 1)
          of "(" => conStr ^ "_" ^ s
	   | _   => conStr ^ "_(" ^ s ^ ")"
      end
  in
    fun toString (oper : A.fl_op) : string = (case oper
      of A.ID t => sub "ID" t
       | A.Cat t => sub "Cat" t
       | A.Unzip t => sub "Unzip" t
       | A.Map (oper, t) => sub ("Map(" ^ toString oper ^ ")") t
       | A.Compose (o1, o2) =>
           "(" ^ toString o1 ^ " o " ^ toString o2 ^ ")"
       | A.CrossCompose os => 
           "(" ^ String.concatWith " x " (List.map toString os) ^ ")"
      (* end case *))
  end (* local *)

(* typeOf *)
  local
    val domOf = (fn T.FunTy(d,_) => d | _ => raise Fail "domOf")
    val rngOf = (fn T.FunTy(_,r) => r | _ => raise Fail "rngOf")
    fun unzip ts = let
      fun lp ([], doms, rngs) = (doms, rngs)
	| lp (T.FunTy(d,r)::ts, doms, rngs) = 
            lp (ts, d::doms, r::rngs)
	| lp _ = raise Fail "unzip"
      in
        lp (List.rev ts, [], [])
      end
  in
  (* typeOf : A.fl_op -> T.ty *)
  (* type reconstruction for opers *)
    fun typeOf (q : A.fl_op) : T.ty =
     (case q 
       of A.ID t => t
	| A.Cat t => t
	| A.Map (_, t) => t
	| A.Unzip t => t
	| A.Compose (q1, q2) => let
	    val t1 = typeOf q1
	    val t2 = typeOf q2
	    in
	    (* check here... *)
	      if (U.same (domOf t1, rngOf t2)) then
                T.FunTy (domOf t2, rngOf t1)
	      else
                raise Fail "typeOf: compose mismatch"
            end
	| A.CrossCompose qs => let
	    val (ds, rs) = unzip (List.map typeOf qs)
            in
	      T.FunTy (T.TupleTy ds, T.TupleTy rs)
	    end
     (* end case *))
  end (* local *)

(* same : A.fl_op * A.fl_op -> bool *)
  fun same (o1 : A.fl_op, o2 : A.fl_op) : bool = (case (o1, o2)
    of (A.ID t1, A.ID t2) => U.same (t1, t2)
     | (A.Cat t1, A.Cat t2) => U.same (t1, t2)
     | (A.Map (op1, t1), A.Map (op2, t2)) => same (op1, op2) andalso U.same (t1, t2)
     | (A.Unzip t1, A.Unzip t2) => U.same (t1, t2)
     | (A.Compose (op11, op12), A.Compose (op21, op22)) =>
         same (op11, op21) andalso same (op12, op22)
     | (A.CrossCompose os1, A.CrossCompose os2) =>
         ListPair.allEq same (os1, os2)
     | _ => false)

(* compare : oper * oper -> order *)
(* for use in ORD_KEY-based collections *)
  local
    fun consIndex (c : A.fl_op) : int = (case c
      of A.ID _           => 0
       | A.Cat _          => 1
       | A.Unzip _        => 2
       | A.Map _          => 3
       | A.Compose _      => 4
       | A.CrossCompose _ => 5
      (* end case *))
  (* listCmp builds a lexicographic-style ordering on lists of elements *)
  (*   given a compare function for individual elements*)
    fun listCmp (cmp : 'a * 'a -> order) : 'a list * 'a list -> order = let
      fun lp ([], []) = EQUAL
	| lp (_::_, []) = GREATER
	| lp ([], _::_) = LESS
	| lp (x::xs, y::ys) = 
           (case cmp (x, y)
	     of EQUAL => lp (xs, ys)
	      | neq => neq)
      in
	lp
      end
  in
    fun compare (o1 : A.fl_op, o2 : A.fl_op) : order = let
      fun cmp (o1, o2) = let
        val (i1, i2) = (consIndex o1, consIndex o2)
        in
          if i1 <> i2 then Int.compare (i1, i2)
	  else case (o1, o2)
            of (A.ID t1, A.ID t2) => U.compare (t1, t2)
	     | (A.Cat t1, A.Cat t2) => U.compare (t1, t2)
	     | (A.Unzip t1, A.Unzip t2) => U.compare (t1, t2)
	     | (A.Map (o1, t1), A.Map (o2, t2)) =>
                (case cmp (o1, o2)
		  of EQUAL => U.compare (t1, t2)
		   | neq => neq)
	     | (A.Compose (o11, o12), A.Compose (o21, o22)) =>
                (case cmp (o11, o21)
		  of EQUAL => cmp (o12, o22)
		   | neq => neq)
	     | (A.CrossCompose os1, A.CrossCompose os2) => opers (os1, os2)
	     | _ => raise Fail "BUG!" (* shouldn't happen ever *)
        end
      and opers (os1, os2) = (listCmp cmp) (os1, os2)
      in
        cmp (o1, o2)
      end
  end (* local *)

  structure OperKey : ORD_KEY = struct
    type ord_key = A.fl_op
    val compare = compare
  end

  structure Map = RedBlackMapFn(OperKey)

  structure Set = RedBlackSetFn(OperKey)


(*
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

end
