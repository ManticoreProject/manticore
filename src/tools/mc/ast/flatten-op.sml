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

(* type-indexed flattening operators *)
  datatype fl_op
    = ID of Types.ty
    | Unzip of Types.ty
    | CatMap of fl_op
    | Compose of fl_op * fl_op
    | CrossCompose of fl_op list

  val typeOf  : fl_op -> Types.ty

  val same    : fl_op * fl_op -> bool
  val compare : fl_op * fl_op -> order

  val toString : fl_op -> string

  structure Map : ORD_MAP where type Key.ord_key = fl_op
  structure Set : ORD_SET where type Key.ord_key = fl_op

end = struct

  structure A = AST
  structure T = Types
  structure U = TypeUtil

(* type-indexed flattening operators *)
  datatype fl_op
    = ID of T.ty
    | Unzip of T.ty
    | CatMap of fl_op
    | Compose of fl_op * fl_op
    | CrossCompose of fl_op list

(* toString *)
  local
    fun sub conStr ty = conStr ^ "_<" ^ U.toString ty ^ ">"
  in
    fun toString (oper : fl_op) : string = (case oper
      of ID t => sub "ID" t
       | Unzip t => sub "Unzip" t
       | CatMap oper => "CatMap(" ^ toString oper ^ ")"
       | Compose (o1, o2) =>
           "(" ^ toString o1 ^ " o " ^ toString o2 ^ ")"
       | CrossCompose os => 
           "(" ^ String.concatWith " x " (List.map toString os) ^ ")"
      (* end case *))
  end (* local *)

  local
    fun fArray ty = T.FArrayTy (ty, T.LfTy)
    fun isGroundTy ty = 
     (case ty
       of T.ConTy ([], c) => 
            List.exists (fn c' => TyCon.same (c',c)) Basis.primTycs
	| _ => false)
  in
    fun mkFlOp (r : T.ty) : fl_op = let
      val f = (case r
        of T.FunTy (r1, r2) => ID (T.FunTy (fArray r, fArray r))
	 | T.TupleTy [] => (* unit *) ID (T.FunTy (fArray r, fArray r))
	 | T.TupleTy ts => let
	     val unzipDom = fArray r
	     val unzipRng = T.TupleTy (List.map fArray ts)
             val unzip = Unzip (T.FunTy (unzipDom, unzipRng))
             in
	       Compose (CrossCompose (List.map mkFlOp ts), unzip)
	     end
	 | T.FArrayTy (t', n) => CatMap (mkFlOp t')
	 | T.ConTy (ts, c) => 
	     if isGroundTy r then 
               ID (T.FunTy (fArray r, fArray r))
	     else 
	       raise Fail "todo"
	 | T.VarTy a => raise Fail "todo"
	 | _ => raise Fail ("mkFlOp: " ^ U.toString r)
	(* end case *))
    in
      f
    end
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
  (* typeOf : fl_op -> T.ty *)
  (* type reconstruction for opers *)
    fun typeOf (q : fl_op) : T.ty =
     (case q 
       of ID t => t
	| Unzip t => t
	| CatMap q' => (case typeOf q'
            of T.FunTy (T.FArrayTy (r, n), T.FArrayTy (r', n')) =>
                 T.FunTy (T.FArrayTy (T.FArrayTy (r, n), T.LfTy),
			  T.FArrayTy (r', T.NdTy n))
	     | t => raise Fail ("typeOf: " ^ U.toString t))
	| Compose (q1, q2) => let
	    val t1 = typeOf q1
	    val t2 = typeOf q2
	    in
	    (* check here... *)
	      if (U.same (domOf t1, rngOf t2)) then
                T.FunTy (domOf t2, rngOf t1)
	      else
                raise Fail "typeOf: compose mismatch"
            end
	| CrossCompose qs => let
	    val (ds, rs) = unzip (List.map typeOf qs)
            in
	      T.FunTy (T.TupleTy ds, T.TupleTy rs)
	    end
     (* end case *))
  end (* local *)

(* same : fl_op * fl_op -> bool *)
  fun same (o1 : fl_op, o2 : fl_op) : bool = (case (o1, o2)
    of (ID t1, ID t2) => U.same (t1, t2)
     | (Unzip t1, Unzip t2) => U.same (t1, t2)
     | (CatMap o1, CatMap o2) => same (o1, o2)
     | (Compose (op11, op12), Compose (op21, op22)) =>
         same (op11, op21) andalso same (op12, op22)
     | (CrossCompose os1, CrossCompose os2) =>
         ListPair.allEq same (os1, os2)
     | _ => false)

(* compare : oper * oper -> order *)
(* for use in ORD_KEY-based collections *)
  local
    fun consIndex (c : fl_op) : int = (case c
      of ID _           => 0
       | Unzip _        => 1
       | CatMap _       => 2
       | Compose _      => 3
       | CrossCompose _ => 4
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
  (* pairCmp *)
    fun pairCmp (cmp : 'a * 'a -> order) : ('a * 'a) * ('a * 'a) -> order =
     (fn ((p1,p2), (q1,q2)) => (case cmp (p1, q1)
        of EQUAL => cmp (p2, q2)
	 | neq => neq))
  in
    fun compare (o1 : fl_op, o2 : fl_op) : order = let
      fun cmp (o1, o2) = let
        val (i1, i2) = (consIndex o1, consIndex o2)
        in
          if i1 <> i2 then Int.compare (i1, i2)
	  else case (o1, o2)
            of (ID t1, ID t2) => U.compare (t1, t2)
	     | (Unzip t1, Unzip t2) => U.compare (t1, t2)
	     | (CatMap o1, CatMap o2) => cmp (o1, o2)
	     | (Compose pair1, Compose pair2) =>
                 (pairCmp cmp) (pair1, pair2)
	     | (CrossCompose os1, CrossCompose os2) => 
                 (listCmp cmp) (os1, os2)
	     | _ => raise Fail "BUG!" (* shouldn't happen ever *)
        end
      in
        cmp (o1, o2)
      end
  end (* local *)

  structure OperKey : ORD_KEY = struct
    type ord_key = fl_op
    val compare = compare
  end

  structure Map = RedBlackMapFn(OperKey)

  structure Set = RedBlackSetFn(OperKey)

end
