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

  val construct : Types.ty -> AST.fl_op
  val typeOf    : AST.fl_op -> Types.ty
  val same      : AST.fl_op * AST.fl_op -> bool
  val compare   : AST.fl_op * AST.fl_op -> order
  val toString  : AST.fl_op -> string

  structure Map : ORD_MAP where type Key.ord_key = AST.fl_op
  structure Set : ORD_SET where type Key.ord_key = AST.fl_op

end = struct

  structure A = AST
  structure T = Types
  structure U = TypeUtil

  fun ntreeNat (T.LfTy) = 0
    | ntreeNat (T.NdTy n) = 1 + ntreeNat n

(* toString *)
  local
    fun sub conStr ty = conStr ^ "_<" ^ U.toString ty ^ ">"
  in
    fun toString (oper : A.fl_op) : string = (case oper
      of A.ID t => sub "A.ID" t
       | A.Unzip t => sub "A.Unzip" t
       | A.Cat t => sub "A.Cat" t
       | A.Map (oper, n) => "A.Map(" ^ toString oper ^ ")_" 
			    ^ Int.toString (ntreeNat n)
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
  (* fl_op : A.fl_op -> T.ty *)
  (* type reconstruction for opers *)
    fun typeOf (q : A.fl_op) : T.ty = (case q 
      of A.ID domTy => T.FunTy (domTy, domTy)
       | A.Unzip domTy => let
           val (ts, n) = (case domTy 
             of T.FArrayTy (T.TupleTy ts, n) => (ts, n)
	      | _ => raise Fail "typeOf: unzip domain")
	   val rngTy = T.TupleTy (List.map (fn ty => T.FArrayTy (ty, n)) ts)
           in
	     T.FunTy (domTy, rngTy)
	   end
       | A.Cat domTy => (case domTy
           of T.FArrayTy (T.FArrayTy (r, n), T.LfTy) => let
                val rngTy = T.FArrayTy (r, T.NdTy n)
                in
		  T.FunTy (domTy, rngTy)
	        end 
	    | _ => raise Fail ("typeOf, Cat: " ^ U.toString domTy)
           (* end case *))
       | A.Map (oper, n) => (case typeOf oper
           of T.FunTy (r, r') => T.FunTy (T.FArrayTy (r, n), T.FArrayTy (r', n))
	    | _ => raise Fail ("typeOf, Map: " ^ toString oper)
           (* end case *))
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

(* construct *)
  local
    fun fArray ty = T.FArrayTy (ty, T.LfTy)
    fun isGroundTy ty = 
     (case ty
       of T.ConTy ([], c) => 
            List.exists (fn c' => TyCon.same (c',c)) Basis.primTycs
	| _ => false)
  in
    fun construct (r : T.ty) : A.fl_op = let
      val f = (case r
        of T.FunTy (r1, r2) => A.ID (fArray r)
	 | T.TupleTy [] => (* unit *) A.ID (fArray r)
	 | T.TupleTy ts => let
             val unzip = A.Unzip (fArray r)
             in
	       A.Compose (A.CrossCompose (List.map construct ts), unzip)
	     end
	 | T.FArrayTy (t', n) => let
             val oper' = construct t'
             val domTy = U.domainType (typeOf oper')
             in
               A.Compose (A.Cat domTy, A.Map (oper', n))
             end
	 | T.ConTy (ts, c) => 
	     if isGroundTy r then 
               A.ID (T.FunTy (fArray r, fArray r))
	     else 
	       raise Fail "todo"
	 | T.VarTy a => raise Fail "todo"
	 | _ => raise Fail ("construct: " ^ U.toString r)
	(* end case *))
    in
      f
    end
  end (* local *)

(* same : A.fl_op * A.fl_op -> bool *)
  fun same (o1 : A.fl_op, o2 : A.fl_op) : bool = (case (o1, o2)
    of (A.ID t1, A.ID t2) => U.same (t1, t2)
     | (A.Unzip t1, A.Unzip t2) => U.same (t1, t2)
     | (A.Cat t1, A.Cat t2) => U.same (t1, t2)
     | (A.Map (o1, n1), A.Map (o2, n2)) =>
         same (o1, o2) andalso (ntreeNat n1 = ntreeNat n2)
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
       | A.Unzip _        => 1
       | A.Cat _          => 2
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
  (* pairCmp *)
    fun pairCmp (cmp : 'a * 'a -> order) : ('a * 'a) * ('a * 'a) -> order =
     (fn ((p1,p2), (q1,q2)) => (case cmp (p1, q1)
        of EQUAL => cmp (p2, q2)
	 | neq => neq))
  in
    fun compare (o1 : A.fl_op, o2 : A.fl_op) : order = let
      fun cmp (o1, o2) = let
        val (i1, i2) = (consIndex o1, consIndex o2)
        in
          if i1 <> i2 then Int.compare (i1, i2)
	  else case (o1, o2)
            of (A.ID t1, A.ID t2) => U.compare (t1, t2)
	     | (A.Unzip t1, A.Unzip t2) => U.compare (t1, t2)
	     | (A.Cat t1, A.Cat t2) => U.compare (t1, t2)
	     | (A.Map (o1, n1), A.Map (o2, n2)) =>
                (case cmp (o1, o2)
		  of EQUAL => Int.compare (ntreeNat n1, ntreeNat n2)
		   | neq => neq)
	     | (A.Compose pair1, A.Compose pair2) =>
                 (pairCmp cmp) (pair1, pair2)
	     | (A.CrossCompose os1, A.CrossCompose os2) => 
                 (listCmp cmp) (os1, os2)
	     | _ => raise Fail "BUG!" (* shouldn't happen ever *)
        end
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

end
