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

  val commas = String.concatWith ","

  val toString : A.parray_op -> string = let
    fun ps (A.PSub_Nested t) = "PSub_Nested_{" ^ TU.toString t ^ "}"
      | ps (A.PSub_Flat t) = "PSub_Flat_{" ^ TU.toString t ^ "}"
      | ps (A.PSub_Tuple os) = concat ["PSub_Tuple[",
				       commas (List.map ps os),
				       "]"]
    fun pop (A.PA_Length t) = "PSub_Length_{" ^ TU.toString t ^ "}"
      | pop (A.PA_Sub s) = ps s
    in
      pop      
    end

  val typeOf : A.parray_op -> T.ty = let
    fun mk d r = T.FunTy (T.TupleTy [d, B.intTy], r)
    fun ps (A.PSub_Nested t) = (case t
          of T.FArrayTy (t', T.NdTy n) => mk t (T.FArrayTy (t', n))
	   | _ => raise Fail ("ps " ^ TU.toString t)
          (* end case *))
      | ps (A.PSub_Flat t) = (case t
          of T.FArrayTy (t', T.LfTy) => mk t t'
	   | _ => raise Fail ("ps " ^ TU.toString t)
          (* end case *))
      | ps (A.PSub_Tuple os) = let
          val ts = List.map ps os
	  val ds = List.map TU.domainType ts
	  fun fst (T.TupleTy [t1, t2]) = t1
	    | fst _ = raise Fail "compiler bug"
	  val fs = List.map fst ds
	  val rs = List.map TU.rangeType ts
          in
	    mk (T.TupleTy fs) (T.TupleTy rs)
	  end
    fun pop (A.PA_Length t) = T.FunTy (t, B.intTy)
      | pop (A.PA_Sub s) = ps s
    in
      pop
    end				 

  val same : A.parray_op * A.parray_op -> bool = let
    fun ps (A.PSub_Nested t1, A.PSub_Nested t2) = TU.same (t1, t2)
      | ps (A.PSub_Flat t1, A.PSub_Flat t2) = TU.same (t1, t2)
      | ps (A.PSub_Tuple ts1, A.PSub_Tuple ts2) = 
          ListPair.allEq ps (ts1, ts2)
      | ps _ = false
    fun pop (A.PA_Length t1, A.PA_Length t2) = TU.same (t1, t2)
      | pop (A.PA_Sub s1, A.PA_Sub s2) = ps (s1, s2)
      | pop _ = false
    in
      pop
    end

(* compare : parray_op * parray_op -> order *)
(* for use in ORD_KEY-based collections *)
  local
    fun consIndexPS (A.PSub_Nested _) = 0
      | consIndexPS (A.PSub_Flat _) = 1
      | consIndexPS (A.PSub_Tuple _) = 2
    fun consIndex (A.PA_Length _) = 0
      | consIndex (A.PA_Sub _) = 1
  in
    val compare : A.parray_op * A.parray_op -> order = let
      fun ps (o1, o2) = let
        val (i1, i2) = (consIndexPS o1, consIndexPS o2)
        in
          if (i1 <> i2) then Int.compare (i1, i2)
	  else case (o1, o2)
            of (A.PSub_Nested t1, A.PSub_Nested t2) => TU.compare (t1, t2)
	     | (A.PSub_Flat t1, A.PSub_Flat t2) => TU.compare (t1, t2)
	     | (A.PSub_Tuple os1, A.PSub_Tuple os2) => pss (os1, os2)
	     | _ => raise Fail "compiler bug"
         end
      and pss (os1, os2) = let
        fun lp ([], []) = EQUAL
	  | lp (_::_, []) = GREATER
	  | lp ([], _::_) = LESS
	  | lp (q::qs, r::rs) = (case ps (q, r)
              of EQUAL => lp (qs, rs)
	       | neq => neq
              (* end case *))               
        in 
	  lp (os1, os2)
        end
      fun pop (o1, o2) = let
        val (i1, i2) = (consIndex o1, consIndex o2)
        in
          if (i1 <> i2) then Int.compare (i1, i2)
	  else case (o1, o2)
            of (A.PA_Length t1, A.PA_Length t2) => TU.compare (t1, t2)
	     | (A.PA_Sub s1, A.PA_Sub s2) => ps (s1, s2)
	     | _ => raise Fail "compiler bug"
        end
      in
        pop
      end
  end (* local *)

  structure OperKey : ORD_KEY = struct
    type ord_key = A.parray_op
    val compare = compare
  end

  structure Map = RedBlackMapFn(OperKey)

  structure Set = RedBlackSetFn(OperKey)

(* constructLength : ty -> parray_op *)
  fun constructLength t = A.PA_Length t

(* constructSub : ty -> parray_op *)
  val constructSub : T.ty -> A.parray_op = let
    fun mkPS (T.TupleTy ts) = A.PSub_Tuple (List.map mkPS ts)
      | mkPS (t as T.FArrayTy (_, s)) = (case s
          of T.LfTy => A.PSub_Flat t
	   | T.NdTy _ => A.PSub_Nested t
          (* end case *))
      | mkPS t = raise Fail ("unexpected type " ^ TU.toString t)
    fun mk (t as T.TupleTy [t', i]) =
          if TU.same (B.intTy, i) 
	    then A.PA_Sub (mkPS t')
	    else raise Fail ("unexpected type " ^ TU.toString t)
      | mk t = raise Fail ("unexpected type " ^ TU.toString t)
    in
      mk
    end

end
