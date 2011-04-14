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

  infixr -->
  fun domTy --> rngTy = T.FunTy (domTy, rngTy)

  local
    fun isg c = List.exists (fn g => TyCon.same (c,g)) B.primTycs
  in
    fun isGroundTy (T.ConTy ([], c)) = isg c
      | isGroundTy _ = false
  end (* local *)

  local
    fun tos s t = String.concat [s, "_{", TU.toString t, "}"]
  in
    val toString : A.parray_op -> string = let
      fun ps (A.PSub_Nested t) = tos "PSub_Nested" t
	| ps (A.PSub_Flat t) = tos "PSub_Flat" t
	| ps (A.PSub_Tuple os) = 
	    String.concat ["PSub_Tuple[",
			   commas (List.map ps os),
			   "]"]
      fun pop (A.PA_Length t) = tos "PA_Length" t
	| pop (A.PA_Sub s) = "PA_Sub_{" ^ ps s ^ "}"
	| pop (A.PA_Tab t) = tos "PA_Tab" t
	| pop (A.PA_Map t) = tos "PA_Map" t
	| pop (A.PA_Reduce t) = tos "PA_Reduce" t				  
	| pop (A.PA_Range t) = tos "PA_Range" t
      in
        pop      
      end
  end (* local *)

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
      | pop (A.PA_Tab t) = let
          val domTy = T.TupleTy [B.intTy, T.FunTy (B.intTy, t)]
	  val rngTy = T.FArrayTy (t, T.LfTy)
          in
	    T.FunTy (domTy, rngTy)
	  end	  
      | pop (A.PA_Map t) = (case t
          of T.FunTy (domTy, rngTy) => (case domTy
               of T.TupleTy ts => raise Fail "todo"
		| T.ConTy (ts, c) => 
                    if isGroundTy domTy then let
                      fun f t = T.FArrayTy (t, T.LfTy)
                      in
                        T.FunTy (T.FunTy (domTy, rngTy),
				 T.FunTy (f domTy, f rngTy))
		      end
		    else
		      raise Fail ("todo " ^ TU.toString t)
		| _ => raise Fail ("todo " ^ TU.toString t)
               (* end case *))
	   | _ => raise Fail ("unexpected ty " ^ TU.toString t)
          (* end case *))	
      | pop (A.PA_Reduce t) =
          if isGroundTy t then let
            val ta = T.FArrayTy (t, T.LfTy)
            in
              (t --> t) --> (t --> (ta --> t))
            end
          else
	    raise Fail ("todo: reductions on " ^ TU.toString t)
      | pop (A.PA_Range t) = let
          val _ = if TU.same (t, B.intTy) then () 
		  else raise Fail ("not int: " ^ TU.toString t)
          in
	    (T.TupleTy [B.intTy, B.intTy, B.intTy]) --> (B.parrayTy B.intTy)
	  end
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
      | pop (A.PA_Tab t1, A.PA_Tab t2) = TU.same (t1, t2)
      | pop (A.PA_Map t1, A.PA_Map t2) = TU.same (t1, t2)
      | pop (A.PA_Reduce t1, A.PA_Reduce t2) = TU.same (t1, t2)
      | pop (A.PA_Range t1, A.PA_Range t2) = TU.same (t1, t2)
      | pop _ = false
    in
      pop
    end

(* compare : parray_op * parray_op -> order *)
(* for use in ORD_KEY-based collections *)
  local

    fun consIndexPS (A.PSub_Nested _) = 0
      | consIndexPS (A.PSub_Flat _)   = 1
      | consIndexPS (A.PSub_Tuple _)  = 2

    fun consIndex (A.PA_Length _) = 0
      | consIndex (A.PA_Sub _)    = 1
      | consIndex (A.PA_Tab _)    = 2
      | consIndex (A.PA_Map _)    = 3
      | consIndex (A.PA_Reduce _) = 4
      | consIndex (A.PA_Range _)  = 5
  in

    val compare : A.parray_op * A.parray_op -> order = let
      fun ps (o1, o2) = let
        val (i1, i2) = (consIndexPS o1, consIndexPS o2)
        in
          if (i1 <> i2) then Int.compare (i1, i2)
	  else case (o1, o2)
            of (A.PSub_Nested t1, A.PSub_Nested t2) => TU.compare (t1, t2)
	     | (A.PSub_Flat t1, A.PSub_Flat t2) => TU.compare (t1, t2)
	     | (A.PSub_Tuple os1, A.PSub_Tuple os2) => List.collate ps (os1, os2)
	     | _ => raise Fail "compiler bug"
         end

      fun pop (o1, o2) = let
        val (i1, i2) = (consIndex o1, consIndex o2)
        in
          if (i1 <> i2) then Int.compare (i1, i2)
	  else case (o1, o2)
            of (A.PA_Length t1, A.PA_Length t2) => TU.compare (t1, t2)
	     | (A.PA_Sub s1, A.PA_Sub s2) => ps (s1, s2)
	     | (A.PA_Tab t1, A.PA_Tab t2) => TU.compare (t1, t2)
	     | (A.PA_Map t1, A.PA_Tab t2) => TU.compare (t1, t2)
	     | (A.PA_Reduce t1, A.PA_Reduce t2) => TU.compare (t1, t2)
	     | (A.PA_Range t1, A.PA_Range t2) => TU.compare (t1, t2)
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

(* constructLength : ty -> exp *)
  fun constructLength t = A.PArrayOp (A.PA_Length t)

(* constructSub : ty -> exp *)
  val constructSub : T.ty -> A.exp = let
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
      A.PArrayOp o mk
    end

(* constructTab : ty -> exp *)
  val constructTab : T.ty -> A.exp = let
    fun int t = TU.same (t, B.intTy)
    fun mk (domTy as T.TupleTy [i1, T.FunTy (i2, eltTy)]) =
          if int i1 andalso int i2 then let
            val eltsTy = T.FArrayTy (eltTy, T.LfTy)
            val fl = FlattenOp.construct eltTy
	    val rngTy = (case FlattenOp.typeOf fl
              of T.FunTy (_, r) => r
	       | _ => raise Fail "compiler bug"
              (* end case *))
	    val tab = A.PArrayOp (A.PA_Tab domTy)
	    val arg = Var.new ("arg", domTy)
          (* note: in what follows, I cannot use ASTUtil.mkApplyExp *)
	  (*   b/c referring to ASTUtil induces cyclic deps *)
	    val body = A.ApplyExp (A.FlOp fl, 
				   A.ApplyExp (tab, A.VarExp (arg, []), eltsTy),
				   rngTy)
            in
              A.FunExp (arg, body, rngTy)
	    end
	  else
            raise Fail ("unexpected ty (ints expected) " ^ TU.toString domTy)
      | mk t = raise Fail ("unexpected ty " ^ TU.toString t)
    in
      mk
    end

(* constructMap : ty -> exp *)
  val constructMap : T.ty -> A.exp = let
    fun mk (ft as T.FunTy (domTy, rngTy)) = let
          val fl = FlattenOp.construct rngTy
	  val flRngTy = (case FlattenOp.typeOf fl
            of T.FunTy (_, r) => r
	     | t => raise Fail ("unexpected ty " ^ TU.toString t)
            (* end case *))
	  fun a t = T.FArrayTy (t, T.LfTy)
	  val f = Var.new ("f", ft)
	  val arr = Var.new ("arr", a domTy)
	  val mapOp = A.PArrayOp (A.PA_Map ft)
        (* note: in what follows, I cannot use ASTUtil.mkApplyExp *)
	(*   b/c referring to ASTUtil induces cyclic deps *)
	(* here I am building the following term: *)
        (*   fn f => fn arr => fl (map f arr) *)
	  val innerApp0 = A.ApplyExp (mapOp, A.VarExp (f, []), A.FunTy (a domTy, a rngTy))
	  val innerApp1 = A.ApplyExp (innerApp0, A.VarExp (arr, []), a rngTy)
	  val innerApp2 = A.ApplyExp (A.FlOp fl, innerApp1, flRngTy)
	  val innerFn = A.FunExp (arr, innerApp2, flRngTy)
	  val outerFn = A.FunExp (f, innerFn, T.FunTy (a domTy, flRngTy))
          in
            outerFn
          end
      | mk t = raise Fail ("unexpected ty " ^ TU.toString t) 
    in
      mk
    end

(* constructReduce : ty -> exp *)
  val constructReduce : T.ty -> A.exp = let
    fun mk (operTy as T.FunTy (T.TupleTy [t1, t2], t3)) =
          if TU.same (t1, t2) andalso TU.same (t2, t3) then
           (if isGroundTy t1 then
              A.PArrayOp (A.PA_Reduce t1)
	    else
              raise Fail ("todo: reduce for type " ^ TU.toString t1))
	  else
            raise Fail ("cannot be an associative operator with type " ^ 
			TU.toString operTy)
      | mk t = raise Fail ("unexpected type " ^ TU.toString t)
    in
      mk
    end

(* constructRange : ty -> exp *)
  val constructRange : T.ty -> A.exp = let
    fun mk (t as T.TupleTy (ts as [t1, t2, t3])) = 
          if List.all (fn t => TU.same (t, B.intTy)) ts then
            A.PArrayOp (A.PA_Range B.intTy)
	  else            
            raise Fail ("unexpected type " ^ TU.toString t)
      | mk t = raise Fail ("unexpected type " ^ TU.toString t)
    in
      mk
    end

end
