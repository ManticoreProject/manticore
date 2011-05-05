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

(* structure AU = ASTUtil -- can't include ASTUtil, cyclic deps *)

  val commas = String.concatWith ","

  infixr -->
  fun domTy --> rngTy = T.FunTy (domTy, rngTy)

  fun `f x = (fn y => f (x, y))

  local
    fun groundCon c = List.exists (`TyCon.same c) B.primTycs
  in
    fun isGroundTy (T.ConTy ([], c)) = groundCon c
      | isGroundTy (unitTy as T.TupleTy []) = true
      | isGroundTy _ = false
  end (* local *)

  local
    fun tos s t = String.concat [s, "_{", TU.toString t, "}"]
    fun $f xs = List.map f xs
  in
    val toString : A.parray_op -> string = let
      fun ps (A.PSub_Nested t) = tos "PSub_Nested" t
	| ps (A.PSub_Flat t) = tos "PSub_Flat" t
	| ps (A.PSub_Tuple os) = 
	    String.concat ["PSub_Tuple[",
			   commas ($ps os),
			   "]"]
      fun pop (A.PA_Length t) = tos "PA_Length" t
	| pop (A.PA_Sub s) = "PA_Sub_{" ^ ps s ^ "}"
	| pop (A.PA_Tab t) = tos "PA_Tab" t
	| pop (A.PA_TabFTS t) = tos "PA_TabFTS" t
	| pop (A.PA_TabTupleFTS ts) = 
            "PA_TabTupleFTS_{" ^ commas ($TU.toString ts) ^ "}"
	| pop (A.PA_Map t) = tos "PA_Map" t
	| pop (A.PA_Reduce t) = tos "PA_Reduce" t				  
	| pop (A.PA_Range t) = tos "PA_Range" t
	| pop (A.PA_App t) = tos "PA_App" t
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
    fun pop (A.PA_Length t) = (t --> B.intTy)
      | pop (A.PA_Sub s) = ps s
      | pop (A.PA_Tab eltTy) = let
	  val domTy = T.TupleTy [B.intTy, B.intTy --> eltTy]
	  val rngTy = T.FArrayTy (eltTy, T.LfTy)
          in
	    domTy --> rngTy
	  end
      | pop (A.PA_TabFTS eltTy) = let
          val i = B.intTy
          val domTy = T.TupleTy [i, i, i, i --> eltTy]
	  val rngTy = T.FArrayTy (eltTy, T.LfTy)
          in
	    domTy --> rngTy
	  end
      | pop (A.PA_TabTupleFTS ts) = let
          val i = B.intTy
	  val eltTy = T.TupleTy ts
	  val domTy = T.TupleTy [i, i, i, i --> eltTy]
	  val rngTy = T.TupleTy (List.map (fn t => T.FArrayTy (t, T.LfTy)) ts)
          in
	    domTy --> rngTy
	  end
      | pop (A.PA_Map t) = (case t
          of T.FunTy (domTy, rngTy) => (case domTy
               of T.TupleTy ts => raise Fail "todo"
		| T.ConTy (ts, c) => 
                    if isGroundTy domTy then let
                      fun f t = T.FArrayTy (t, T.LfTy)
                      in
                        (domTy --> rngTy) --> ((f domTy) --> (f rngTy))
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
      | pop (A.PA_App eltTy) = let
          val domTy = eltTy --> B.unitTy
	  val rngTy = T.FArrayTy (eltTy, T.LfTy) --> B.unitTy
          in
	    domTy --> rngTy
	  end
    in
      pop
    end				 

(* compare : parray_op * parray_op -> order *)
(* for use in ORD_KEY-based collections *)
  local

    fun consIndexPS (A.PSub_Nested _) = 0
      | consIndexPS (A.PSub_Flat _)   = 1
      | consIndexPS (A.PSub_Tuple _)  = 2

    fun consIndex (A.PA_Length _)        = 0
      | consIndex (A.PA_Sub _)           = 1
      | consIndex (A.PA_Tab _)           = 2
      | consIndex (A.PA_TabFTS _)        = 3
      | consIndex (A.PA_Map _)           = 4
      | consIndex (A.PA_Reduce _)        = 5
      | consIndex (A.PA_Range _)         = 6
      | consIndex (A.PA_App _)           = 7
      | consIndex (A.PA_TabTupleFTS _)   = 8
  in

    val compare : A.parray_op * A.parray_op -> order = let

      fun $ cmp (xs, ys) = List.collate cmp (xs, ys)

      fun ps (o1, o2) = let
        val (i1, i2) = (consIndexPS o1, consIndexPS o2)
        in
          if (i1 <> i2) then Int.compare (i1, i2)
	  else case (o1, o2)
            of (A.PSub_Nested t1, A.PSub_Nested t2) => TU.compare (t1, t2)
	     | (A.PSub_Flat t1, A.PSub_Flat t2) => TU.compare (t1, t2)
	     | (A.PSub_Tuple os1, A.PSub_Tuple os2) => $ps (os1, os2)
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
	     | (A.PA_TabFTS t1, A.PA_TabFTS t2) => TU.compare (t1, t2)
	     | (A.PA_TabTupleFTS ts1, A.PA_TabTupleFTS ts2) => $TU.compare (ts1, ts2)
	     | (A.PA_Map t1, A.PA_Map t2) => TU.compare (t1, t2)
	     | (A.PA_Reduce t1, A.PA_Reduce t2) => TU.compare (t1, t2)
	     | (A.PA_Range t1, A.PA_Range t2) => TU.compare (t1, t2)
	     | (A.PA_App t1, A.PA_App t2) => TU.compare (t1, t2)
	     | _ => raise Fail "compiler bug"
        end
      in
        pop
      end

  end (* local *)

  val same : A.parray_op * A.parray_op -> bool = (fn ops => compare ops = EQUAL)

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
      | mkPS t = raise Fail ("constructSub(loc0): unexpected type " ^ TU.toString t)
    fun mk (t as T.TupleTy [t', i]) =
          if TU.same (B.intTy, i) then 
            A.PA_Sub (mkPS t')
	  else 
	    raise Fail ("constructSub(loc1): unexpected type " ^ TU.toString t)
      | mk t = raise Fail ("constructSub(loc2): unexpected type " ^ TU.toString t)
    in
      A.PArrayOp o mk
    end

(* constructTab : ty -> exp *)
  local
    val isInt = (fn t => TU.same (t, B.intTy))
    fun groundPairWitness (T.TupleTy [t1, t2]) = 
          if isGroundTy t1 andalso isGroundTy t2 then SOME (t1, t2) else NONE
      | groundPairWitness _ = NONE
    fun groundPair (g1, g2) = A.PArrayOp (A.PA_Tab (T.TupleTy [g1, g2]))
  in
    val constructTab : T.ty -> A.exp = let
      fun mk (domTy as T.TupleTy [i1, T.FunTy (i2, eltTy)]) =
            if not (isInt i1) orelse not (isInt i2) then
              raise Fail ("unexpected ty (ints expected) " ^ TU.toString domTy)
	    else (case groundPairWitness eltTy
              of SOME (g1, g2) => groundPair (g1, g2)
	       | NONE => let 
                   val eltsTy = T.FArrayTy (eltTy, T.LfTy)          
		   val fl = FlattenOp.construct eltTy
    		   val rngTy = TU.rangeType (FlattenOp.typeOf fl)
    		   val tab = A.PArrayOp (A.PA_Tab eltTy)
    		   val arg = Var.new ("arg", domTy)
		   (* note: in what follows, I cannot use ASTUtil.mkApplyExp *)
    		   (*   b/c referring to ASTUtil induces cyclic deps *)
    		   val body = A.ApplyExp (A.FlOp fl,
    					  A.ApplyExp (tab, A.VarExp (arg, []), eltsTy),
    					  rngTy)
                   in
                     A.FunExp (arg, body, rngTy)
                   end
              (* end case *))
	| mk t = raise Fail ("unexpected ty " ^ TU.toString t)
    in
      mk
    end
  end (* local *)

(* constructTabFTS : ty -> exp *)
  local
    val isInt = (fn t => TU.same (t, B.intTy))
    fun groundPairWitness (T.TupleTy [t1, t2]) = 
          if isGroundTy t1 andalso isGroundTy t2 then SOME (t1, t2) else NONE
      | groundPairWitness _ = NONE
    fun groundPair (g1, g2) = A.PArrayOp (A.PA_TabTupleFTS [g1, g2])
  in
    val constructTabFTS : T.ty -> A.exp = let
      fun mk (domTy as T.TupleTy [i1, i2, i3, T.FunTy (i4, eltTy)]) =
            if not (List.all isInt [i1, i2, i3, i4]) then
              raise Fail ("unexpected ty (ints expected) " ^ TU.toString domTy)
	    else (case groundPairWitness eltTy
              of SOME (g1, g2) => groundPair (g1, g2)
	       | NONE => let 
                   val eltsTy = T.FArrayTy (eltTy, T.LfTy)          
		   val fl = FlattenOp.construct eltTy
    		   val rngTy = TU.rangeType (FlattenOp.typeOf fl)
    		   val tab = A.PArrayOp (A.PA_TabFTS eltTy)
    		   val arg = Var.new ("arg", domTy)
		   (* note: in what follows, I cannot use ASTUtil.mkApplyExp *)
    		   (*   b/c referring to ASTUtil induces cyclic deps *)
    		   val body = A.ApplyExp (A.FlOp fl,
    					  A.ApplyExp (tab, A.VarExp (arg, []), eltsTy),
    					  rngTy)
                   in
		     A.FunExp (arg, body, rngTy)
                   end
              (* end case *))
	| mk t = raise Fail ("unexpected ty " ^ TU.toString t)
    in
      mk
    end
  end (* local *)

  fun isIntTy t = TU.same (B.intTy, t)
  fun isIntParrayTy t = TU.same (t, B.parrayTy B.intTy)

(* constructMap : ty -> exp *)
  val constructMap : T.ty -> A.exp = let
(* FIXME: not working properly for [| [| 1 to 10 |] | r in v |] (where v = [| 1 to 10 |]) *)
    fun mk (ft as T.FunTy (domTy, rngTy)) = 
          if isIntTy domTy andalso isIntParrayTy rngTy andalso false (* !!!! *) then let
            in
              raise Fail "todo: int -> int parray"
            end
	  else let
            val pr = fn ss => (print (String.concat ss); print "\n")
            val _ = pr ["called constructMap on ", TU.toString ft]
            val fl = FlattenOp.construct rngTy
	    val _ = pr ["fl is ", FlattenOp.toString fl]
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
    fun isGroundPair t = (case t
      of T.TupleTy [t1, t2] => isGroundTy t1 andalso isGroundTy t2
       | _ => false
      (* end case *))
    fun mk (operTy as T.FunTy (T.TupleTy [t1, t2], t3)) =
          if TU.same (t1, t2) andalso TU.same (t2, t3) then
            (if isGroundTy t1 orelse isGroundPair t1 then
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

(* have to copy some of ASTUtil here; cyclic deps :-( *)
  fun mkIntLit n = Literal.Int (IntInf.fromInt n)
  fun mkIntConst n = A.LConst (mkIntLit n, Basis.intTy)
  fun mkIntPat n = A.ConstPat (mkIntConst n)
  fun mkInt n = A.ConstExp (mkIntConst n)

  fun plusOne n = A.ApplyExp (A.VarExp (B.int_plus, []),
			      A.TupleExp [n, mkInt 1],
			      B.intTy)

  fun mkApply resTy = let
    fun app (e, []) = raise Fail "mkApply.app"
      | app (e, [x]) = A.ApplyExp (e, x, resTy)
      | app (e, xs) = A.ApplyExp (e, A.TupleExp xs, resTy)
    in
      app
    end
			
  val intApp = mkApply B.intTy
  val boolApp = mkApply B.boolTy
  val unitApp = mkApply B.unitTy

(* constructApp : ty -> exp *)
(* At the moment, the supported types are ground types, parrays of ground types, *)
(*   and tuples of supported types. *)
(* I don't yet do datatypes. The problem is I need to do some type flattening here *)
(*   and I'm not ready to flatten datatypes here in ast/. It's doable, but I'm leaving it *)
(*   to the future for now. *)
  local
    val supportedTy : T.ty -> bool = let
      fun s t = 
        if isGroundTy t then 
          true
	else (case t
	  of T.FArrayTy (t', _) => isGroundTy t'
	   | T.TupleTy ts => List.all s ts
	   | _ => false
	  (* end case *))
      in
	s
      end
  (* a function to "lift" supported types to parrays in a particular way -- *)
  (* - ground types are lifted to flat arrays of those types *)
  (* - arrays of ground types are lifted one level of depth *)
  (* - tuples of supported types are lifted to tuples of lifted types *)
  (* ex: int --> FArray (inf, Lf)                              *)
  (* ex: FArray (int, Lf) --> FArray (int, Nd Lf)              *)
  (* ex: (int * int) --> (FArray (int, Lf) * FArray (int, Lf)) *)
  (*   (as opposed to FArray (int * int, Lf))                  *)
    val lift : T.ty -> T.ty = let
      fun l t =
        if isGroundTy t then
          T.FArrayTy (t, T.LfTy)
	else (case t
          of T.FArrayTy (t', n) => T.FArrayTy (t', T.NdTy n)
	   | T.TupleTy ts => T.TupleTy (List.map l ts)
	   | _ => raise Fail ("lift: unexpected type " ^ TU.toString t)
          (* end case *))
      in
	l
      end
  in
  (* constructApp : ty -> exp *)
  (* For each use of app, this code will synthesize a monomorphic function as follows:
       let fun app (f : t -> unit) = let
         fun f' arr = let
           val n = PArray.length arr
           fun lp i = if (i >= n) then ()
	              else (f (arr!i)); lp (i+1))
           in lp 0 end
         in f' end
      in app end 
    for some monomorphic type t and specialized PArray.length and !.
  *)
  (* TODO don't generate this --every-- time it's needed... *)
    val constructApp : T.ty -> A.exp = let 
      fun mk (t as T.FunTy (eltTy, uTy)) =
            if not (TU.same (B.unitTy, uTy)) then 
              raise Fail ("unexpected type " ^ TU.toString t)
            else if not (supportedTy eltTy) then
              raise Fail ("constructApp: unsupported type " ^ TU.toString t)
	    else (* generate custom app function *) let
(* val _ = print ("GGGGG generating custom app for elt ty " ^ TU.toString eltTy ^ "\n") *)
	      fun v x = A.VarExp (x, [])
	      val eltTy' = lift eltTy
              val lenExp = constructLength eltTy'
	      val subExp = constructSub (T.TupleTy [eltTy', B.intTy])
	      val app = Var.new ("app", (eltTy --> B.unitTy) --> (eltTy' --> B.unitTy))
	      val f = Var.new ("f", eltTy --> B.unitTy)
	      val f' = Var.new ("f'", eltTy' --> B.unitTy)
	      val arr = Var.new ("arr", eltTy')
	      val n = Var.new ("n", B.intTy)
	      val lp = Var.new ("lp", B.intTy --> B.unitTy)
	      val i = Var.new ("i", B.intTy)
	      val subi = mkApply eltTy (subExp, [v arr, v i])
	      val seq = A.SeqExp (unitApp (v f, [subi]), intApp (v lp, [plusOne (v i)]))
	      val lpTest = boolApp (v B.int_gte, [v i, v n])
              val lpBody = A.IfExp (lpTest, A.TupleExp [], seq, B.unitTy)		
	      val lpLam = A.FB (lp, i, lpBody)
	      val lpBind = A.FunBind [lpLam]
	      val nBind = A.ValBind (A.VarPat n, intApp (lenExp, [v arr]))
	      val f'Body = A.LetExp (nBind, A.LetExp (lpBind, unitApp (v lp, [mkInt 0])))
	      val f'Lam = A.FB (f', arr, f'Body)
	      val appBody = A.LetExp (A.FunBind [f'Lam], v f')
	      val appLam = A.FB (app, f, appBody)
	      in
                A.LetExp (A.FunBind [appLam], v app)
	      end
	| mk t = raise Fail ("constructApp: unexpected type " ^ TU.toString t)
      in
        mk
      end
  end (* local *)
        
end
