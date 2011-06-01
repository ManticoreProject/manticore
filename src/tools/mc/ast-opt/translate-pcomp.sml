(* translate-pcomp.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TranslatePComp : sig

  (* An AST to AST translation of parallel comprehensions. *)
    val tr : (AST.exp -> AST.exp) 
             -> AST.exp * (AST.pat * AST.exp) list * AST.exp option 
             -> AST.exp

  end  = struct

    structure A = AST
    structure B = Basis
    structure T = Types
    structure AU = ASTUtil
    structure TU = TypeUtil

    structure DV = DelayedBasis.Var

    fun dup (n, x) = List.tabulate (n, fn i => x)

  (* tab2DMatch : exp * (pat * exp) list * exp option -> (exp^7) option *)
  (* This function identifies pcomps of the form *)
  (* [| [| e' | j in rng1 |] | i in rng2 |]  *)
  (* which will be compiled specially. *)
  (* The variables i and j are assumed to be of type int (and checked as such). *)
    fun tab2DMatch (e, pes, oe) = (case e
      of A.PCompExp (e', [(A.VarPat j, A.RangeExp r1)], NONE) => (case pes
           of [(A.VarPat i, A.RangeExp r2)] => (case oe
	        of NONE => let
                     val _ = print "***** FOUND TAB2D!\n"
		     val ij = Var.new ("ij", A.TupleTy [B.intTy, B.intTy])
		     val m = A.PatMatch (A.TuplePat [A.VarPat i, A.VarPat j], e')
		     val body = AU.mkCaseExp (A.VarExp (ij, []), [m])
		     val f_ij = AU.mkFunExp (ij, body)
		     in
		       collect (f_ij, r2, r1)
		     end
		 | _ => NONE)
	    | _ => NONE)
       | _ => NONE)
    and collect (f_ij, (iFrom, iTo, iStepOpt, iTy), (jFrom, jTo, jStepOpt, jTy)) = let
      val _ = if TU.same (iTy, B.intTy) then () else raise Fail "i is not an int"
      val _ = if TU.same (jTy, B.intTy) then () else raise Fail "j is not an int"
      val iStep = Option.getOpt (iStepOpt, AU.one)
      val jStep = Option.getOpt (jStepOpt, AU.one)
      in
        SOME (iFrom, iTo, iStep, jFrom, jTo, jStep, f_ij)
      end

  (* regularD : exp * (pat * exp) list * exp option -> (exp * exp * exp) list option *)
  (* This is the generalization of tab2D above. *)
  (* If a pcomp is a regular tab of the form *)
  (*   [| ... [| e | i_1 in rng_1 |] ... | i_k in rng_k |]*)
  (* this function returns the "range triples" of the ranges 1 through k. *)
  (* A "range triple" is a triple of ints, representing from, to, and step. *)
  (* Each range must be "rigid" in the sense that its range triple consists *)
  (*   only of constants and/or variables. *)
  (* TODO: This only catches ranges -- it's too conservative. *)
    fun regularD (e, pes, oe) = let
      fun lp (A.PCompExp (e, [(p, A.RangeExp rng)], NONE), vars) = let
            val (iFrom, iTo, iStepOpt, ty) = rng
            val i = (case p
              of A.VarPat i => i
	       | A.WildPat _ => Var.new ("dummy", ty)
	       | A.ConstPat _ => raise Fail "todo: const pat matching range (odd case, but fair)"
	       | _ => raise Fail ("unexpected pat in pcomp (can it match an int?): " ^ 
				  AU.patToString p)
              (* end case *))
            val iStep = Option.getOpt (iStepOpt, AU.one)
            in case lp (e, vars)
              of NONE => NONE
	       | SOME (tups, vars, e') => SOME ((iFrom, iTo, iStep)::tups, i::vars, e')
            end
	| lp (A.PCompExp _, _) = NONE
	| lp (innermostExp, vars) = SOME ([], vars, innermostExp)
      in case lp (A.PCompExp (e, pes, oe), [])
        of NONE => NONE
	 | SOME (tups, vars, innermostExp) => (case (tups, vars)
             of ([(from,to,step)], [i]) => let
                  val f = AU.mkFunExp (i, innermostExp)
                  in
                    SOME (tups, f)
		  end
	      | _ => let
		  val argTy = A.TupleTy (dup (List.length vars, B.intTy))
                  val arg = Var.new ("arg", argTy)
		  val m = A.PatMatch (A.TuplePat (List.map A.VarPat vars), innermostExp)
		  val body = AU.mkCaseExp (A.VarExp (arg, []), [m])
		  val f = AU.mkFunExp (arg, body)
		  in
                    SOME (tups, f)
                  end
             (* end case *))
      end                   

  (* mkRegularTab : (exp * exp * exp) list * exp * ty -> exp *)
    fun mkRegularTab (triples, f, outTy) = let
      fun tabExp t = A.VarExp (t (), [outTy])
      fun triple (e1, e2, e3) = AU.mkTupleExp [e1, e2, e3]
      val (tab, args) = (case triples
        of [] => raise Fail "bug: empty list of triples"
	 | [(from,to,step)] => let 
             val tab = tabExp DV.parrayTabFTS
             in
               (tab, [from, to, step, f])
             end
	 | [(f1,t1,s1),(f2,t2,s2)] => let
             val tab2D = tabExp DV.parrayTab2D
             in
	       (tab2D, [triple(f1,t1,s1),
			triple(f2,t2,s2),
			f])
	     end
	 | [(f1,t1,s1),(f2,t2,s2),(f3,t3,s3)] => let
             val tab3D = tabExp DV.parrayTab3D
             in
               (tab3D, [triple(f1,t1,s1),
			triple(f2,t2,s2),
			triple(f3,t3,s3),
			f])
             end
	 | [(f1,t1,s1),(f2,t2,s2),(f3,t3,s3),(f4,t4,s4)] => let
             val tab4D = tabExp DV.parrayTab4D
             in
               (tab4D, [triple(f1,t1,s1),
			triple(f2,t2,s2),
			triple(f3,t3,s3),
			triple(f4,t4,s4),
			f])
             end

	 | [(f1,t1,s1),(f2,t2,s2),(f3,t3,s3),(f4,t4,s4),(f5,t5,s5)] => let
             val tab5D = tabExp DV.parrayTab5D
             in
               (tab5D, [triple(f1,t1,s1),
			triple(f2,t2,s2),
			triple(f3,t3,s3),
			triple(f4,t4,s4),
			triple(f5,t5,s5),
			f])
             end
	 | _ => raise Fail ("todo: regular tabs of " ^ 
			    Int.toString (List.length triples) ^ 
			    "dimensions"))
      in
        AU.mkApplyExp (tab, args)
      end

  (* tr : (exp -> exp) -> exp * (pat * exp) list * exp option -> exp *)
    fun tr trExp (e, pes, oe) = (case regularD (e, pes, oe)
      of SOME (triples, f) => mkRegularTab (triples, f, TypeOf.exp e)
       | NONE => (case (pes, oe)
           of ([], _) => raise Fail "a parallel comprehension with no pbinds at all"
	    | ([(p1, e1)], optPred) => let (* the one pbind, no predicate case *)
                val eltTy = TypeOf.exp e
		val patTy = TypeOf.pat p1
		val x1 = Var.new ("x1", patTy)
		val e' = trExp e
		val c1 = AU.mkCaseExp (A.VarExp (x1, []), [A.PatMatch (p1, e')])
		val f = AU.mkFunExp (x1, c1)
		val e1' = trExp e1
		val mapP = A.VarExp (DV.parrayMap (), [eltTy, patTy]) 
                (* NOTE: these type args seem backwards to me, but I've tested this. - ams*)
		fun map arr = AU.mkCurriedApplyExp (mapP, [f, arr])
                in case optPred
		  of NONE => map e1'
		   | SOME pred => raise Fail "todo(1)" (* see COMMENT 1 below *) 
		end
	    | ([(p1, e1), (p2, e2)], NONE) => let
                val eltTy = TypeOf.exp e
		val p1Ty = TypeOf.pat p1
		val p2Ty = TypeOf.pat p2
		val arg = Var.new ("arg", T.TupleTy [p1Ty, p2Ty])
		val body = AU.mkCaseExp (A.VarExp (arg, []),
		  [A.PatMatch (A.TuplePat [p1, p2], e)])
		val f = AU.mkFunExp (arg, body)
		val map = A.VarExp (DV.parrPairMap (), [p1Ty, p2Ty, eltTy])
		val args = [f, e1, e2]
	        in
	          AU.mkApplyExp (map, args)
	        end
	    | (pes, NONE) => raise Fail "todo(3)" (* see COMMENT 3 below *)
	    | (pes, SOME pred) => raise Fail "todo: pcomp with predicate on multiple pbinds"
           (* end case *))
      (* end case *))
      
end

(* COMMENT 1 *)
(* we don't have filter yet *)
(* let *)
(*                         val pred' = trExp pred *)
(*                         val filterPV = BasisEnv.getVarFromBasis ["Rope", "filterP"] *)
(* 			val filterP = A.VarExp (filterPV, [t]) *)
(* 			val tmpV = Var.new ("tmp", t1) *)
(* 			val cs = A.CaseExp (A.VarExp (tmpV, []), *)
(* 					    [A.PatMatch (p1, pred')], *)
(* 					    Basis.boolTy) *)
(* 			val predFn = A.FunExp (tmpV, cs, Basis.boolTy) *)
(* 			val filtered = A.ApplyExp (filterP, *)
(* 						   A.TupleExp [predFn, e1'], *)
(* 						   Basis.parrayTy t1) *)
(*                         in *)
(* 			  map filtered *)
(* 		        end *)

(* COMMENT 2 *)
(* let (\* two pbinds, no predicates *\) *)
(*                val t = TypeOf.exp e *)
(* 	       val t1 = TypeOf.pat p1 *)
(* 	       val t2 = TypeOf.pat p2 *)
(* 	       val pairTy = A.TupleTy [t1, t2] *)
(* 	       val pairV = Var.new ("pair", pairTy) *)
(* 	       val pairPat = A.TuplePat [p1, p2] *)
(* 	       val e' = trExp e *)
(* 	       val e1' = trExp e1 *)
(* 	       val e2' = trExp e2 *)
(* 	       val ca = A.CaseExp (A.VarExp (pairV, []), *)
(* 				   [A.PatMatch (pairPat, e')], *)
(* 				   t) *)
(* 	       val f = A.FunExp (pairV, ca, t) *)
(* 	       val mapP2V = BasisEnv.getVarFromBasis ["RopePair", "mapP"] *)
(* 	       val mapP2 = A.VarExp (mapP2V, [t1, t2, t]) *)
(* 	       in *)
(*                  A.ApplyExp (mapP2, A.TupleExp [f, e1', e2'], Basis.parrayTy t) *)
(* 	       end *)

(* COMMENT 3 *)
(* let (\* any number of pbinds, no pred *\) *)
(*   	       val arity = List.length pes *)
(* 	       val te = TypeOf.exp e *)
(* 	       val e' = trExp e *)
(* 	       val (ps, es) = ListPair.unzip pes *)
(* 	       val tps = List.map TypeOf.pat ps *)
(* 	       val argTy = A.TupleTy tps *)
(* 	       val funTy = A.FunTy (argTy, te) *)
(* 	       val arg = Var.new ("arg", argTy) *)
(* 	       val f = Var.new ("f", funTy) *)
(* 	       val b = A.CaseExp (A.VarExp (arg, []), *)
(* 				  [A.PatMatch (A.TuplePat ps, e')], *)
(* 				  te) *)
(* 	       val lam = A.FB (f, arg, b) *)
(* 	       val resTy = Basis.parrayTy te *)
(* 	       val tup = A.VarExp (f, []) :: (List.map trExp es) *)
(* 	       val mapPn = let *)
(*                  val tupTy = A.TupleTy (funTy :: (List.map TypeOf.exp es)) *)
(* 		 val ty = A.FunTy (tupTy, resTy) *)
(* 		 in *)
(* 		   A.VarArityOpExp (A.MapP, arity, ty) *)
(* 	         end *)
(* 	       in *)
(* 		 A.LetExp (A.FunBind [lam], *)
(* 			   A.ApplyExp (mapPn, A.TupleExp tup, resTy)) *)
(* 	       end *)
