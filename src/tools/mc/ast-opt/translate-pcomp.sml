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
    structure AU = ASTUtil
    structure TU = TypeUtil

    structure DV = DelayedBasis.Var

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
		     val f_ij = A.FunExp (ij, body, TypeOf.exp e')
		     in
		       SOME (collect (f_ij, r2, r1))
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
        (iFrom, iTo, iStep, jFrom, jTo, jStep, f_ij)
      end

  (* tr : (exp -> exp) -> exp * (pat * exp) list * exp option -> exp *)
    fun tr trExp (e, pes, oe) = (case tab2DMatch (e, pes, oe)
      of SOME (iFrom, iTo, iStep, jFrom, jTo, jStep, f) => let
	   val eTy = TypeOf.exp e
	   val args = [iFrom, iTo, iStep, jFrom, jTo, jStep, f]
           in
	     AU.mkApplyExp (A.VarExp (DV.parrayTab2D (), [eTy]), args)
	   end
       | NONE => (case (pes, oe)
           of ([], _) => raise Fail "a parallel comprehension with no pbinds at all"
	    | ([(p1, e1 as A.RangeExp (loExp, hiExp, optStepExp, rngEltTy))], NONE) => let
                (* optimization of a common case: [| f(n) | n in [| 1 to 100 |] |] *)
	        (* becomes PArray.tabFromToStep (1, 101, 1, f) *)
		(* (as opposed to a PArray.map over a constructed range) *)
	        val _ = if TU.same (rngEltTy, B.intTy) then ()
	 		else raise Fail ("unexpected type " ^ TU.toString rngEltTy)
		val eTy = TypeOf.exp e
		val pTy = TypeOf.pat p1 (* should be same as t *)
		val x = Var.new ("x", pTy)
		val e' = trExp e
		val c = A.CaseExp (A.VarExp (x, []), [A.PatMatch (p1, e')], eTy)
		val f = A.FunExp (x, c, eTy)
		val stepExp = Option.getOpt (optStepExp, AU.mkInt 1)
	        in
	          AU.mkApplyExp (A.VarExp (DV.parrayTabFTS (), [eTy]),
				 [loExp, hiExp, stepExp, f])
	        end
	    | ([(p1, e1)], optPred) => let (* the one pbind, no predicate case *)
                val eltTy = TypeOf.exp e
		val patTy = TypeOf.pat p1
		val x1 = Var.new ("x1", patTy)
		val e' = trExp e
		val c1 = AU.mkCaseExp (A.VarExp (x1, []), [A.PatMatch (p1, e')])
		val f = A.FunExp (x1, c1, eltTy)
		val e1' = trExp e1
		val mapP = A.VarExp (DV.parrayMap (), [eltTy, patTy]) 
                (* NOTE: these type args seem backwards to me, but I've tested this. - ams*)
		fun map arr = AU.mkCurriedApplyExp (mapP, [f, arr])
                in case optPred
		  of NONE => map e1'
		   | SOME pred => raise Fail "todo" (* see COMMENT 1 below *) 
		end
	    | ([(p1, e1), (p2, e2)], NONE) => raise Fail "todo" (* see COMMENT 2 below *)
	    | (pes, NONE) => raise Fail "todo" (* see COMMENT 3 below *)
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
