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

  (* tab2DMatch *)
  (* This function identifies pcomps of the form *)
  (* [| [| f(i,j) | j in rng1 |] | i in rng2 |]  *)
  (* which will be compiled specially.           *)
    fun tab2DMatch (e, pes, oe) = (case e
      of A.PCompExp (f_ij, [(A.VarPat j, A.RangeExp _)], NONE) => (case pes
           of [(A.VarPat i, A.RangeExp _)] => (case oe
	        of NONE => true)
	    | _ => false)
       | _ => false)

    fun tr trExp (e, pes, oe) = 
      if tab2DMatch (e, pes, oe) then
        raise Fail "todo: 2D"
      else (case (pes, oe)
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
	       val c1 = A.CaseExp (A.VarExp (x1, []), 
				   [A.PatMatch (p1, e')],
				   eltTy)
	       val f = A.FunExp (x1, c1, eltTy)
	       val e1' = trExp e1
	       val mapP = A.VarExp (DV.parrayMap (), [eltTy, patTy]) 
               (* NOTE: these type args seem backwards to me, but I've tested this. - ams*)
	       fun map arr = AU.mkCurriedApplyExp (mapP, [f, arr])
               in
                 case optPred
		   of NONE => map e1'
		    | SOME pred => raise Fail "todo" (* we don't have filter yet *)
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
               end
	   | ([(p1, e1), (p2, e2)], NONE) => raise Fail "todo"
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
	   | (pes, NONE) => raise Fail "todo"
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
	   | (pes, SOME pred) => raise Fail "todo: pcomp with predicate on multiple pbinds"
          (* end case *))
      
end
