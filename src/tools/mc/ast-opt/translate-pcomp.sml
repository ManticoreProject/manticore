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
  structure D = DelayedBasis

  structure AU = ASTUtil
  structure TU = TypeUtil

  fun tr trExp (e, pes, oe) = 
   (case (pes, oe)
     of ([], _) => raise Fail "a parallel comprehension with no pbinds at all"
     | ([(p1, e1 as A.RangeExp (loExp, hiExp, optStepExp, t))], NONE) => let
      (* optimization of common case: [| f(n) | n in [| 1 to 100 |] |] *)
      (* becomes Rope.tabFromToP (1, 101, f) *)
      (* (as opposed to a mapP over a constructed range) *) 
	  val _ = if TU.same(t,B.intTy) then ()
		  else raise Fail ("unexpected type " ^ TU.toString t)
          val eTy = TypeOf.exp e
	  val pTy = TypeOf.pat p1
	  val x = Var.new ("x", pTy)
	  val e' = trExp e
	  val c = A.CaseExp (A.VarExp (x, []), [A.PatMatch (p1, e')], eTy)
	  val f = A.FunExp (x, c, eTy)
          val (tab, args) = (case optStepExp 
            of NONE => let
                 val t = A.VarExp (D.Var.ropeTabFT (), [eTy])              
		 val a = [loExp, hiExp, f]
                 in 
		   (t, a) 
	         end
	     | SOME stepExp => let
                 val t = A.VarExp (D.Var.ropeTabFTS (), [eTy])
		 val a = [loExp, hiExp, stepExp, f]
                 in
		   (t, a)
	         end      
	    (* end case *))
          in
	     AU.mkApplyExp (tab, args)
	  end
      | ([(p1, e1)], optPred) => let (* the one pbind, no predicate case *)
          val t  = TypeOf.exp e
	  val t1 = TypeOf.pat p1
	  val x1 = Var.new ("x1", t1)
	  val e' = trExp e
	  val c1 = A.CaseExp (A.VarExp (x1, []), 
			      [A.PatMatch (p1, e')],
			      t)
	  val f = A.FunExp (x1, c1, t)
	  val e1' = trExp e1
	  val mapP = A.VarExp (D.Var.ropeMapP (), [t1, t])
	  val resTy = Basis.parrayTy t
	  fun map arr = AU.mkApplyExp (mapP, [f, arr])
          in case optPred
            of NONE => map e1'
	     | SOME pred => let
                 val pred' = trExp pred
		 val filterP = A.VarExp (D.Var.ropeFilterP (), [t])
		 val tmpV = Var.new ("tmp", t1)
		 val cs = A.CaseExp (A.VarExp (tmpV, []),
				     [A.PatMatch (p1, pred')],
				     Basis.boolTy)
		 val predFn = A.FunExp (tmpV, cs, Basis.boolTy)
		 val filtered = A.ApplyExp (filterP,
					    A.TupleExp [predFn, e1'],
					    Basis.parrayTy t1)
                 in
		   map filtered
	       end
          end
      | ([(p1, e1), (p2, e2)], NONE) => let (* two pbinds, no predicates *)
          val t = TypeOf.exp e
	  val t1 = TypeOf.pat p1
	  val t2 = TypeOf.pat p2
	  val pairTy = A.TupleTy [t1, t2]
	  val pairV = Var.new ("pair", pairTy)
	  val pairPat = A.TuplePat [p1, p2]
	  val e' = trExp e
	  val e1' = trExp e1
	  val e2' = trExp e2
	  val ca = A.CaseExp (A.VarExp (pairV, []),
			      [A.PatMatch (pairPat, e')],
			      t)
	  val f = A.FunExp (pairV, ca, t)
	  val mapP2 = A.VarExp (D.Var.ropePairMapP (), [t1, t2, t])
	  in
            A.ApplyExp (mapP2, A.TupleExp [f, e1', e2'], Basis.parrayTy t)
	  end
      | (pes, NONE) => let (* any number of pbinds, no pred *)
  	  val arity = List.length pes
	  val te = TypeOf.exp e
	  val e' = trExp e
	  val (ps, es) = ListPair.unzip pes
	  val tps = List.map TypeOf.pat ps
	  val argTy = A.TupleTy tps
	  val funTy = A.FunTy (argTy, te)
	  val arg = Var.new ("arg", argTy)
	  val f = Var.new ("f", funTy)
	  val b = A.CaseExp (A.VarExp (arg, []),
			     [A.PatMatch (A.TuplePat ps, e')],
			     te)
	  val lam = A.FB (f, arg, b)
	  val resTy = Basis.parrayTy te
	  val tup = A.VarExp (f, []) :: (List.map trExp es)
	  val mapPn = let
            val tupTy = A.TupleTy (funTy :: (List.map TypeOf.exp es))
	    val ty = A.FunTy (tupTy, resTy)
            in
	      A.VarArityOpExp (A.MapP, arity, ty)
	    end
	  in
	    A.LetExp (A.FunBind [lam],
		      A.ApplyExp (mapPn, A.TupleExp tup, resTy))
	  end
      | (pes, SOME pred) => raise Fail "todo: pcomp with predicate on multiple pbinds"
     (* end case *)) 

end
