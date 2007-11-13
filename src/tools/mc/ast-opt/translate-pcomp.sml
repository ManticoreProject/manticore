(* translate-pcomp.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TranslatePComp : sig

  (* An AST to AST translation of parallel comprehensions. *)
    val tr : (AST.exp -> AST.exp) 
             -> AST.exp 
             -> AST.exp * (AST.pat * AST.exp) list * AST.exp option 
             -> AST.exp

  end  = struct

    structure A = AST
    structure B = Basis
    structure F = Futures
    structure R = Ropes
    structure U = UnseenBasis

    fun tr trExp workQ (e, pes, oe) =
	  (case (pes, oe)
	     of (* first I'll build the one pattern, no predicate case, and iter. refine it *)
	       ([(p1, e1)], NONE) =>
                 let val t  = TypeOf.exp e
		     val t1 = TypeOf.pat p1
		     val x1 = Var.new ("x1", t1)
		     val e' = trExp e
		     val c1 = A.CaseExp (A.VarExp (x1, []), 
					[A.PatMatch (p1, e')],
					t)
		     val f = A.FunExp (x1, c1, t)
		     val resTy = B.parrayTy t
		     val lfSize = ASTUtil.mkInt R.maxLeafSize
		 in
		     (case e1
		        of A.RangeExp (lo, hi, optStep, rangeEltTy) =>
			     let val lo' = trExp lo
				 val hi' = trExp hi
				 val tabD = A.VarExp (U.tabulateD, [t])
			     in
				 (case optStep
			            of NONE =>
				         let val tup = A.TupleExp [workQ, f, lfSize, lo', hi']
					 in
					     A.ApplyExp (tabD, tup, resTy)
					 end
				     | SOME step =>
				         let val n = Var.new ("n", rangeEltTy) 
					     val stepTabD = A.VarExp (U.steppedTabulateD, [t])
					     val step' = trExp step
					     val tup = A.TupleExp [workQ, f, lfSize, lo', hi', step']
					 in
					     A.ApplyExp (stepTabD, tup, resTy)
					 end
  				   (* end case *))
			     end
			 | _ (* not a range exp *) =>
			     let val e1' = trExp e1
				 val mapPQ = A.VarExp (B.mapPQ, [t1, t])
				 val mapResTy =
				     (case TypeOf.exp mapPQ
				        of A.FunTy (_, rty) => rty
					 | _ => raise Fail "expected function type"
				      (* end case *))
			     in
				 A.ApplyExp (A.ApplyExp (mapPQ, workQ, mapResTy),
					     A.TupleExp [f, e1'], 
					     resTy)
			     end
		       (* end case *))
		 end
	     | _ => raise Fail "todo"
	   (* end case *))

  end
