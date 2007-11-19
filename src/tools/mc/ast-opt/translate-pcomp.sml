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
	     of ([], _) => raise Fail "no pbinds at all"
	      | ([(p1, e1)], NONE) =>  (* first I'll build the one pattern, no predicate case, and iter. refine it *)
		                       (* these and subsequent cases will eventually be unified together *)
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
                                  val step = (case optStep
                                                of NONE => ASTUtil.mkInt 1
                                                 | SOME s => trExp s)
				  val tabD = A.VarExp (U.tabD, [t])
				  val tup = A.TupleExp [workQ, f, lfSize, lo', hi', step]
			      in
				  A.ApplyExp (tabD, tup, resTy)
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
	      | (pe::_, NONE) => (* the multiple pbind, no pred case *)
                                 (* NOTE this isn't really built to deal with ranges yet *)
		                 (* FIXME magicalMap! *)
		  let val e' = trExp e
		      val t = TypeOf.exp e
		      fun build ([], _, xs, ps, es) =
			    let val (xs, ps, es) = (rev xs, rev ps, rev es)
				val tupExp = A.TupleExp (map (fn x => A.VarExp (x, [])) xs)
				val tupPat = A.TuplePat ps
				val caseExp = A.CaseExp (tupExp, [A.PatMatch (tupPat, e')], t)
				val arg = Var.new ("arg", TypeOf.exp tupExp)
				val m = A.PatMatch (tupPat, caseExp)
				val f = A.FunExp (arg, A.CaseExp (A.VarExp (arg, []), [m], t), t)
				val magicalMap = raise Fail "todo"
			    in
				A.ApplyExp (magicalMap, 
					    A.TupleExp [f, A.TupleExp es], 
					    B.parrayTy t)
			    end
			| build ((p,a)::tl, n, xs, ps, es) =
			    let val x = Var.new ("x" ^ Int.toString n, TypeOf.pat p)
			    in
				build (tl, n+1, x::xs, p::ps, trExp(a)::es)
			    end
		  in
		      build (pes, 1, [], [], [])
		  end				
	      | _ => raise Fail "todo"
	  (* end case *))

  end
