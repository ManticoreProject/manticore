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
    structure F = Futures
    structure R = Ropes
    structure U = UnseenBasis

    fun parrayElementType t =
	(case t
	   of A.ConTy ([t'], k) =>
	        if TyCon.same (k, B.parrayTyc)
		then t'
		else raise Fail "not a parray"
	    | _ =>  raise Fail "not a parray")

    fun tr trExp (e, pes, oe) =
	  (case (pes, oe)
	     of ([], _) => raise Fail "no pbinds at all"
	      | ([(p1, e1)], NONE) =>  (* first I'll build the one pbind, no predicate case, and iter. refine it *)
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
		       val lfSize = ASTUtil.mkInt (R.maxLeafSize ())
		   in
		       (case e1
		         of A.RangeExp (lo, hi, optStep, rangeEltTy) =>
			      let val lo' = trExp lo
				  val hi' = trExp hi
                                  val step = (case optStep
                                                of NONE => ASTUtil.mkInt 1
                                                 | SOME s => trExp s)
				  val tabD = A.VarExp (U.tabD, [t])
				  val tup = A.TupleExp [f, lfSize, lo', hi', step]
			      in
				  A.ApplyExp (tabD, tup, resTy)
			      end
			  | _ (* not a range exp *) =>
			      let val e1' = trExp e1
				  val mapP = A.VarExp (U.mapP, [t1, t])
			      in
				  A.ApplyExp (mapP, A.TupleExp [f, e1'], resTy)				   
			      end
		       (* end case *))
		   end
	      | (pes, NONE) => (* any number of pbinds, no pred *)
  		  let val arity = List.length pes
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
		      val resTy = B.parrayTy te
		      val tup = A.VarExp (f, []) :: (List.map trExp es)
		      val mapPn =
			  let val tupTy = A.TupleTy (funTy :: (List.map TypeOf.exp es))
			      val ty = A.FunTy (tupTy, resTy)
			  in
			      A.VarArityOpExp (A.MapP, arity, ty)
			  end
		  in
		      A.LetExp (A.FunBind [lam],
		      A.ApplyExp (mapPn, A.TupleExp tup, resTy))
		  end
	      | _ => raise Fail "todo"
	  (* end case *))

  end
