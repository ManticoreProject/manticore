(* translate-pcomp.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TranslatePComp : sig

  (* An AST to AST translation of parallel comprehensions. *)
    val tr : (AST.exp -> AST.exp) -> AST.exp -> AST.exp * (AST.pat * AST.exp) list * AST.exp option -> AST.exp

  end  = struct

    structure A = AST
    structure B = Basis
    structure F = Futures
    structure U = UnseenBasis

    fun tr trExp workQ (e, pes, oe) =
	  (case (pes, oe)
	     of ([(A.VarPat x, A.RangeExp (lo, hi, NONE, rngTy))], NONE) =>
                  if not (TypeUtil.same (rngTy, B.intTy))
		  then raise Fail "todo"
		  else
                    let val t = TypeOf.exp e
			val f = A.FunExp (x, e, t)
			val leafSize' = ASTUtil.mkInt Ropes.maxLeafSize
			val lo' = trExp lo
			val hi' = trExp hi
			val tup = A.TupleExp [workQ, f, leafSize', lo', hi']
			val resTy = B.parrayTy t
           	    in
			A.ApplyExp (A.VarExp (U.tabulateD, [t]), tup, resTy)
		    end
	      | _ => raise Fail "todo"
	    (* end case *))

  end
