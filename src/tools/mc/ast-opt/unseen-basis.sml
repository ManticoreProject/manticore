(* unseen-basis.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module includes operators which are not part of the surface language
 * but are the targets of early translations from operators in the surface 
 * language.
 *
 * e.g., sumP --> sumPQ
 *)

structure UnseenBasis : sig

  (* predefined functions *)
    val reducePQ  : AST.var
    val sumPQ     : AST.var
    val tabD : AST.var

  end = struct

    structure B = Basis
    structure T = Types

    val --> = T.FunTy
    fun ** (t1, t2) = T.TupleTy[t1, t2]
    infix 9 **
    infixr 8 -->
	   
    fun forall mkTy =
	let val tv = TyVar.new (Atom.atom "'a")
	in
	    AST.TyScheme ([tv], mkTy (AST.VarTy tv))
	end

    fun forAllMulti (n, mkTy) = 
	let fun mkTv n = TyVar.new (Atom.atom ("'a" ^ Int.toString n))
	    val tvs = map mkTv (List.tabulate (n, fn n => n))
	in
	    AST.TyScheme (tvs, mkTy (map AST.VarTy tvs))
	end

    fun polyVar (name, mkTy) = Var.newPoly (name, forall mkTy)

    fun polyVarMulti (name, n, mkTy) = Var.newPoly (name, forAllMulti (n, mkTy))

    val qTy = B.workQueueTy

    val reducePQ =
	let fun mkTy ([a,b]) =
		  let val fnTy = (a ** a) --> b
		      val tupTy = T.TupleTy [fnTy, b, B.parrayTy a]
		  in
		      qTy --> (tupTy --> b)
		  end
	      | mkTy _ = raise Fail "BUG: bad instantiation for reducePQ"
	in
	    polyVarMulti ("reducePQ", 2, mkTy)
	end

    val sumPQ = 
	let val t = qTy --> ((B.parrayTy B.intTy) --> B.intTy)
	in
	    Var.new ("sumPQ", t)
	end

    val tabD = 
	let fun mkTy tv = 
		let val intTy = B.intTy
		    val tupTy = T.TupleTy [qTy, intTy --> tv, intTy, intTy, intTy, intTy]
		in
		    tupTy --> (B.parrayTy tv)    
		end
	in
	    polyVar ("tabD", mkTy)
	end

  end
