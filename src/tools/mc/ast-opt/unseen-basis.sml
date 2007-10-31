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
    val sumPQ : AST.var
    val tabulateD : AST.var

  end = struct

    structure B = Basis
    structure F = Futures
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

    fun polyVar (name, mkTy) = Var.newPoly (name, forall mkTy)

    val sumPQ = 
	let val t = F.workQueueTy --> ((B.parrayTy B.intTy) --> B.intTy)
	in
	    Var.new ("sumPQ", t)
	end

    val tabulateD = 
	let fun mkTy tv = 
		let val (intTy, qTy) = (B.intTy, F.workQueueTy)
		    val tupTy = T.TupleTy [intTy --> tv, qTy, intTy, intTy, intTy]
		in
		    tupTy --> (B.parrayTy tv)    
		end
	in
	    polyVar ("tabulateD", mkTy)
	end

  end
