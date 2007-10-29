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

  end = struct

    structure B = Basis
    structure F = Futures

    val --> = AST.FunTy
    fun ** (t1, t2) = AST.TupleTy[t1, t2]
    infix 9 **
    infixr 8 -->
	   
    val sumPQ = 
	let val t = F.workQueueTy --> ((B.parrayTy B.intTy) --> B.intTy)
	in
	    Var.new ("sumPQ", t)
	end

  end
