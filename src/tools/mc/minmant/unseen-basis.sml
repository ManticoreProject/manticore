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
    val mapP    : AST.var
    val map2P   : AST.var
    val reduceP : AST.var
    val sumP    : AST.var
    val tabD     : AST.var

  end = struct

    structure B = Basis
    structure T = Types

    val --> = T.FunTy
    fun ** (t1, t2) = T.TupleTy [t1, t2]
    infix 9 **
    infixr 8 -->
	  
    val (polyVar, polyVarMulti) = (BasisUtils.polyVar, BasisUtils.polyVarMulti)

    val mapP =
	let fun mkTy ([a,b]) =
		  let val fnTy = a --> b
		      val argTy = AST.TupleTy [fnTy, B.parrayTy a]
		  in
		      (argTy --> (B.parrayTy b))
		  end
	      | mkTy _ = raise Fail "BUG: bad instantiation for mapPQ"
	in
	    polyVarMulti ("mapP", 2, mkTy)
	end
				 
    val map2P =
	let fun mkTy ([a,b,c]) = 
		  let val fnTy = a --> (b --> c)
		      val argTy = AST.TupleTy [fnTy, B.parrayTy a, B.parrayTy b]
		  in
		      (argTy --> (B.parrayTy c))
		  end
	      | mkTy _ = raise Fail "BUG: bad instantiation for map2PQ"
	in
	    polyVarMulti ("map2P", 3, mkTy)
	end

    val reduceP =
	let fun mkTy ([a,b]) =
		  let val fnTy = (a ** a) --> b
		      val tupTy = T.TupleTy [fnTy, b, B.parrayTy a]
		  in
		      (tupTy --> b)
		  end
	      | mkTy _ = raise Fail "BUG: bad instantiation for reducePQ"
	in
	    polyVarMulti ("reduceP", 2, mkTy)
	end

    val sumP = 
	let val t = ((B.parrayTy B.intTy) --> B.intTy)
	in
	    Var.new ("sumP", t)
	end

    val tabD = 
	let fun mkTy tv = 
		let val intTy = B.intTy
		    val tupTy = T.TupleTy [intTy --> tv, intTy, intTy, intTy, intTy]
		in
		    tupTy --> (B.parrayTy tv)    
		end
	in
	    polyVar ("tabD", mkTy)
	end

  end
