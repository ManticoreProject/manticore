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

  (* elements not in the surface language *)

    val workQueueTyc : Types.tycon

    val workQueueTy  : Types.ty

  (* predefined functions *)
    val mapPQ    : AST.var
    val map2PQ   : AST.var
    val reducePQ : AST.var
    val sumPQ    : AST.var
    val tabD     : AST.var

  end = struct

    structure B = Basis
    structure T = Types

    val --> = T.FunTy
    fun ** (t1, t2) = T.TupleTy [t1, t2]
    infix 9 **
    infixr 8 -->
	  
    val (polyVar, polyVarMulti) = (BasisUtils.polyVar, BasisUtils.polyVarMulti)

    val workQueueTyc = TyCon.newAbsTyc (Atom.atom "work_queue", 0, false)

    val workQueueTy = AST.ConTy ([], workQueueTyc)

    val qTy = workQueueTy

    val mapPQ =
	let fun mkTy ([a,b]) =
		  let val fnTy = a --> b
		      val argTy = AST.TupleTy [fnTy, B.parrayTy a]
		  in
		      qTy --> (argTy --> (B.parrayTy b))
		  end
	      | mkTy _ = raise Fail "BUG: bad instantiation for mapPQ"
	in
	    polyVarMulti ("mapPQ", 2, mkTy)
	end
				 
    val map2PQ =
	let fun mkTy ([a,b,c]) = 
		  let val fnTy = a --> (b --> c)
		      val argTy = AST.TupleTy [fnTy, B.parrayTy a, B.parrayTy b]
		  in
		      qTy --> (argTy --> (B.parrayTy c))
		  end
	      | mkTy _ = raise Fail "BUG: bad instantiation for map2PQ"
	in
	    polyVarMulti ("map2PQ", 3, mkTy)
	end

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
