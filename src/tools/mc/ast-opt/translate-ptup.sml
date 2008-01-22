(* fut-par-tup.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * This module rewrites parallel tuples in terms of futures and touches.
 *
 * Roughly, the transformation turns
 *  (| e1, e2, ..., en |)
 * into
 *   let val f2 = future (fn () => e2)
 *       ...
 *       val fn = future (fn () => en)
 *   in
 *       (e1, touch f2, ..., touch fn)
 *   end
 *
 * Note this rewriting is type preserving.
 *)

structure TranslatePtup : sig

    val tr : (AST.exp -> AST.exp) -> AST.exp -> AST.exp list -> AST.exp option

  end  = 

  struct

    structure A = AST
    structure T = Types
    structure F = Futures

    val changed = ref false

    fun tr trExp workQ es =
	(* Precondition: The argument to the function, a list, must not be empty. *)
	(* Consumes a list whose members are the contents of a parallel tuple, *)
	(* and produces a LetExp that is a "futurized" ptuple. *)
	(* Note: the first member of the list is not futurized (an optimization). *)
	let fun ptuple [] = raise Fail "empty parallel tuple"
	      | ptuple (e::es) = 
		  let (* mkVar : int * ty -> var *)
		      fun mkVar (n, t) = 
			  let val name = "f" ^ Int.toString n
			  in
			      Var.newWithKind (name, A.VK_Pat, t)
			  end
		      (* build : exp list * int * binding list * exp list -> exp *) 
		      fun build ([], _, bs, tupExps) = 
			    let val tup = A.TupleExp (trExp e :: tupExps)
			    in
				foldr A.LetExp tup bs
			    end
			| build (e::es, n, bs, tupExps) =
			    if F.isFutureCand e then
				let val _ = (changed := true)
				    val fe = F.mkFuture1 (trExp e)
				    val f_n = mkVar (n, TypeOf.exp fe)
				    val b = A.ValBind (A.VarPat f_n, fe)
				    val t = F.mkTouch1 (A.VarExp (f_n, []))
				in
				    build (es, n+1, b::bs, t::tupExps)
				end
			    else
				build (es, n, bs, trExp e :: tupExps)
		      val es' = map trExp es
		  in
		      build (rev es', 1, [], [])
		  end
	    val result = ptuple es
	in
	    if !changed then SOME result else NONE
	end
  end
