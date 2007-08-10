(* nester.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utilities for both nesting and flattening tuples.
 *)

(**** NOTE : THIS IS NOT IN WORKING CONDITION AT THE MO'. - ams ****)

structure Nester (* : sig

    val fromTy  : Types.ty -> AST.var * AST.lambda
    val fromExp : AST.exp  -> AST.var * AST.lambda

  end *) =

  struct

    structure A = AST
    structure T = Types

    (* removeParens : A.exp list -> A.exp list *)
    (* pre: argument must not be empty *)
    fun removeParens es =
	let (* exp : exp -> exp list *)
	    fun exp (A.TupleExp es) = exps es
	      | exp (A.PTupleExp es) = exps es
	      | exp e = [e]
	    (* exps : exp list -> exp list *)
	    and exps ([e]) = exp e
	      | exps (e::es) = exp e @ exps es
	      | exps [] = raise Fail "empty"
	in
	    exps es
	end

    (* flatten : A.exp -> A.exp *)
    fun flatten (A.TupleExp es)  = A.TupleExp (removeParens es)
      | flatten (A.PTupleExp es) = A.PTupleExp (removeParens es)
      | flatten e = e

    (* mkVarTup : T.ty -> A.exp *)
    fun mkVarTup (T.TupleTy ts) =
	  let fun v var = A.VarExp (var, []) 
	      (* build : T.ty list -> A.exp list *)
	      fun build ([], acc) = rev acc
		| build (t::ts, acc) = 
		    (case t
		      of T.TupleTy _ => build (ts, (mkVarTup t) :: acc)
		       | _ => build (ts, (v (FreshVar.fresh (NONE, t))) :: acc))
	  in
	      A.PTupleExp (build (ts, []))
	  end
      | mkVarTup _ = raise Fail "not a tuple type"

    (* fromTy : T.ty -> A.var * A.lambda *)
    (* pre: The argument t is a TupleTy. *)
    (* note: this function does not (cannot) extract data constructors *)
    fun fromTy t =
	let val nestedVarTup = mkVarTup t 
                            (* will throw an exception if pre is violated *)
	    val flatVarTup = flatten nestedVarTup
	    val nestedTupTy = t
	    val flatTupTy = TypeOf.exp flatVarTup
	    val nest = Var.new ("nest", T.FunTy (flatTupTy, nestedTupTy))
	    val x = Var.new ("x", flatTupTy)
	    val pat = 
		(* flatVarTup is a parallel tuple of variable expressions *)
		let (* p : A.exp -> A.pat *)
		    fun p (A.VarExp (v, ts)) = A.VarPat v
		      | p _ = raise Fail "expected a VarExp"
		    (* es : A.exp list *)
		    val es = case flatVarTup
			       of (A.PTupleExp es) => es
				| _ => raise Fail "expected a PTupleExp"
		in
		    A.TuplePat (map p es)
		end
	    val body = A.CaseExp (A.VarExp (x, []),
				  [(pat, Unpar.unpar nestedVarTup)],
				  nestedTupTy)
	in
	    FreshVar.resetVarNames ();
	    (nest, A.FB (nest, x, body))
	end

    (* isDConApp : A.exp -> bool 
     * Returns true if the given exp is an application of a data constructor.
     * Nullary data constructors are constants, so this function will
     * return false for them. *)
    fun isDConApp (A.ApplyExp (A.ConstExp (A.DConst _), _, _)) = true
      | isDConApp _ = false

    (* composeAll : T.dcon list -> A.exp *)
    (* Pre: The argument cs is not empty. *)
    fun composeAll (c::[]) = 
	  let val ts = raise Fail "todo" (* I need a list of types here... *)
	  in 
	      A.ConstExp (A.DConst (c, ts))
	  end
      | composeAll (c::cs) = 
	  let val ts = raise Fail "todo" (* I need a list of types here... *)
	  in
	      (* fn a => c ((composeAll cs) a) *)
	      raise Fail "todo"
	  end
      | composeAll [] = raise Fail "composeAll: given empty list"

    (* collectDCons : A.exp list -> A.exp option list
     * ex: collectDCons [Fahr 32, Cel 100]   ==> [SOME Fahr, SOME Cel] 
     * ex: collectDCons [optionSOME 8, 9]    ==> [SOME optionSOME, NONE]
     * ex: collectDCons [optionSOME (Int 8)] ==> [SOME (optionSOME o Int)] *)
    fun collectDCons es =
	let (* exp : A.exp -> T.dcon list *)
	    fun exp (A.ApplyExp(A.ConstExp(A.DConst(c,_)),e',_)) = c::exp(e')
	      | exp _ = []
	    (* mash : T.dcon list -> A.exp option *)
	    fun mash [] = NONE
	      | mash cs = SOME (composeAll cs)
	in
	    map (mash o exp) es
	end

    (* fromExp : A.exp -> A.var * A.lambda *)
    (* Pre: The argument is a tuple. *)
    fun fromExp (A.PTupleExp es) =
	let val es' = removeParens es
	    val dcons = collectDCons es'
	in
	    raise Fail "todo"
	end
      | fromExp (A.TupleExp es)  = raise Fail "todo: fromExp"
      | fromExp _ = raise Fail "fromExp: tuple expected"

  end
