(* nester.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utilities for both nesting and flattening tuples.
 *)

structure Nester (* : sig

    val fromExp : AST.exp  -> AST.var * AST.lambda

  end *) =

  struct

    structure A = AST
    structure T = Types

    (* fail : string -> 'a *) 
    fun fail msg = raise Fail msg

    (* todo : string -> 'a *)
    fun todo msg = fail ("todo: " ^ msg)

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
	      | exps [] = fail "empty"
	in
	    exps es
	end

    (* flatten : A.exp -> A.exp *)
    fun flatten (A.TupleExp es)  = A.TupleExp  (removeParens es)
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
      | mkVarTup _ = fail "not a tuple type"

    (* isDCon : A.exp -> bool *)
    fun isDCon (A.ConstExp (A.DConst _)) = true
      | isDCon _ = false

    (* isTup : A.exp -> bool *)
    fun isTup (A.PTupleExp _) = true
      | isTup (A.TupleExp  _) = true
      | isTup _ = false

    (* isDConApp : A.exp -> bool 
     * Returns true if the given exp is an application of a data constructor.
     * Nullary data constructors are constants, so this function will
     * return false for them. *)
    fun isDConApp (A.ApplyExp (f, _, _)) = isDCon f
      | isDConApp _ = false

    (* funFromLam : A.lambda -> A.exp *)
    (* To clothe a naked lambda in the garb of an expression. *)
    fun funFromLam (lam as (A.FB (f, _, _))) = 
	A.LetExp (A.FunBind [lam], 
		  A.VarExp (f, []))
        (* ??? should VarExp list nec. be [] ??? *)	

    (* compose: A.exp * A.exp -> A.exp *)
    (* Return the composition of two functions. *)
    (* Note: compose (f, g) ==> fn x => f (g x) *)
    (* Pre: the output type of g matches the input type of f. *)
    fun compose (f, g) =
	let val (fd, fr) = case TypeOf.exp f
			     of T.FunTy t => t
			      | _ => fail "compose: f is not a function"
	    val (gd, gr) = case TypeOf.exp g
			     of T.FunTy t => t
			      | _ => fail "compose: g is not a function"
	in
	    if TypeUtil.same (gr, fd) then
		let val h = Var.new ("h", T.FunTy (gd, fr))
		    val x = Var.new ("x", gd)
		    val app = A.ApplyExp (f, 
					  A.ApplyExp (g, 
						      A.VarExp (x, []), 
						      fd), 
					  fr)
		in
		    funFromLam (A.FB (h, x, app))
		end
	    else
		 fail "compose: g range doesn't match f domain"
	end
	
    (* freshVarExp : T.ty -> A.exp *)
    fun freshVarExp t = A.VarExp (FreshVar.fresh (NONE, t), []) (* ??? *)

    (* argTy : A.exp -> T.ty option *)
    (* If the given expression is a non-nullary data constructor, return *)
    (*   SOME argument type; otherwise return NONE. *)
    fun argTy (A.ConstExp (A.DConst (d, _))) = DataCon.argTypeOf d
                               (* note: argTypeOf already returns an option *)
      | argTy _ = NONE

    local
	(* v : T.ty -> A.exp list * A.exp *)
	fun v t = let val x = freshVarExp t in ([x], x) end
    in
    (* nesterComponents : A.exp -> A.exp list * A.exp *)
    (* Produces a flat list of variables and the properly-shaped tuple *)
    (*   of variables and data constructors. *)
    fun nesterComponents (app as A.ApplyExp (e1, e2, t)) = 
	if (isDCon e1) andalso (isDConApp e2) then
	    let val c2 = (case nesterComponents e2
                            of (_, A.ApplyExp (c, _, _)) => c
			     | _ => fail "nesterComponents: not a function")
		val c2ArgTy = (case TypeOf.exp c2
                                 of T.FunTy (d, _) => d
				  | _ => fail "nesterComponents: not a function")
		val x = freshVarExp c2ArgTy
	    in
		([x], A.ApplyExp (compose (e1, c2), x, t))
	    end
	else if (isDCon e1) andalso (isTup e2) then
	    let val (vs, ft) = nesterComponents e2
	    in
		(vs, A.ApplyExp (e1, ft, t))
	    end
	else
	    (case argTy e1
 	       of SOME at => 
		  let val x = freshVarExp at
		  in
		      ([x], A.ApplyExp (e1, x, t))
		  end
		| NONE => v t
  	      (* end case *))
      | nesterComponents (A.TupleExp es) = 
  	  let val (vss, xs) = ListPair.unzip (map nesterComponents es)
	  in
	      (List.concat vss, A.TupleExp xs)
	  end
      | nesterComponents (A.PTupleExp es) = 
	  let val (vss, xs) = ListPair.unzip (map nesterComponents es)
	  in
	      (List.concat vss, A.TupleExp xs)
	  end
      | nesterComponents e = v(TypeOf.exp e)
    end (* local *)

    (* fromExp : A.exp -> A.var * A.lambda *)
    (* Pre: The argument is a tuple. *)
    (* Generates a "nester" based on the given expression. *)
    (* Returns the var naming the generated nester and its definition. *)
    (* A nester is a fn that imposes tuple and datacon structure on a flat tup. *)
    (* ex: fromExp (1,(SOME 2,3)) ==> fn (a,b,c) => (a,(SOME b,c)) *)
    fun fromExp e = 
	let fun exps es =
		let val _ = FreshVar.resetVarNames ()
		    val (ves, e') = nesterComponents e
		    val flatTup = A.TupleExp ves
		    val nestedTy  = TypeOf.exp e
		    val flatTy = TypeOf.exp flatTup
		    val nest = Var.new ("nest", T.FunTy (flatTy, nestedTy))
		    val x = Var.new ("x", flatTy)
		    fun vpat (A.VarExp (v, _)) = A.VarPat v
		      | vpat _ = fail "vpat: expected VarExp"
		    val p = A.TuplePat (map vpat ves)
		    val body = A.CaseExp (A.VarExp (x, []),
					  [(p, Unpar.noPTups e')],
					  nestedTy)
		in
		    (nest, A.FB (nest, x, body))
		end
	in
	    case e
 	      of A.PTupleExp es => exps es
	       | A.TupleExp  es => exps es
	       | _ => fail "fromExp: expected tuple"
	end

    (**** tests ****)

    local

	structure B = Basis
	structure U = TestUtils 

	fun fromExp' e = 
	    let val (v, lam) = fromExp e
	    in
		funFromLam lam
	    end

	val testTup = U.test fromExp'

	val t0 = U.ptup [U.int 1, U.ptup [U.int 2, U.int 3]]
	val t1 = U.ptup [U.int 1, U.ptup [U.some (U.int 2),
					  U.int 3]]
	val t2 = U.ptup [U.some (U.int 2), U.some (U.int 9), U.none B.intTy]
	val t3 = U.ptup [U.some (U.int 1), 
			 U.some (U.ptup [U.int 2, U.int 3])]
	val t4 = U.ptup [U.ptup [U.ptup [U.ptup [U.int 1, U.some (U.int 2)],
					 U.int 3],
				 U.int 4],
			 U.int 5]

	(* t5 = (| 1, SOME (| 2, 3 |) |) *)
	(* desired outcome : fn (a,b,c) => (a,SOME(b,c)) *)
	val t5 = U.ptup [U.int 1, U.some (U.ptup [U.int 2, U.int 3])]
			 
	(* t6 = (| 1, SOME (SOME 2) |) *)
	(* desired outcome : fn (a, b) => (a, SOME (SOME b)) *)
	val t6 = U.ptup [U.int 1, U.some (U.some (U.int 2))]

        (* t7 = (| SOME (SOME (SOME (SOME 4))) |) *)
	val t7 = U.ptup [U.some (U.some (U.some (U.some (U.int 4))))]

	(* t8 = (| SOME (| SOME 5, SOME 6 |) |) *)
	val t8 = U.ptup [U.some (U.ptup [U.some (U.int 5), 
					 U.some (U.int 6)])]

    in

        (* test : int -> unit *)
        fun test n =
	    let val tests = [t0,t1,t2,t3,t4,t5,t6,t7,t8]
	    in
		testTup (List.nth (tests, n))
		handle Subscript =>
		  let val msg = "Nester.test: Please choose a test value \
                                \between 0 and "
				^ Int.toString ((List.length tests) - 1)
				^ ".\n"
		  in
		      print msg
		  end
	    end
        
    end

  end
