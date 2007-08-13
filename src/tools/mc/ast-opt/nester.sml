(* nester.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utilities for both nesting and flattening tuples.
 *)

structure Nester (* : sig

    val fromTy  : Types.ty -> AST.var * AST.lambda
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

    (* funFromDCon : T.dcon * T.ty list -> A.exp *)
    (* Given a constructor like "Fahr", returns a function as follows: *)
    (* "let val f = fn x => Fahr x in f end" *)
    (* Pre: The given data constructor is non-nullary. *)
    fun funFromDCon (d, ts) =
	let val dc = A.DConst (d, ts)
	    val t = TypeOf.const dc
	in
	    case t
	      of T.FunTy (dty, rty) =>
		 let val f = Var.new ("f", t)
		     val x = Var.new ("x", dty)
		     val app = A.ApplyExp (A.ConstExp dc,
					   A.VarExp (x, []), (* ??? *)
					   rty)
		 in
		     funFromLam (A.FB (f, x, app))
		 end
	       | _ => fail "funFromDCon: given a nullary data con"
	end

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
		(print "f: ";
		 PrintTypes.printTy (TypeOf.exp f);
		 print "f domain: ";
		 PrintTypes.printTy fd;
		 print "f range: ";
		 PrintTypes.printTy fr;
		 print "g domain: ";
		 PrintTypes.printTy gd;
		 print "g range: ";
		 PrintTypes.printTy gr;
		 fail "compose: g range doesn't match f domain")
	end
	
    (* foldr1 : ('a -> 'a) -> 'a list -> 'a *)
    (* Pre: the list is not empty. *)
    fun foldr1 f xs =
	let fun g (x::[]) = x
	      | g (x::xs) = f (x, g xs)
	      | g [] = fail "foldr1: given empty list"
	in
	    g xs
	end

    (* collectDCons : A.exp list -> A.exp option list
     * ex: collectDCons [Fahr 32, Cel 100]   ==> [SOME Fahr, SOME Cel] 
     * ex: collectDCons [optionSOME 8, 9]    ==> [SOME optionSOME, NONE]
     * ex: collectDCons [optionSOME (Int 8)] ==> [SOME (optionSOME o Int)] *)
    val collectDCons =
	let (* exp : A.exp -> A.exp list *)
	    fun exp (A.ApplyExp(A.ConstExp(A.DConst(c,ts)),e',_)) = 
		  funFromDCon (c,ts) :: exp e'
	      | exp _ = []
	    (* mash : (T.dcon * T.ty list) list -> A.exp option *)
	    fun mash [] = NONE
	      | mash fs = SOME (foldr1 compose fs)
	in
	    map (mash o exp)
	end

    (**** main traversal of the AST ****)

    (* freshVar : T.ty -> A.exp *)
    fun freshVar t = A.VarExp (FreshVar.fresh (NONE, t), []) (* ??? *)

    (* v : T.ty -> A.exp list * A.exp *)
    fun fresh t = let val x = freshVar t in ([x], x) end

    (* argTy : A.exp -> T.ty option *)
    (* If the given expression is a non-nullary data constructor, return *)
    (*   SOME argument type; otherwise return NONE. *)
    fun argTy (A.ConstExp (A.DConst (d, _))) = DataCon.argTypeOf d
                               (* note: argTypeOf already returns an option *)
      | argTy _ = NONE

    (* mkVars : A.exp -> A.exp list * A.exp *)
    (* Produces a flat list of variables and the properly-shaped tuple *)
    (*   of variables and data constructors. *)
    fun mkVars (app as A.ApplyExp (e1, e2, t)) = 
	(* TODO: make this recursive *)
	if (isDCon e1) andalso (isDConApp e2) then
	    let val c2 = (case mkVars e2
                            of (_, A.ApplyExp (c, _, _)) => c
			     | _ => fail "mkVars: not a function")
		val c2ArgTy = (case TypeOf.exp c2
                                 of T.FunTy (d, _) => d
				  | _ => fail "mkVars: not a function")
		val x = freshVar c2ArgTy
	    in
		([x], A.ApplyExp (compose (e1, c2), x, t))
	    end
	else 
	    (case argTy e1
 	       of SOME at => 
		  let val x = freshVar at
		  in
		      ([x], A.ApplyExp (e1, x, t))
		  end
		| NONE => fresh t
  	      (* end case *))
      | mkVars (A.TupleExp es) = 
  	  let val (vss, xs) = ListPair.unzip (map mkVars es)
	  in
	      (List.concat vss, A.TupleExp xs)
	  end
      | mkVars (A.PTupleExp es) = 
	  let val (vss, xs) = ListPair.unzip (map mkVars es)
	  in
	      (List.concat vss, A.TupleExp xs)
	  end
      | mkVars e = fresh (TypeOf.exp e)

    (* fromExp : A.exp -> A.var * A.lambda *)
    (* Pre: The argument is a tuple. *)
    fun fromExp e = 
	let fun exps es =
		let val _ = FreshVar.resetVarNames ()
		    val (ves, e') = mkVars e
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
	val test = U.test fromExp'
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

    in

        fun test0 () = test t0
	fun test1 () = test t1
	fun test2 () = test t2
	fun test3 () = test t3
	fun test4 () = test t4
	fun test5 () = test t5
	fun test6 () = test t6
	fun test7 () = test t7

    end

  end
