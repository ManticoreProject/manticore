(* nester.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A function for building nesters based on tuples.
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

    local
	structure VM = RedBlackMapFn (struct
				       type ord_key = A.var
				       val compare = Var.compare
				      end)
	(* isDCon : A.exp -> bool *)
	fun isDCon (A.ConstExp (A.DConst _)) = true
	  | isDCon _ = false
	(* dcon : A.exp -> A.dcon *)
	fun dcon (A.ConstExp (A.DConst (c, _))) = c
	  | dcon _ = fail "dcon"
    in
    (* same : A.lambda * A.lambda -> bool *)
    (* Nesters are restricted enough that they can be compared for equality. *)
    (* Pre: Both arguments are in fact nesters. *) 
    (* There may be no test to verify this (have to think about that one). *)
    (* As such, care must be taken when invoking this function. *)
    (* The implementation depends on nesters having the following form: *)
    (* fun nest x = case x of (a1, ..., an) => (a1, (a2, a3), ...) *)
    fun same (n1 as A.FB (_, _, b1), n2 as A.FB (_, _, b2)) =
	let val (p1, e1) = (case b1
			      of A.CaseExp (_, [(p, e)], _) => (p, e)
			       | _ => fail "same: n1 is not a nester")
	    val (p2, e2) = (case b2
			      of A.CaseExp (_, [(p, e)], _) => (p, e)
			       | _ => fail "same: n2 is not a nester")
	    (* pat : A.pat * A.pat -> bool *)
	    (* Both pats must be flat tuples of vars, and have the same length. *)
	    fun pat (A.TuplePat ps1, A.TuplePat ps2) = 
		  let (* ok : A.pat list * A.pat list -> bool *)
		      fun ok ([], []) = true
			| ok (A.VarPat _ :: t1, A.VarPat _ :: t2) = ok (t1, t2)
			| ok _ = false
		  in
		      ok (ps1, ps2)
		  end
	      | pat _ = false
	    (* exp : A.exp * A.exp -> bool *)
	    (* Both exps must consist only of variables and tuples and datacons. *)
	    (* They must be isomorphic, that is, identical up to variable renaming. *)
	    fun exp (e1, e2) = 
		let type vvmap = A.var VM.map
		    (* e : vvmap * A.exp * A.exp -> vvmap option  *)
		    (* Returns the isomorphism for e1 and e2, if it exists. *)
		    fun e (m, A.VarExp(v1,_), A.VarExp(v2,_)) =
			  (case VM.find (m, v1)
			     of SOME v => if Var.same (v, v2)
					  then SOME m
					  else NONE
			      | NONE => SOME (VM.insert (m, v1, v2)))
		      | e (m, A.TupleExp es1, A.TupleExp es2) = es (m, es1, es2)
		      | e (m, A.ApplyExp (c1, e1, _), A.ApplyExp (c2, e2, _)) =
			  if isDCon c1 andalso isDCon c2 andalso 
			     DataCon.same (dcon c1, dcon c2) 
			  then e (m, e1, e2)
			  else NONE
		      | e _ = NONE
		    (* es : vvmap * A.exp list * A.exp list -> vvmap option *)
		    and es (m, [], []) = SOME m
		      | es (m, e1::es1, e2::es2) = 
			  (case e (m, e1, e2)
		 	     of SOME m' => es (m', es1, es2)
			      | NONE => NONE)
		      | es _ = NONE	
		in
		    case e (VM.empty, e1, e2)
		      of SOME isomorphism => true
		       | NONE => false
		end
	in
	    pat (p1, p2) andalso exp (e1, e2)
	end
    end (* local *)

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

	(* t9 = (| SOME (| SOME 15, SOME 16 |) |) *)
	val t9 = U.ptup [U.some (U.ptup [U.some (U.int 15), 
					 U.some (U.int 16)])]
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

	(* testSame : int -> unit *)
	fun testSame n =
	    let fun t (e1, e2) =
		    let val (_, lam1) = fromExp e1
			val (_, lam2) = fromExp e2
			val s = same (lam1, lam2)
		    in
			PrintAST.print (funFromLam lam1);
			PrintAST.printComment "****";
			PrintAST.print (funFromLam lam2);
			PrintAST.printComment ("same: " ^ Bool.toString s)
		    end
	    in
		case n
 		 of 0 => t (t8, t9)
		  | 1 => t (t7, t8)
		  | _ => print (Int.toString n ^ ": no such test for testSame.\n")
	    end
    end

  end
