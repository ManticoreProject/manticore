(* hash.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Hash (* : sig

    val mkHash : int * Types.ty list -> AST.lambda

  end *) = 

  struct

    structure A = AST
    structure T = Types

    (* fail : string -> 'a *)
    fun fail msg = raise Fail msg

    (* todo : string -> 'a *)
    fun todo thing = fail ("todo: " ^ thing)

    (* selectVar : int * A.pat -> Var.var *)
    (* Pre: p is a TuplePat of VarPats. *)
    (* Note: n is 1-based (in keeping with the conventions of #). *)
    fun selectVar (n, p) =
	let fun getVar (A.VarPat x) = x
	      | getVar _ = fail "not a VarPat"
	in
	    case p
	      of A.TuplePat ps => getVar (List.nth (ps, n-1))
	       | _ => fail "not a TuplePat"
	end

    (* typedPat : T.ty list -> A.pat *)
    (* Given a list of types, produce a tuple pattern of such-typed variables. *)
    fun typedPat ts =
	let (* build : T.ty list * int * A.pat list -> A.pat list *)
	    fun build ([], _, acc) = rev acc
	      | build (t::ts, n, acc) = 
		  let val xn = Var.new ("x" ^ Int.toString n, t)
		  in
		      build (ts, n+1, A.VarPat xn :: acc)
		  end
	in
	    A.TuplePat (build (ts, 1, []))
	end

    (* mkHash : int * T.ty list -> A.lambda *)
    (* Pre: n is on [1, length ts]. *)
    fun mkHash (n, ts) =
	let val hashVar = Var.new ("hash" ^ Int.toString n,
				   T.FunTy (T.TupleTy ts,
					    List.nth (ts, n+1)))
	    val tuplePat = typedPat ts
	    val varOfInterest = selectVar (n, tuplePat)
	in
	    todo "mkHash"
	end

    (**** tests ****)

    local

	structure U = TestUtils

	(* testPVal : A.exp -> unit *)
	fun tst (n, ts) = todo "tst"

    in
        (* test : int -> unit *)
        val test = U.mkTest tst []

    end

  end
