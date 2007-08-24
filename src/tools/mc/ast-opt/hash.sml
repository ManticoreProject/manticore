(* hash.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * n.b. This is hash in the sense of ML's #, not a hash function, table, etc.
 *)

structure Hash (* : sig

    val mkHash : int * Types.ty list -> AST.lambda

  end *) = 

  struct

    structure A = AST
    structure T = Types
    structure F = Futures

    (* fail : string -> 'a *)
    fun fail msg = raise Fail msg

    (* todo : string -> 'a *)
    fun todo thing = fail ("todo: " ^ thing)

    (* patAndVar : T.ty list * int -> A.pat * Var.var *)
    (* Given a list of types and an index (1-based) into that list, *)
    (* return a tuple pattern, appropriately typed, and the var to select *)
    (* out of that pattern. *)
    fun patAndVar (ts, i) =
	let (* build : T.ty list * int * A.pat list * Var.var option
                       -> A.pat * Var.var *)
	    fun build ([], _, acc, ov) = 
		  let val x = case ov
			        of SOME x => x
				 | NONE => fail ("patAndVar: n is outside the interval\
                                                \ [1," ^ Int.toString (length ts) ^ "]")
		  in
		      (A.TuplePat (rev acc), x)
		  end
	      | build (t::ts, n, acc, ov) =
		  let val xn = Var.new ("x" ^ Int.toString n, t)
		      val ov' = if (n=i) then SOME xn else ov
		  in
		      build (ts, n+1, A.VarPat xn :: acc, ov')
		  end
	in
	    build (ts, 1, [], NONE)
	end

    (* mkHash : int * T.ty list -> A.var * A.lambda *)
    (* n.b. This is hash in the sense of ML's #, not a hash function, table, etc. *)
    (* Pre: n is on [1, length ts]. *)
    fun mkHash (n, ts) =
	let val resultTy = List.nth (ts, n-1)
	    val hashTy = T.FunTy (T.TupleTy ts, resultTy)
	    val hashVar = Var.newWithKind ("hash" ^ Int.toString n,
					   A.VK_Fun,
					   hashTy)
	    val (tuplePat, varOfInterest) = patAndVar (ts, n)
	    val arg = Var.new ("t", T.TupleTy ts)
	    val hashDef = A.FB (hashVar,
				arg,
				A.CaseExp (A.VarExp (arg, []),
					   (* ??? Is it right not to have any types in this list? ??? *)
					   [(tuplePat,
					     A.VarExp (varOfInterest, []))],
					   (* ??? Or this one? ??? *)
					   resultTy))
	in
	    (hashVar, hashDef)
	end

    (**** tests ****)

    local

	structure U = TestUtils

	(* testPVal : int * T.ty list -> unit *)
	fun tst (n, ts) = 
	    let val (hashVar, hashDef) = mkHash (n, ts)
		val exp = A.LetExp (A.FunBind [hashDef],
				    A.VarExp (hashVar, []))
	    in
		PrintAST.print exp;
		PrintAST.printComment "whose type is";
		PrintTypes.printTy (TypeOf.exp exp)
	    end

        (* t0 *)
	val t0 = (1, [Basis.intTy, Basis.boolTy]) 

        (* t1 *)
	val t1 = (2, [Basis.intTy, Basis.boolTy])

        (* t2 *)
	val t2 = (3, [Basis.intTy, Basis.boolTy, F.futureTy Basis.floatTy])

    in
        (* test : int -> unit *)
        val test = U.mkTest tst [t0,t1,t2]

    end

  end
