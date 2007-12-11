(* list-map-maker.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utilities to generate polymorphic sequential map functions of various arities.
 *)

structure ListMapMaker : sig

    val mkMap : int -> BOM.exp
    val test  : int -> unit

  end = struct

    structure A = AST
    structure B = BOM
    structure BTy = BOMTy
    structure BV = B.Var
    structure MB = Basis

    val tupTy = BTy.T_Tuple
    val anyTy = BTy.T_Any

    (* mkFPrime : int * BV.var -> B.lambda *)
    fun mkFPrime (arity: int, f: BV.var, exh: BV.var) : B.lambda =
	let fun build (n: int, xs: BV.var list, manyAny: BTy.ty list) : B.lambda =
		if n>0 then 
		    let val x = BV.new ("any" ^ Int.toString n, anyTy)
		    in
			build (n-1, x::xs, anyTy::manyAny)
		    end
		else if n=0 then
		    let val fPrimeTy = BTy.T_Fun (manyAny, [], [anyTy])  
			val tTy = tupTy (false, manyAny)
			val tupVar = BV.new ("tup", tTy)
			val rVar = BV.new ("r", anyTy)
			val body = B.mkStmt ([tupVar], B.E_Alloc (tTy, xs),
                                     B.mkLet ([rVar], B.mkApply (f, xs, [exh]),
                                       B.mkRet [rVar]))
		    in
			B.FB {f = BV.new ("fPrime", fPrimeTy),
			      params = xs,
			      exh = [], (* fPrime has no exh by design:
                                         * it uses the outer-scope-level exh *)
			      body = body}
		    end
		else (* n<0 *)
		    raise Fail "mkFPrime: BUG"
	in
	    build (arity, [], [])
	end

    (* The arity is be the number of arguments to the function to be mapped. *)
    (* e.g., mkMap(2) will generate a function of type *)
    (*  ('a * 'b -> 'c) * ('a list) * ('b list) -> ('c list) *)
    fun mkMap arity = raise Fail "todo"

    (* TESTS FOLLOW *)

    fun println s = (print s; print "\n")

    val f0 = BV.new ("f", BTy.T_Fun ([tupTy (false, [anyTy, anyTy])],
				     [anyTy],
				     [anyTy]))

    val exh0 = BV.new ("exh", anyTy)

    fun test 0 = 
	  let val f' = mkFPrime (2, f0, exh0)
	  in
	      PrintBOM.printExp (B.mkFun ([f'], B.mkRet [exh0]))
	  end
      | test _ = println "No such test."

  end
