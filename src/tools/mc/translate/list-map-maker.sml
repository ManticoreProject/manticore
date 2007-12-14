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
    structure BB = BOMBasis
    structure BTy = BOMTy
    structure BV = B.Var
    structure MB = Basis

    val listTy = BB.listTy
    val tupTy = BTy.T_Tuple
    val anyTy = BTy.T_Any
    val exhTy = BTy.exhTy

    (* copies : int * 'a -> 'a list *)
    fun copies (n, x) = List.tabulate (n, fn _ => x)

    (* mkFPrime : int * BV.var -> B.lambda *)
    (* P: to construct an inner "de-tupled" version of the function being mapped. *)
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

    local
	val revHLOp = HLOpEnv.listRevOp
	val nilPat = raise Fail "todo: nilPat"
	fun mkConsPat (hdVar, tlVar) = B.P_DCon (BB.listCons, [hdVar, tlVar])
	fun zip3Eq (xs, ys, zs) =
	    let fun f ([], [], [], acc) = rev acc
		  | f (x::xs, y::ys, z::zs, acc) = f (xs, ys, zs, (x,y,z)::acc)
		  | f _ = raise Fail "unequal lengths"
	    in
		f (xs, ys, zs, [])
	    end
	(* mkVars : string * int * BTy.ty -> BV.var list *)
	(* P: to build a list of numbered variable names, all of same type, backwards. *)
	(* ex: mkVars ("foo", 3, any) --> [foo3 : any, foo2 : any, foo1 : any] *)
	fun mkVars (prefix: string, n: int, t: BTy.ty) : BV.var list =
	    let fun f (k, acc) =
		    if k>n 
		    then acc
		    else f (k+1, BV.new (prefix ^ Int.toString n, t) :: acc)
	    in
		if (n<1)
		then raise Fail "mkVars: BUG"
		else f (1, [])
	    end
    in
    (* mkInnerMap : int * BV.var * BV.var -> B.lambda *)
    (* P: to build an inner map function with an accumulator and baked-in parameters. *)
    fun mkInnerMap (arity: int, fPrime: BV.var, exh: BV.var) : B.lambda = 
	let val listVars = mkVars ("list", arity, listTy) (* remember, these are backwards *)
	    val hdVars = mkVars ("hd", arity, anyTy)      (* remember, these are backwards *)
	    val tlVars = mkVars ("tl", arity, listTy)     (* remember, these are backwards *)
	    val accVar = BV.new ("acc", listTy)
	    val thisFuncVar = 
		let val t = BTy.T_Fun (copies (arity+1, listTy), (* one extra for accumulator *)
				       [exhTy],
				       [listTy])
		in
		    BV.new ("map_" ^ Int.toString arity, t)
		end
	    val revAcc = B.mkHLOp (revHLOp, [accVar], [exh])
	    fun freshExit () = 
		let val ccaVar = BV.new ("cca", listTy)
		in
		    B.mkLet ([ccaVar], revAcc, B.mkRet [ccaVar])
		end
	    fun buildCase ([], acc) = acc
	      | buildCase ((listN, hdN, tlN) :: r, acc) =
		  let val consPat = mkConsPat (hdN, tlN)
		      val acc' = B.mkCase (listN, 
					   [(nilPat, freshExit()),
					    (consPat, acc)],
					   NONE)
		  in
		      buildCase (r, acc')
		  end
	    (* Note: the args to innermost case are in reverse order for convenience. *)
	    fun mkInnermostCase (lists as listN::_, hds as hdN::_, tls as tlN::_) = 
		let val consPat = mkConsPat (hdN, tlN)
		    val consCaseBody = 
			let val newHd = BV.new ("newHd", anyTy)
			    val newAcc = BV.new ("newAcc", listTy)
			    val r = BV.new ("r", listTy)
			in
			    B.mkLet ([newHd], B.mkApply (fPrime, rev hds, []),
                             B.mkStmt ([newAcc], B.E_DCon (BB.listCons, [newHd, accVar]),
                              B.mkLet ([r], B.mkApply (thisFuncVar, rev (newAcc::tls), []),
                               B.mkRet [r])))
			end                               
		in
		    B.mkCase (listN,
			      [(nilPat, freshExit ()),
			       (consPat, consCaseBody)],
			      NONE)
		end
	      | mkInnermostCase _ = raise Fail "mkInnermostCase: BUG"
	    val innermostCase = mkInnermostCase (listVars, hdVars, tlVars)
	    val body = buildCase (zip3Eq (listVars, hdVars, tlVars), innermostCase)
	in
	    B.FB {f = thisFuncVar,
		  params = rev (accVar::listVars),
		  exh = [exh],
		  body = body}
	end

    end (* locals for mkInnerMap  *)

    local
	fun lamVar (B.FB {f, ...}) = f
	fun mkListSelectors (ls: BV.var list, tuple: BV.var, e: B.exp) : B.exp =
	    let fun go ([], _, exp) = exp
		  | go (ln::ls, n, exp) = 
                      go (ls, n-1, B.mkStmt ([ln], B.E_Select (n, tuple), exp))
	    in
		go (rev ls, length ls, e)
	    end
    in
    (* The arity is be the number of arguments to the function to be mapped. *)
    (* e.g., mkMap(2) will generate a function of type *)
    (*  ('a * 'b -> 'c) * ('a list) * ('b list) -> ('c list) *)
    fun mkMap (arity: int) : B.exp = 
	let val fTy = BTy.T_Fun ([tupTy (false, copies (arity, anyTy))],
				 [exhTy],
				 [anyTy])
	    val fVar = BV.new ("f", fTy)
	    val exhVar = BV.new ("exh", exhTy)
	    val fPrime = mkFPrime (arity, fVar, exhVar)
	    val innerMap = mkInnerMap (arity, lamVar fPrime, exhVar)
	    val argTy = tupTy (false, fTy::copies(arity, listTy))
	    val mapTy = BTy.T_Fun ([argTy], [exhTy], [listTy])
	    val mapVar = BV.new ("list_map_" ^ Int.toString arity, mapTy) 
	    val fVar = BV.new ("f", fTy)
	    val listVars =
		let fun mk n = BV.new ("list" ^ Int.toString (n+1), listTy)
		in
		    List.tabulate (arity, mk)
		end
	    val argVar = BV.new ("arg", argTy)
	    val nilVar = raise Fail "todo"
	    val resultVar = BV.new ("result", listTy)
	    val body = B.mkStmt ([fVar], B.E_Select (0, argVar),
                        mkListSelectors (listVars, argVar,
                         B.mkLet ([resultVar],
                          B.mkApply (lamVar innerMap, listVars @ nilVar, []),
                           B.mkRet [resultVar])))
	    val lam = B.FB {f = mapVar,
			    params = argVar :: listVars,
			    exh = [exhVar],
			    body = body}
	in
	    B.mkFun ([fPrime, innerMap, lam], B.mkRet [mapVar])
	end
    end (* locals for mkMap *)

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
