(* list-map-maker.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utilities to generate polymorphic sequential map functions of various arities.
 *)

structure ListMapMaker : sig

  (* these are intended to be removed from the signature *)
    val mkMap : int -> BOM.lambda
    val test  : int -> unit

  (* The following will retrieve the desired map function from a cache, or
   * synthesize the appropriate function, stash it in the cache, and return it. *)
    val gen : int -> BOM.lambda

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
    val exnTy = BTy.exnTy
    val exhTy = BTy.exhTy

    val nilConst = (Literal.Enum 0w0, listTy)

    (* copies : int * 'a -> 'a list *)
    fun copies (n, x) = List.tabulate (n, fn _ => x)

    (* mkFPrime : int * BV.var -> B.lambda *)
    (* P: to construct an inner "de-tupled" version of the function being mapped. *)
    (* FIXME? Should I rely on the optimizer to do this? *)
    fun mkFPrime (arity: int, fV: BV.var, exhV: BV.var) : B.lambda =
	let fun build (n: int, xs: BV.var list, manyAny: BTy.ty list) : B.lambda =
		if n>0 then 
		    let val x = BV.new ("any" ^ Int.toString n, anyTy)
		    in
			build (n-1, x::xs, anyTy::manyAny)
		    end
		else if n=0 then
		    let val fPrimeTy = BTy.T_Fun (manyAny, [], [anyTy])  
			val tTy = tupTy (false, manyAny)
			val tupV = BV.new ("tup", tTy)
			val rV = BV.new ("r", anyTy)
			val body = B.mkStmt ([tupV], B.E_Alloc (tTy, xs),
                                   B.mkLet  ([rV], B.mkApply (fV, [tupV], [exhV]),
                                   B.mkRet [rV]))
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

    (* mkVars : string * int * BTy.ty -> BV.var list *)
    (* P: to build a list of numbered variable names, all of same type, backwards. *)
    (* ex: mkVars ("foo", 3, any) --> [foo3 : any, foo2 : any, foo1 : any] *)
    fun mkVars (prefix: string, n: int, t: BTy.ty) : BV.var list =
	let fun f (k, acc) =
		if k>n 
		then acc
		else f (k+1, BV.new (prefix ^ Int.toString k, t) :: acc)
	in
	    if (n<1)
	    then raise Fail "mkVars: BUG"
	    else f (1, [])
	end

    val nilPat = B.P_Const nilConst
    
    fun mkConsPat (hdVar, tlVar) = B.P_DCon (BB.listCons, [hdVar, tlVar])

    local
	val revHLOp = HLOpEnv.listRevOp
	fun zip3Eq (xs, ys, zs) =
	    let fun f ([], [], [], acc) = rev acc
		  | f (x::xs, y::ys, z::zs, acc) = f (xs, ys, zs, (x,y,z)::acc)
		  | f _ = raise Fail "unequal lengths"
	    in
		f (xs, ys, zs, [])
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
		let val t = BTy.T_Fun (copies (arity+1, listTy), (* one extra argument for accumulator *)
				       [],
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
	    val body = buildCase (tl (zip3Eq (listVars, hdVars, tlVars)), innermostCase)
	in
	    B.FB {f = thisFuncVar,
		  params = rev (accVar::listVars),
		  exh = [],
		  body = body}
	end

    end (* locals for mkInnerMap  *)

    local
	fun lamV (B.FB {f, ...}) = f
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
    fun mkMap (arity: int) : B.lambda = 
	let val _ = if (arity < 2) then raise Fail "mkMap: arity must be at least 2" else ()
	    val fTy = BTy.T_Fun ([tupTy (false, copies (arity, anyTy))],
				 [exhTy],
				 [anyTy])
	    val fV = BV.new ("f", fTy)
	    val exhV = BV.new ("exh", exhTy)
	    val fPrime = mkFPrime (arity, fV, exhV)
	    val innerMap = mkInnerMap (arity, lamV fPrime, exhV)
	    val argsTy = fTy :: copies (arity, listTy)
	    val mapTy = BTy.T_Fun (argsTy, [exhTy], [listTy])
	    val mapV = BV.new ("list_map_" ^ Int.toString arity, mapTy) 
	    val listVs =
		let fun mk n = BV.new ("list" ^ Int.toString (n+1), listTy)
		in
		    List.tabulate (arity, mk)
		end
	    val nilV = BV.new ("nil", listTy)
	    val resultV = BV.new ("result", listTy)
	    val body = B.mkStmt ([nilV], B.E_Const nilConst, 
                       B.mkFun ([fPrime],
                       B.mkFun ([innerMap],
                       B.mkLet ([resultV],
                       B.mkApply (lamV(innerMap), listVs @ [nilV], []),
                       B.mkRet [resultV]))))
	in
	    B.FB {f = mapV,
		  params = fV :: listVs,
		  exh = [exhV],
		  body = body}
	end
    end (* locals for mkMap *)

    structure MapFnCache = CacheFn(struct 
				       type t = B.lambda
				       val mkItem = mkMap
				     end)

    val gen : int -> B.lambda = MapFnCache.getItem

    (* TESTS FOLLOW *)

    local 
	fun println s = (print s; print "\n")
	fun printLam (lam as B.FB {f, ...}) = 
	    PrintBOM.printExp (B.mkFun ([lam], B.mkRet [f]))
	val f0 = BV.new ("f", BTy.T_Fun ([tupTy (false, [anyTy, anyTy])],
					 [anyTy],
					 [anyTy]))
	val exh0 = BV.new ("exh", anyTy)
    in
    fun test 0 = 
	  let val f' = mkFPrime (2, f0, exh0)
	  in
	      PrintBOM.printExp (B.mkFun ([f'], B.mkRet [exh0]))
	  end
      | test 1 = printLam (mkMap 2)
      | test 2 = printLam (mkMap 3)
      | test 3 = printLam (gen 4)
      | test _ = println "No such test."
    end (* local *)

  end (* struct *)
