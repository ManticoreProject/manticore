(* rope-map-maker.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utilities to generate polymorphic parallel map functions over ropes in BOM.
 *)

structure RopeMapMaker : sig

  (* these are intended to be removed from the signature *)
    val mkList : BOM.var * BOM.var list * BOM.exp -> BOM.exp
    val test : int -> unit

  (* The following will retrieve the desired map function from a cache, or
   * synthesize the appropriate function, stash it in the cache, and return it. *)
    val getMapFunction : int -> BOM.exp

  end = struct

    structure A = AST
    structure B = BOM
    structure BB = BOMBasis
    structure BTy = BOMTy
    structure BV = B.Var
    structure MB = Basis

    val listTy = BB.listTy
    val tupTy = BTy.T_Tuple
    val ropeTy = BTy.ropeTy
    val intTy = BTy.T_Raw RawTypes.T_Int
    val anyTy = BTy.T_Any
    val exnTy = BTy.exnTy
    val exhTy = BTy.exhTy

    val nilConst = (Literal.Enum 0w0, listTy)

    (* copies : int * 'a -> 'a list *)
    fun copies (n, x) = List.tabulate (n, fn _ => x)

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

  (* mkListToTup : int * (unit -> B.exp) -> B.lambda *)
  (* Consumes an arity and a mkRaise function. *)
  (* (The latter is for inserting raises in the generated code.) *)
  (* Produces a function that turns a list of lists of that many elements *)
  (* into a tuple of lists of that many elements. *)
  (* The lists need not have a common element type. *)
  (* This is a function that can't be typed in H-M systems; it's just *)
  (* for local, under-the-hood use. *)
    fun mkListToTup (arity : int, mkRaise : unit -> B.exp) : B.lambda =
	let val returnTy = tupTy (false, copies (arity, listTy))
	    val xss = BV.new ("xss", listTy)
	    val xsVars = mkVars ("xs", arity, listTy) (* these are backwards *)
	    val tlVars = mkVars ("tl", arity, listTy) (* these are backwards *)
	    val tlN = hd tlVars
            val innermostCase =
		let val consPat = mkConsPat (BV.new("_",listTy), BV.new("_",listTy))
		    val retval = 
			let val result = BV.new ("result", returnTy)
			in
			    B.mkStmt ([result],
                              B.E_Alloc (returnTy, rev xsVars),
                                B.mkRet [result])
			end
		in
		    B.mkCase (tlN,
                      [(consPat, mkRaise ()),
                       (nilPat, retval)],
                      NONE)
		end
	    fun build ([xs1], [tl1], e) = 
		  B.mkCase (xss,
                    [(nilPat, mkRaise ()),
		     (mkConsPat (xs1, tl1), e)],
                    NONE)
	      | build (xsK::xsTl, tlK::(tlTl as tlKPred::_), e) = 
		  let val e' = B.mkCase (tlKPred,
                                 [(nilPat, mkRaise ()),
				  (mkConsPat (xsK, tlK), e)],
                                 NONE)
                  in
		      build (xsTl, tlTl, e')
		  end
	      | build _ = raise Fail "mkListToTup: BUG"
	    val body = build (xsVars, tlVars, innermostCase)
	    val fTy = BTy.T_Fun ([listTy], [], [returnTy]) 
	    val fVar = BV.new ("list_to_tup_" ^ Int.toString arity, fTy)
	in
	    B.FB {f = fVar,
		  params = [xss],
		  exh = [],
		  body = body}
	end

  (* mkCons : B.var * B.var -> B.rhs *)
    fun mkCons (h : B.var, t : B.var) : B.rhs = B.E_DCon (BB.listCons, [h, t])

  (* mkList : B.var * B.var list * B.exp -> B.exp *)
  (* Produces an expression that binds x to the *list* of vars in e. *)
  (* Said list needs to be built with incremental CONSes. *)
    fun mkList (wholeList : B.var, ys : B.var list, e : B.exp) : B.exp =
	let val nilVar = BV.new ("nil", listTy)
	    fun build ([y], [], prev) = B.mkStmt ([wholeList], mkCons (y, prev), e)
	      | build (y::ys, curr::more, prev) = 
		  B.mkStmt ([curr], mkCons (y, prev), build (ys, more, curr))
	      | build _ = raise Fail "mkList: BUG"
	    val listVars = List.tabulate (length(ys) - 1, 
				       fn n => BV.new ("list" ^ Int.toString (n+1), listTy))
	in
	    B.mkStmt ([nilVar], B.E_Const nilConst, (* FIXME - I don't want to do this with a var *)
              build (rev ys, listVars, nilVar))
	end

  (* mkLeafCaseBody : B.var * B.var * B.var * B.var -> B.exp *)
    fun mkLeafCaseBody (dataVar : B.var, startPosVar : B.var, othersVar : B.var, indexVar : B.var) : B.exp =
	raise Fail "todo"

  (* mkCatCaseBody : B.var * B.var * B.var * B.var -> B.exp *)
    fun mkCatCaseBody (innerMapVar : B.var, startPosVar : B.var, lVar : B.var, rVar : B.var) : B.exp =
	raise Fail "todo"
	
  (* mkInnerMap : int * B.var * B.var -> B.lambda *)
    fun mkInnerMap (arity : int, indexVar : B.var, othersVar : B.var) : B.lambda =
      let val fTy = BTy.T_Fun ([ropeTy, intTy], [], [ropeTy])
	  val fVar = BV.new ("rmap" ^ Int.toString arity, fTy)
	  val ropeListVar = BV.new ("ropes", listTy)
	  val ropeVars = List.tabulate (arity,
				     fn n => BV.new ("rope" ^ Int.toString (n+1), ropeTy))
	  val shortestVar = BV.new ("short", ropeTy)
	  val startPosVar = BV.new ("start", intTy)
	  val dataVar = BV.new ("data", listTy)
	  val leafCaseBody = mkLeafCaseBody (dataVar, startPosVar, othersVar, indexVar)
	  val shortLVar = BV.new ("shortL", ropeTy)
	  val shortRVar = BV.new ("shortR", ropeTy)
	  val catCaseBody = mkCatCaseBody (fVar, startPosVar, shortLVar, shortRVar)
      in
	  raise Fail "todo"
      end

  (* mkMap : int -> B.exp *)
    fun mkMap (arity : int) : B.exp =
	let val exh = BV.new ("exh", exhTy)
	    val lengthExn = BV.new ("Length", exnTy)
	    fun mkRaise () = B.mkThrow (exh, [lengthExn])
	    val l2t = mkListToTup (arity, mkRaise)
            val argTy = 
		let val fTy = BTy.T_Fun (copies (arity, anyTy), [exhTy], [anyTy])
		in
		    tupTy (false, fTy :: copies (arity, ropeTy))
		end
	    val rmapTy = BTy.T_Fun ([argTy], [exhTy], [ropeTy])
	    val rmapVar = BV.new ("rope_map_" ^ Int.toString arity, rmapTy)

	in
	    raise Fail "todo"
	end

    structure MapFnCache = CacheFn(struct 
				     type t = B.exp
				     val mkItem = mkMap
				   end)

    val getMapFunction : int -> BOM.exp = MapFnCache.getItem

    (* TESTS FOLLOW *)

    local 
	fun println s = (print s; print "\n")
	val f0 = BV.new ("f", BTy.T_Fun ([tupTy (false, [anyTy, anyTy])],
					 [anyTy],
					 [anyTy]))
	val exn0 = BV.new ("Length", exnTy)
	val exh0 = BV.new ("exh", anyTy)
	fun mkRaise () = B.mkThrow (exh0, [exn0])

	(* mkListToTupExp : int * (unit -> B.exp) -> B.exp *)
	fun mkListToTupExp (n, mkRs) =
	    let val f as B.FB {f=fVar, ...} = mkListToTup (n, mkRs)
	    in
		B.mkFun ([f], B.mkRet [fVar])	    
	    end

	val showMe = PrintBOM.printExp
    in
    fun test 0 = showMe (mkListToTupExp (2, mkRaise))
      | test 1 = showMe (mkListToTupExp (3, mkRaise))
      | test 2 = showMe (mkListToTupExp (6, mkRaise))
      | test 3 = 
	  let val x = BV.new ("x", listTy)
	      val vars = map (fn n => BV.new ("y" ^ Int.toString n, anyTy)) [1,2,3,4,5]
	  in
	      showMe (mkList (x, vars, mkRaise ()))
	  end
      | test _ = println "No such test."
    end (* local *)

  end (* struct *)
