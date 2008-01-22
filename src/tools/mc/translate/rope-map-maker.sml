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

    val anyTy = BTy.T_Any
    val boolTy = BB.boolTy
    val exhTy = BTy.exhTy
    val exnTy = BTy.exnTy
    val futureTy = BTy.futureTy
    val intTy = BB.intTy
    val listTy = BB.listTy
    val rawIntTy = BTy.T_Raw BTy.T_Int
    val ropeTy = BB.ropeTy
    val tupTy = BTy.T_Tuple
    val unitTy = BTy.unitTy
    val workQueueTy = BTy.workQueueTy

  (* iPairTy : BTy.ty * BTy.ty -> BTy.ty *)
  (* Consumes two types. *)
  (* Produces the corresponding immutable pair type. *)
    fun iPairTy (t1 : BTy.ty, t2 : BTy.ty) : BTy.ty = BTy.T_Tuple (false, [t1, t2])

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
	    (* build var list * var list * var -> exp *)
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

  (* mkLeafCase : int *  B.var * B.var * B.var * B.var * B.var -> B.pat * B.exp *)
    fun mkLeafCase (arity, shortV, startV, iV, othersV, exhV) : B.pat * B.exp =
	let (* variables *)
	    val mlLenV = BV.new ("ml_len", intTy)
	    val dataV = BV.new ("data", listTy)
	    val othersVs = mkVars ("others", arity-1, listTy) (* backwards *) 
            val lenV = BV.new ("len", rawIntTy)
	    val getV = BV.new ("get", BTy.T_Fun ([ropeTy], [exhTy], [iPairTy (listTy, boolTy)]))
	    val othersXV = BV.new ("othersX", listTy)
	    val argTy =  raise Fail "todo: [fun (any / exh -> any), list, ..., list]"
	    val argV = BV.new ("arg", argTy)
	    val allV = BV.new ("all", listTy)
	    val dVs = rev (mkVars ("d", arity, listTy)) (* forwards *)
	    val dataXV = BV.new ("dataX", listTy)
	    val eV = BV.new ("e", ropeTy)
	    (* misc *)
            (* prependOtherVs : B.var list * B.exp -> B.exp *)
	    fun prependOtherVs (vs, e) =
		let fun mkNth n = 
                        (* FIXME -- This tupling/alloc should be rejiggered -- eliminated. *)
			let val nStr = Int.toString n
			    val nV = BV.new ("n" ^ nStr, intTy)
			    val tupTy =  iPairTy (listTy, intTy)
			    val tupV  = BV.new ("tup" ^ nStr, tupTy)
			in
			    B.mkStmt ([nV], B.E_Const (Literal.Int (IntInf.fromInt n), intTy),
                             B.mkStmt ([tupV], B.E_Alloc (tupTy, [othersV, nV]),
                              B.mkHLOp (HLOpEnv.listNthOp, [tupV], [exhV])))
			end
		    fun p ([], _, e) = e
		      | p (v::vs, n, e) = B.mkLet ([v], mkNth n, p (vs, n+1, e))
		in
		    p (vs, 0, e)
		end
	    val leafPat = B.P_DCon (BB.ropeLeaf, [mlLenV, dataV])
	    val leafBody = 
              prependOtherVs (othersVs, 
               B.mkStmt ([lenV], B.unwrap mlLenV,
                B.mkLet ([getV], B.mkHLOp (raise Fail "todo: HLOpEnv.CURRIED_ROPE_SUBLIST, [startV, lenV], [exhV]"),
                 B.mkStmt ([argV], B.E_Alloc (argTy, getV::(rev othersVs)),
                  B.mkLet ([othersXV], raise Fail "todo: apply list-map-[n] (arg / exh)",
                   B.mkLet ([allV], raise Fail "todo: @insert-at (data, othersX, i / exh)",
                    B.mkLet (dVs, raise Fail "todo: apply list-to-tup-[n] (all)",
                     B.mkLet ([dataXV], raise Fail "todo: apply list-map-[n] (f, d1, ..., dn)",
                      B.mkStmt ([eV], raise Fail "todo: LEAF (ml_len, dataX)",
                       B.mkRet [eV])))))))))
	in	
	    (leafPat, leafBody)
	end 
     
  (* mkCatBody : B.var * B.var * B.var * B.var -> B.exp *)
    fun mkCatBody (shortV, startV, innerMapV, mlLenV, mlDepthV, shortLV, shortRV, exhV) : B.exp =
	let (* types *)
	    val thunkTy = BTy.T_Fun ([unitTy], [exhTy], [ropeTy])
	    val qtTy = iPairTy (workQueueTy, thunkTy)
	    val qfTy = iPairTy (workQueueTy, futureTy)
	    (* variables *)
	    val lenLV = BV.new ("lenL", rawIntTy)
	    val startRV = BV.new ("startR", rawIntTy)
	    val thunkV = BV.new ("thunk", thunkTy)
	    val qtV = BV.new ("qt", qtTy)
	    val shortRX_FV = BV.new ("shortRX_F", futureTy)
	    val shortLXV = BV.new ("shortLX", ropeTy)
	    val qfV = BV.new ("qf", qfTy)
	    val shortRXV = BV.new ("shortRX", ropeTy)
	    val cV = BV.new ("c", ropeTy)
	    (* misc *)
	    val thunkLam = B.FB {f = thunkV,
				 params = [BV.new ("u", unitTy)],
				 exh = [exhV],
				 body = B.mkApply (innerMapV, [shortRV, startRV], [])}
	    (* apply innerMapV (shortRV, startRV) *)
	    val retVal = B.E_DCon (BB.ropeCat, [mlLenV, mlDepthV, shortLXV, shortRXV])
	in
	    B.mkLet ([lenLV], B.mkHLOp (HLOpEnv.ropeLengthIntOp, [shortLV], [exhV]),
             B.mkStmt ([startRV], B.E_Prim (Prim.I32Add (startV, lenLV)),
              B.mkFun ([thunkLam], 
               B.mkStmt ([qtV], B.E_Alloc (qtTy, [raise Fail "qV", thunkV]),
                B.mkLet ([shortRX_FV], B.mkHLOp (HLOpEnv.future1Op, [qtV], [exhV]),
                 B.mkLet ([shortLXV], B.mkApply (innerMapV, [shortLV, startV], []),
                  B.mkStmt ([qfV], B.E_Alloc (qfTy, [raise Fail "qV", shortRX_FV]),
                   B.mkLet ([shortRXV], B.mkHLOp (HLOpEnv.touch1Op, [qfV], [exhV]),
                    B.mkStmt ([cV], retVal,
                     B.mkRet [cV])))))))))
	end

  (* mkCatCase : B.var * B.var * B.var * B.var -> B.pat * B.exp *)
    fun mkCatCase (shortV : B.var, startV : B.var, innerMapV : B.var, exhV : B.var) 
                  : B.pat * B.exp =
	let val mlLenV = BV.new ("ml_len", intTy)
	    val mlDepthV = BV.new ("ml_d", intTy)
	    val shortLV = BV.new ("shortL", ropeTy)
	    val shortRV = BV.new ("shortR", ropeTy)
	    val catPat = B.P_DCon (BB.ropeCat, [mlLenV, mlDepthV, shortLV, shortRV])
	    val catBody = mkCatBody (shortV, startV, innerMapV, 
				     mlLenV, mlDepthV, shortLV, shortRV, exhV)

	in
	    (catPat, catBody)
	end

  (* mkInnerMap : int * B.var * B.var * B.var -> B.lambda *)
    fun mkInnerMap (arity : int, indexV : B.var, othersV : B.var, exhV : B.var) : B.lambda =
	let val innerMapV = BV.new ("rmap" ^ Int.toString arity,  
			     BTy.T_Fun ([ropeTy, rawIntTy], [], [ropeTy]))
	    val shortV = BV.new ("short", ropeTy)
	    val startV = BV.new ("start", rawIntTy)
	    val leafCase = mkLeafCase (arity, shortV, startV, indexV, othersV, exhV)
	    val catCase = mkCatCase (shortV, startV, innerMapV, exhV)
	    val body = B.mkCase (shortV, [leafCase, catCase], NONE)
	in
	    B.FB {f = innerMapV,
		  params = [shortV, startV],
		  exh = [],
		  body = body}
	end

  (* mkMap : int -> B.exp *)
    fun mkMap (arity : int) : B.exp =
	let val exhV = BV.new ("exh", exhTy)
	    val lengthExn = BV.new ("Length", exnTy)
	    fun mkRaise () = B.mkThrow (exhV, [lengthExn])
	    val l2t = mkListToTup (arity, mkRaise)
            val fTy = BTy.T_Fun (copies (arity, anyTy), [exhTy], [anyTy])
	    val f = BV.new ("f", fTy)
	    val argTy = tupTy (false, fTy :: copies (arity, ropeTy)) 
	    val rmapTy = BTy.T_Fun ([argTy], [exhTy], [ropeTy])
	    val rmapVar = BV.new ("rope_map_" ^ Int.toString arity, rmapTy)
	    val ropeListVar = BV.new ("ropes", listTy)
	    val ropeVars = List.tabulate (arity,
				       fn n => BV.new ("rope" ^ Int.toString (n+1), ropeTy))
	    val indexVar = BV.new ("i", rawIntTy)
	    val shortestVar = BV.new ("s", ropeTy)
	    val othersVar = BV.new ("others", listTy)
	    val innerMap as B.FB {f=rmapn, ...} = mkInnerMap (arity, indexVar, othersVar, exhV)
	    val body = mkList (ropeListVar, ropeVars,
                         B.mkLet ([indexVar, shortestVar, othersVar], 
                           B.mkHLOp (HLOpEnv.extractShortestRopeOp, [ropeListVar], [exhV]),
                             B.mkFun ([innerMap],
                               B.mkApply (rmapn, [shortestVar], []))))
	    val rmapLam = B.FB {f = rmapVar,
				params = f::ropeVars,
				exh = [exhV],
				body = body}
	in
            (* FIXME: mkMap should return a B.lambda *)
	    B.mkFun ([rmapLam], B.mkRet [rmapVar])
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
