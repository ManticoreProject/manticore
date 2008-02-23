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
    val gen : int -> BOM.lambda

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
    val stringTy = BB.stringTy
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
  (* Consumes an arity and a mkFail function. *)
  (* (The latter is for inserting raises in the generated code.) *)
  (* Produces a function that turns a list of lists of that many elements *)
  (* into a tuple of lists of that many elements. *)
  (* The lists need not have a common element type. *)
  (* This is a function that can't be typed in H-M systems; it's just *)
  (* for local, under-the-hood use. *)
    fun mkListToTup (arity : int, mkFail : unit -> B.exp) : B.lambda =
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
                      [(consPat, mkFail ()),
                       (nilPat, retval)],
                      NONE)
		end
	    fun build ([xs1], [tl1], e) = 
		  B.mkCase (xss,
                    [(nilPat, mkFail ()),
		     (mkConsPat (xs1, tl1), e)],
                    NONE)
	      | build (xsK::xsTl, tlK::(tlTl as tlKPred::_), e) = 
		  let val e' = B.mkCase (tlKPred,
                                 [(nilPat, mkFail ()),
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
  (* FIXME no it doesn't! Just inline them... *)
  (*       .. but how??? *)
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

  (* listMapFun : int -> B.lambda *)
    fun listMapFun n = ListMapMaker.gen n

  (* listMapFunV : int -> B.var *)
    fun listMapFunV n = (let val B.FB {f,...} = listMapFun n in f end)

  (* foldr' : ('a * 'b -> 'b) * 'a list * 'b -> 'b *)
    fun foldr' (f, xs, z) = foldr f z xs

  (* numberFrom : 'a list -> ('a * int) list *)
    fun numberFrom (xs, n) = 
	let fun nf ([], _, acc) = rev acc
	      | nf (x::xs, n, acc) = nf (xs, n+1, (x,n)::acc)
	in
	    nf (xs, n, [])
	end 

  (* mkLeafCase : int * B.var * B.var * B.var * B.var * B.var * B.var * B.var -> B.pat * B.exp *)
    fun mkLeafCase (arity, shortV, startV, fV, iV, othersV, l2tV, exhV) : B.pat * B.exp =
	let (* types *)
            val argTy = (* a type for arguments to @list-map *)
		let val fty = BTy.T_Fun ([anyTy], [exhTy], [anyTy])
		in
		    BTy.T_Tuple (false, [fty, listTy])
		end  
            (* variables *)
 	    val dataV = BV.new ("data", listTy)
            val lenV = BV.new ("len", rawIntTy)
	    val getV = BV.new ("get", BTy.T_Fun ([ropeTy], [exhTy], [iPairTy (listTy, boolTy)]))
            val arg1V = BV.new ("arg1", argTy)
            val arg2V = BV.new ("arg2", argTy)
	    val hash1V = BV.new ("hash1", BTy.T_Fun ([iPairTy (listTy, boolTy)], [exhTy], [listTy]))
	    val sublistsV = BV.new ("sublists", listTy)
	    val others_V = BV.new ("others_", listTy)
	    val allV = BV.new ("all", listTy)
	    val dsV = BV.new ("ds", BTy.T_Tuple (false, copies (arity, anyTy)))
	    val dVs = mkVars ("d", arity, listTy) (* backwards *)
	    val data_V = BV.new ("data_", listTy)
	    val eV = BV.new ("e", ropeTy)
	    (* lams *)
	    val hash1Lam = (* FIXME This could be an HLOp instead of a local fn. *)
		let val tV = BV.new ("t", iPairTy (listTy, boolTy))
		    val resV = BV.new ("res", listTy)
		    val b = B.mkStmt ([resV], B.E_Select (0, tV), B.mkRet [resV])
		in
		    B.FB {f = hash1V,
			  params = [tV],
			  exh = [BV.new ("exh", exhTy)],
			  body = b}
		end
	    (* misc *)
	    fun mkSel ((dn, n), e) = B.mkStmt ([dn], B.E_Select (n, dsV), e)
	    (* and finally...*)
	    val leafPat = B.P_DCon (BB.ropeLeaf, [lenV, dataV])
	    val leafBody = 
               B.mkLet  ([getV], B.mkHLOp (HLOpEnv.curriedRopeSublistOp, [startV, lenV], [exhV]),
               B.mkStmt ([arg1V], B.E_Alloc (argTy, [getV, othersV]),
               B.mkFun  ([hash1Lam],
               B.mkLet  ([sublistsV], B.mkHLOp (HLOpEnv.listMapOp, [arg1V], [exhV]),
               B.mkStmt ([arg2V], B.E_Alloc (argTy, [hash1V, sublistsV]),
               B.mkLet  ([others_V], B.mkHLOp (HLOpEnv.listMapOp, [arg2V], [exhV]),
               B.mkLet  ([allV], B.mkHLOp (HLOpEnv.insertAtOp, [dataV, others_V, iV], [exhV]),
               B.mkLet  ([dsV], B.mkApply (l2tV, [allV], []),
               foldr'   (mkSel, numberFrom (rev dVs, 0),
               B.mkLet  ([data_V], B.mkApply (listMapFunV arity, fV :: (rev dVs), [exhV]),
               B.mkStmt ([eV], B.E_DCon (BB.ropeLeaf, [lenV, data_V]),
               B.mkRet [eV])))))))))))
	in	
	    (leafPat, leafBody)
	end 
     
  (* mkCatBody : B.var * B.var * B.var * B.var -> B.exp *)
    fun mkCatBody (shortV, startV, innerMapV, lenV, depthV, shortLV, shortRV, exhV) : B.exp =
	let (* types *)
	    val thunkTy = BTy.T_Fun ([unitTy], [exhTy], [ropeTy])
	    (* variables *)
	    val lenLV = BV.new ("lenL", rawIntTy)
	    val startRV = BV.new ("startR", rawIntTy)
	    val thunkV = BV.new ("thunk", thunkTy)
	    val shortR_FV = BV.new ("shortR_F", futureTy)
	    val shortL_V = BV.new ("shortL_", ropeTy)
	    val shortR_V = BV.new ("shortR_", ropeTy)
	    val cV = BV.new ("c", ropeTy)
	    (* lams *)
	    val thunkLam = B.FB {f = thunkV,
				 params = [BV.new ("_", unitTy)],
				 exh = [BV.new ("exh", exhTy)],
				 body = B.mkApply (innerMapV, [shortRV, startRV], [])}
	    (* misc *)
	    val fut1Spawn = HLOpEnv.future1SpawnOp
	    val fut1Touch = HLOpEnv.future1TouchOp
	    (* apply innerMapV (shortRV, startRV) *)
	    val retVal = B.E_DCon (BB.ropeCat, [lenV, depthV, shortL_V, shortR_V])
	in
	    B.mkLet ([lenLV], B.mkHLOp (HLOpEnv.ropeLengthIntOp, [shortLV], [exhV]),
            B.mkStmt ([startRV], B.E_Prim (Prim.I32Add (startV, lenLV)),
            B.mkFun ([thunkLam], 
            B.mkLet ([shortR_FV], B.mkHLOp (fut1Spawn, [thunkV], [exhV]),
            B.mkLet ([shortL_V], B.mkApply (innerMapV, [shortLV, startV], []),
            B.mkLet ([shortR_V], B.mkHLOp (fut1Touch, [shortR_FV], [exhV]),
            B.mkStmt ([cV], retVal,
            B.mkRet [cV])))))))
	end

  (* mkCatCase : B.var * B.var * B.var * B.var -> B.pat * B.exp *)
    fun mkCatCase (shortV : B.var, startV : B.var, innerMapV : B.var, exhV : B.var) 
                  : B.pat * B.exp =
	let val lenV = BV.new ("len", rawIntTy)
	    val depthV = BV.new ("d", rawIntTy)
	    val shortLV = BV.new ("shortL", ropeTy)
	    val shortRV = BV.new ("shortR", ropeTy)
	    val catPat = B.P_DCon (BB.ropeCat, [lenV, depthV, shortLV, shortRV])
	    val catBody = mkCatBody (shortV, startV, innerMapV, 
				     lenV, depthV, shortLV, shortRV, exhV)

	in
	    (catPat, catBody)
	end

  (* mkInnerMap : int * B.var * B.var * B.var * B.var * B.var -> B.lambda *)
    fun mkInnerMap (arity, fV, l2tV, indexV, othersV, exhV) : B.lambda =
	let val innerMapV = BV.new ("rmap" ^ Int.toString arity,  
			     BTy.T_Fun ([ropeTy, rawIntTy], [], [ropeTy]))
	    val shortV = BV.new ("shortRope", ropeTy)
	    val startV = BV.new ("start", rawIntTy)
	    val leafCase = mkLeafCase (arity, shortV, startV, fV, indexV, othersV, l2tV, exhV)
	    val catCase = mkCatCase (shortV, startV, innerMapV, exhV)
	    val body = B.mkCase (shortV, [leafCase, catCase], NONE)
	in
	    B.FB {f = innerMapV,
		  params = [shortV, startV],
		  exh = [],
		  body = body}
	end

  (* deconstructTuple : var list * var * exp -> exp *)
    fun deconstructTuple (vs, t, e) =
	let fun d ([], _) = e
	      | d (v::vs, n) = B.mkStmt([v], B.E_Select(n,t), d(vs,n+1))
	in
	    d (vs, 0)
	end

  (* mkMap : int -> B.lambda *)
    fun mkMap (arity : int) : B.lambda =
	let val exhV = BV.new ("exh", exhTy)
	    val msgDataV = BV.new ("msgData", anyTy)
	    val msgLenV = BV.new ("msgLen", rawIntTy)
	    val msgV = BV.new ("msg", BB.stringTy)
	    fun mkFail () = B.mkHLOp (HLOpEnv.failOp, [msgV], [exhV])
	    val l2t as B.FB {f=l2tV, ...} = mkListToTup (arity, mkFail)
	    val listMapN = listMapFun arity
            val fTy = BTy.T_Fun ([tupTy (false, copies (arity, anyTy))], [exhTy], [anyTy])
	    val fV = BV.new ("f", fTy)
	    val argTy = tupTy (false, fTy :: copies (arity, ropeTy))
	    val argV = BV.new ("arg", argTy)
	    val rmapTy = BTy.T_Fun ([argTy], [exhTy], [ropeTy])
	    val rmapV = BV.new ("rope_map_" ^ Int.toString arity, rmapTy)
	    val ropeListV = BV.new ("ropes", listTy)
	    val ropeVs = List.tabulate (arity,
				        fn n => BV.new ("rope" ^ Int.toString (n+1), ropeTy))
	    val esrV = BV.new ("esr", tupTy (false, [ropeTy, listTy, rawIntTy]))
	    val shortestV = BV.new ("s", ropeTy)
	    val othersV = BV.new ("others", listTy)
	    val indexV = BV.new ("i", rawIntTy)
	    val innerMap as B.FB {f=rmapn, ...} = mkInnerMap (arity, fV, l2tV, indexV, othersV, exhV)
	    val zeroV = BV.new ("zero", rawIntTy)
	    val body = deconstructTuple (fV :: ropeVs, argV,
                       B.mkStmt ([msgDataV], B.E_Const (Literal.String "Length", anyTy),
                       B.mkStmt ([msgLenV], BOMUtil.rawInt(6),
                       B.mkStmt ([msgV], B.E_Alloc (iPairTy (stringTy, rawIntTy), 
						    [msgDataV, msgLenV]),
                       B.mkFun ([l2t],
                       B.mkFun ([listMapN],
                       mkList (ropeListV, ropeVs,
		       B.mkLet ([esrV], B.mkHLOp (HLOpEnv.extractShortestRopeOp, [ropeListV], [exhV]),
                       B.mkStmt ([shortestV], B.E_Select (0, esrV),
                       B.mkStmt ([othersV], B.E_Select (1, esrV),
                       B.mkStmt ([indexV], B.E_Select (2, esrV),
                       B.mkFun ([innerMap],
                       B.mkStmt ([zeroV], B.E_Const (Literal.Int 0, rawIntTy),
                       B.mkApply (rmapn, [shortestV, zeroV], []))))))))))))))
	in
	    B.FB {f = rmapV,
		  params = [argV],
		  exh = [exhV],
		  body = body}
	end

    structure MapFnCache = CacheFn(struct 
				     type t = B.lambda
				     val mkItem = mkMap
				   end)

    val gen : int -> B.lambda = MapFnCache.getItem

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

	fun showMe (lam as B.FB {f, ...}) = PrintBOM.printExp (B.mkFun ([lam], B.mkRet [f]))
    in
    fun test 0 = PrintBOM.printExp (mkListToTupExp (2, mkRaise))
      | test 1 = PrintBOM.printExp (mkListToTupExp (3, mkRaise))
      | test 2 = PrintBOM.printExp (mkListToTupExp (6, mkRaise))
      | test 3 = 
	  let val x = BV.new ("x", listTy)
	      val vars = map (fn n => BV.new ("y" ^ Int.toString n, anyTy)) [1,2,3,4,5]
	  in
	      PrintBOM.printExp (mkList (x, vars, mkRaise ()))
	  end
      | test 4 = showMe (mkMap 2)
      | test 5 = showMe (mkMap 3)
      | test 6 = showMe (mkMap 6)
      | test _ = println "No such test."
    end (* local *)

  end (* struct *)
