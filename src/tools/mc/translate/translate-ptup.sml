(* translate-ptup.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Here we translate parallel tuples to scheduling primitives. Our translation uses the
 * fast / slow clone technique designed for the Cilk-5 language.
 * 
 * Our translation extends the Cilk-5 technique by adding lazy promotion. That is, we
 * defer any promotions to the slow clone.
 *
 * TODO: treat trivial computations specially, since there is no point in generating clones for them 
 *)

structure TranslatePTup  : sig

  (* precondition: length es > 1 *)
    val tr : {
	  supportsExceptions : bool,
	  env : TranslateEnv.env,
	  es : BOM.exp list
	} -> BOM.exp

  end  = struct

    structure B   = BOM
    structure BTy = BOMTy
    structure BU  = BOMUtil
    structure BTy = BOMTy
    structure BV = BOM.Var
    structure E = TranslateEnv
    structure Lit = Literal

    val findBOMTy = E.findBOMTyByPath
    val findHLOp = E.findBOMHLOpByPath

    fun varOfLambda (B.FB {f, ...}) = f

    val intTy = BTy.T_Raw RawTypes.T_Int
    fun intRhs x = B.E_Const (Literal.Int (Int.toLarge x), intTy)

    fun mkLets (binds : (BV.var list * BOM.exp) list, e') = List.foldl (fn ((x, e), e') => B.mkLet (x, e, e')) e' binds

    fun unitVar () = BV.new("_unit", BTy.unitTy)

    val nilVal = B.E_Const (Lit.unitLit, BTy.T_Any)

    (* create a tuple whose elements are initialized as nil values *)
    fun mkNilTuple (tuple : B.var, tys : BTy.ty list, e : B.exp) : B.exp =
	let 
	    val nils = List.map (fn ty_i => 
				    let
					val x = BV.new ("x", BTy.T_Any)
					val y = BV.new ("y", BTy.T_Any)
					val v = BV.new ("nil", ty_i)
				    in
					([v],
					 B.mkStmts ([
					     ([x], nilVal),
					     ([y], B.E_Cast (ty_i, x))], 
						    B.mkRet [y]))
				    end)
				tys
	in
	    mkLets (nils, B.mkStmt ([tuple], B.E_Alloc (BTy.T_Tuple (true, tys), List.map (List.hd o #1) nils), e))
	end

    (* use make the exception handler exh', such that
     *   cont exh' (exn) =
     *       body
     *     in
     *       throw exh (exn)
     *)
    fun freshExh (exh, body) =
	let
	    val exh' = BV.new ("exh", BTy.exhTy)
	    val exn = BV.new ("exn", BTy.exnTy)
	in	    
	    B.mkLambda {f = exh', params = [exn], exh = [], body = 
		      body (B.mkThrow (exh, [exn]))}
	end

    fun take1 [] = []
      | take1 (x :: xs) = xs

  (* implicit-thread creation *)
    fun mkThread (k, exh) = B.mkHLOp (findHLOp ["ImplicitThread", "new-thread"], [k], [exh])
  (* scheduling operations *)
    fun spawnThread (k, exh) = B.mkHLOp (findHLOp ["ImplicitThread", "spawn-thread"], [k], [exh])
    fun removeThread (thread, exh) = B.mkHLOp (findHLOp ["ImplicitThread", "remove-thread"], [thread], [exh])
    local
	    fun mkRaiseExn (env, exh) = 
		let
		    val matchExnDCon = 
			(case E.findDCon(env, Basis.exnMatch)
			  of SOME(E.ExnConst matchExnDCon) => matchExnDCon
			   | _ => raise Fail "compiler bug: missing match exception"
 			(* end case *))
		    val matchExn = BV.new("matchExn", BTy.T_TyCon(BOMTyCon.dconTyc matchExnDCon))
		in
		    B.mkStmt([matchExn], B.E_DCon(matchExnDCon, []),
			     B.mkThrow(exh, [matchExn]))
		end
    in
    fun mkStop (env, exh) = 
	  B.mkLet([unitVar()], 
		    B.mkHLOp(findHLOp["SchedulerAction", "stop"], [], []),
	    mkRaiseExn(env, exh))
    end

    (* version of the translation that does not support exceptions *)
    fun trWithoutExh (env, es as e_0 :: es_1toM) =
	let

	    val n = List.length es
	    val m = n - 1

	    val tys as ty_0 :: _ = List.map (List.hd o BOMUtil.typeOfExp) es
	    val tupleTy = BTy.T_Tuple (false, tys)
	    val slowCloneTupleTy = BTy.T_Tuple (true, tys)

	    val r0 = BV.new ("r0", slowCloneTupleTy)
	    val c0 = BV.new ("c0", BTy.T_Tuple (true, [intTy]))

	    val exh = E.handlerOf env

	    val thunks_1toM = 
		let
		    fun mkThunk e_i =
			let
			    val thunk_i = BV.new("thunk_i", BTy.T_Fun([], [BTy.exhTy], BOMUtil.typeOfExp e_i))
			    val (exh, _) = E.newHandler env
			in
			    B.mkLambda {f = thunk_i, params = [], exh = [exh], body = e_i}
			end
		in
		    List.map mkThunk es_1toM
		end

	    val thunkVs_1toM = List.map varOfLambda thunks_1toM

	    (* store the ith value in the result array *)
	    fun store (i, v_i, r, exh) =
		let
		    val ty_i = BV.typeOf v_i
		    val v_i' = BV.new ("v'_" ^ Int.toString i, ty_i)
		in
		    B.mkStmt ([v_i'], B.E_Promote v_i,
			       B.mkStmt ([], B.E_Update (i, r, v_i'), B.mkRet []))
		end

	    (* signal that w elements of the tuple have been evaluated *)
	    fun finish (w, r) =
		let
		    val nPrev = BV.new ("nPrev", intTy)
		    val eq = BV.new ("eq", BTy.boolTy)
		    val nV = BV.new ("n", intTy)
		    val wV = BV.new ("w", intTy)
		    val c' = BV.new ("c'", BTy.T_Tuple (true, [intTy]))
		    val c =  BV.new ("c", BTy.T_Addr intTy)
		in
		    B.mkStmts ([
		        ([c'], B.E_Promote c0),         (* lazy promotion *)
			([c], B.E_AddrOf (0, c')),
			([nV], intRhs (n - w + 1)),     (* nV is the number of other slow clones *)
			([wV], intRhs w),
		        ([nPrev], B.E_Prim (Prim.I32FetchAndAdd (c, wV)))],
			     B.mkStmt ([eq], B.E_Prim (Prim.I32Eq (nV, nPrev)),
				      B.mkIf (eq, 
					      (* the worker has the complete tuple *)
					      B.mkRet [r],
					      (* other workers are still working *)
					      mkStop (env, exh))))
		end
		    
	    (* we create m slow clones *)
	    val slowClones_1toM = 
		List.tabulate (m, fn i => BV.new ("slowCloneThread_" ^ Int.toString (i + 1), 
						      findBOMTy ["ImplicitThread", "thread"]))
	    fun mkSlowClones eNext = 
		let
		    (* evaluate the ith thunk and store the result in the result tuple *)
		    fun force (i, thunk_i, r, exh) = 
			let
			    val BTy.T_Fun (_, _, [ty_i]) = BV.typeOf thunk_i
			    val v_i = BV.new ("v_" ^ Int.toString i, ty_i)
			in
			    B.mkLet ([v_i], B.mkApply (thunk_i, [], [exh]), store (i, v_i, r, exh))
			end

		    fun mkSlowClone (0, [], []) = eNext
		      | mkSlowClone (i, thunk_i :: thunks, slowCloneThread :: slowClones_1toM) =
			let
			    val slowClone_i = BV.new ("slowClone_" ^ Int.toString i, BTy.T_Cont [BTy.unitTy])
			    val r = BV.new ("r", slowCloneTupleTy)
			in
			    B.mkCont (B.mkLambda {f = slowClone_i, params = [unitVar ()], exh = [], 
						  body = 
						  B.mkStmt ([r], B.E_Promote r0,            (* lazy promotion *)
							    B.mkLet ([], force (i, thunk_i, r, exh), finish (1, r)))},
				      B.mkLet ([slowCloneThread], mkThread (slowClone_i, exh), 
					       B.mkLet ([], spawnThread (slowCloneThread, exh),
							mkSlowClone (i - 1, thunks, slowClones_1toM))))
			end
		      | mkSlowClone _ = raise Fail "impossible"
		in
		    (* put slow clones on the queue in reverse order (right to left) *)
		    mkSlowClone (m, List.rev (List.map varOfLambda thunks_1toM), List.rev slowClones_1toM)
		end

	    (* the fast clone evaluates the elements of the tuple in sequential order until either all
	     * the elements have evaluated or all elements to the right have been stolen. *)
	    fun fastClone (_, [], [], vs) = 
		let
		    val tuple = BV.new ("tuple", tupleTy)
		in
		    B.mkStmt ([tuple], B.E_Alloc (tupleTy, List.rev vs), B.mkRet [tuple])
		end
	      | fastClone (i, thunk_i :: thunks, slowCloneThread_i :: slowClones_1toM, vs) =
		let
		    val BTy.T_Fun (_, _, [ty_i]) = BV.typeOf thunk_i
		    val v_i = BV.new ("v_" ^ Int.toString i, ty_i)
		    val notStolen_i = BV.new ("notStolen_" ^ Int.toString i, BTy.boolTy)
		    val r = BV.new ("r", slowCloneTupleTy)
		in
		    B.mkLet([notStolen_i], removeThread (slowCloneThread_i, exh),			    
			    B.mkIf (notStolen_i,
				    B.mkLet ([v_i], B.mkApply (thunk_i, [], [exh]),
					     fastClone (i + 1, thunks, slowClones_1toM, v_i :: vs)),
				    (* switch to the slow clone *)
				    B.mkStmt ([r], B.E_Promote r0,         (* lazy promotion *)
					      ListPair.foldlEq (fn (i, v_i, e) => B.mkLet ([], store (i, v_i, r, exh), e)) 
							       (finish (i, r))
							       (List.tabulate (List.length vs, fn i => i), List.rev vs))))
		end
	      | fastClone _ = raise Fail "impossible"

	    val one = intRhs 1
	    val oneV = BV.new ("one", intTy)

	in
	    mkNilTuple (r0, tys,
			  B.mkStmts ([
			  ([oneV], one),
			  ([c0], B.E_Alloc (BTy.T_Tuple (true, [intTy]), [oneV]))
			  ],
				     B.mkFun (thunks_1toM, 
					      mkSlowClones (
					      let
						  val v_0 = BV.new ("v_0", ty_0)
					      in
						  B.mkLet ([v_0], e_0,
							   fastClone (1, thunkVs_1toM, slowClones_1toM, [v_0]))
					      end))))
	end

    (* version of the translation that supports exceptions *)
    fun trWithExh (env, es as e_0 :: es_1toM) =
	let

	    val n = List.length es
	    val m = n - 1

	    val tys as ty_0 :: tys_1toM = List.map (List.hd o BOMUtil.typeOfExp) es
	    val tupleTy = BTy.T_Tuple (false, tys)
	    val slowCloneTupleTy = BTy.T_Tuple (true, tys)

	    val r0 = BV.new ("r0", slowCloneTupleTy)
	    val c0 = BV.new ("c0", BTy.T_Tuple (true, [intTy]))

	    val exh = E.handlerOf env

	    val cancelables_1toM = List.map (fn _ => BV.new ("ccbl", findBOMTy ["Cancelation", "cancelable"])) es_1toM

	    fun mkCancelAll (exh, cs, e) =
		mkLets (List.map (fn c => ([], 
		      B.mkHLOp(findHLOp["Cancelation", "cancel"], [c], [exh]))) cs, e)

	    val thunks_1toM = 
		let
		    fun mkThunks ([], _) = []
		      | mkThunks (e_i :: es_1toM, cancelablesToTheRight) =
			let
			    val thunk_i = BV.new ("thunk_i", BTy.T_Fun([], [BTy.exhTy], BOMUtil.typeOfExp e_i))
			    val (exh', _) = E.newHandler env
			    val exh'' = freshExh (exh', fn e => mkCancelAll (exh', cancelablesToTheRight, e))
			    val e_i' = BOMUtil.substExp (BOMUtil.singleton (exh, varOfLambda exh''), e_i)
			in
			    B.mkLambda {f = thunk_i, params = [], exh = [exh'], body = B.mkCont (exh'', e_i')} ::
			    mkThunks (es_1toM, take1 cancelablesToTheRight)
			end
		in
		    mkThunks (es_1toM, take1 cancelables_1toM)
		end

	    val thunkVs_1toM = List.map varOfLambda thunks_1toM

	    fun mkWaitOns (_, []) = []
	      | mkWaitOns (i, ty_i :: ty_1toM) =
		let
		    val r = BV.new ("r", slowCloneTupleTy)
		    val t = BV.new ("t", BTy.T_Any)
		    val isNil = BV.new ("isNil", BTy.boolTy)
		    val nilV = BV.new ("nilV", BTy.T_Any)
		    val waitOn_i = BV.new ("waitOn" ^  Int.toString i, BTy.T_Fun ([], [], []))
		in
		  B.mkLambda {f = waitOn_i, params = [], exh = [], body = 
                      B.mkStmts ([([r], B.E_Promote r0),
				  ([t], B.E_Select (i, r)),
				  ([nilV], nilVal),
				  ([isNil], B.E_Prim (Prim.Equal (t, nilV)))],
				 B.mkIf (isNil,
					 B.mkApply (waitOn_i, [], []),
					 B.mkRet []))} ::
		  mkWaitOns (i + 1, ty_1toM)
		end

	    val _ :: waitOns_Mm1to0 = List.rev (mkWaitOns (0, tys))

	    (* store the ith value in the result array *)
	    fun store (i, v_i, r, exh) =
		let
		    val ty_i = BV.typeOf v_i
		    val v_i' = BV.new ("v'_" ^ Int.toString i, ty_i)
		in
		    B.mkStmt ([v_i'], B.E_Promote v_i,
			       B.mkStmt ([], B.E_Update (i, r, v_i'), B.mkRet []))
		end

	    (* signal that w elements of the tuple have been evaluated *)
	    fun finish (w, r) =
		let
		    val nPrev = BV.new ("nPrev", intTy)
		    val eq = BV.new ("eq", BTy.boolTy)
		    val nV = BV.new ("n", intTy)
		    val wV = BV.new ("w", intTy)
		    val c' = BV.new ("c'", BTy.T_Tuple (true, [intTy]))
		    val c =  BV.new ("c", BTy.T_Addr intTy)
		in
		    B.mkStmts ([
		        ([c'], B.E_Promote c0),         (* lazy promotion *)
			([c], B.E_AddrOf (0, c')),
			([nV], intRhs (n - w + 1)),     (* nV is the number of other slow clones *)
			([wV], intRhs w),
		        ([nPrev], B.E_Prim (Prim.I32FetchAndAdd (c, wV)))],
			     B.mkStmt ([eq], B.E_Prim (Prim.I32Eq (nV, nPrev)),
				      B.mkIf (eq, 
					      (* the worker has the complete tuple *)
					      B.mkRet [r],
					      (* other workers are still working *)
					      mkStop (env, exh))))
		end
		    
	    (* we create m = M slow clones *)
	    val slowClones_1toM = 
		List.tabulate (m, fn i => BV.new ("slowCloneThread_" ^ Int.toString (i + 1), 
						      findBOMTy ["ImplicitThread", "thread"]))
	    fun mkSlowClones eNext = 
		let
		    (* evaluate the ith thunk and store the result in the result tuple *)
		    fun force (i, thunk_i, r, exh) = 
			let
			    val BTy.T_Fun (_, _, [ty_i]) = BV.typeOf thunk_i
			    val v_i = BV.new ("v_" ^ Int.toString i, ty_i)
			in
			    B.mkLet ([v_i], B.mkApply (thunk_i, [], [exh]), store (i, v_i, r, exh))
			end

		    fun mkSlowClone (0, [], [], []) = eNext
		      | mkSlowClone (i, thunk_i :: thunks, slowCloneThread :: slowCloneThreads, waitOns) =
			let
			    val slowClone_i = BV.new ("slowClone_" ^ Int.toString i, BTy.T_Cont [BTy.unitTy])
			    val r = BV.new ("r", slowCloneTupleTy)
			    val exhL' = freshExh (exh, fn e => 
					    mkLets (List.map (fn waitOn_i => ([], B.mkApply (varOfLambda waitOn_i, [], []))) waitOns, e))
			in
			    B.mkCont (B.mkLambda {f = slowClone_i, params = [unitVar ()], exh = [], body = 
				    B.mkCont (exhL', 
					  B.mkStmt ([r], B.E_Promote r0,            (* lazy promotion *)
						    B.mkLet ([], force (i, thunk_i, r, varOfLambda exhL'), finish (1, r))))},
				      B.mkLet ([slowCloneThread], mkThread (slowClone_i, exh), 
					       B.mkLet ([], spawnThread (slowCloneThread, exh),
							mkSlowClone (i - 1, thunks, slowCloneThreads, take1 waitOns))))
			end
		      | mkSlowClone _ = raise Fail "impossible"
		in
		    (* put slow clones on the queue in reverse order (right to left) *)
		    mkSlowClone (m, List.rev (List.map varOfLambda thunks_1toM), List.rev slowClones_1toM, waitOns_Mm1to0)
		end

	    (* the fast clone evaluates the elements of the tuple in sequential order until either all
	     * the elements have evaluated or all elements to the right have been stolen. *)
	    fun fastClone (_, [], [], vs) = 
		let
		    val tuple = BV.new ("tuple", tupleTy)
		in
		    B.mkStmt ([tuple], B.E_Alloc (tupleTy, List.rev vs), B.mkRet [tuple])
		end
	      | fastClone (i, thunk_i :: thunks, slowCloneThread_i :: slowClones_1toM, vs) =
		let
		    val BTy.T_Fun (_, _, [ty_i]) = BV.typeOf thunk_i
		    val v_i = BV.new ("v_" ^ Int.toString i, ty_i)
		    val notStolen_i = BV.new ("notStolen_" ^ Int.toString i, BTy.boolTy)
		    val r = BV.new ("r", slowCloneTupleTy)
		in
		    B.mkLet([notStolen_i], removeThread (slowCloneThread_i, exh),			    
			    B.mkIf (notStolen_i,
				    B.mkLet ([v_i], B.mkApply (thunk_i, [], [exh]),
					     fastClone (i + 1, thunks, slowClones_1toM, v_i :: vs)),
				    (* switch to the slow clone *)
				    B.mkStmt ([r], B.E_Promote r0,         (* lazy promotion *)
					      ListPair.foldlEq (fn (i, v_i, e) => B.mkLet ([], store (i, v_i, r, exh), e)) 
							       (finish (i, r))
							       (List.tabulate (List.length vs, fn i => i), List.rev vs))))
		end
	      | fastClone _ = raise Fail "impossible"

	    val one = intRhs 1
	    val oneV = BV.new ("one", intTy)

	in
	    mkNilTuple (r0, tys,
			  B.mkStmts ([
			  ([oneV], one),
			  ([c0], B.E_Alloc (BTy.T_Tuple (true, [intTy]), [oneV]))
			  ],
		mkLets (List.map (fn c => ([c], B.mkHLOp (findHLOp ["Cancelation", "new"], [], [exh]))) cancelables_1toM,
			B.mkFun (waitOns_Mm1to0 @ thunks_1toM, 
				 mkSlowClones (
				 let
				     val v_0 = BV.new ("v_0", ty_0)
				     val exh' = freshExh (exh, fn e => mkCancelAll (exh, cancelables_1toM, e))
				     val e_0' = BOMUtil.substExp (BOMUtil.singleton (exh, varOfLambda exh'), e_0)
				 in
				     B.mkLet ([v_0], B.mkCont (exh', e_0'), fastClone (1, thunkVs_1toM, slowClones_1toM, [v_0]))
				 end)))))
	end

    (* log : real -> (real -> real) *)
    fun log base x = Math.ln x / Math.ln base

    (* ceilingLog : (int * int) -> int *)
    fun ceilingLog (b, x) = ceil (log (Real.fromInt b) (real x))

    (* returns the digits of x written as a base-k integer. digits are ordered from least to most significant *)
    (* e.g., baseKDigits (2, 4) ==> [0, 0, 1] *)
    (* precondition: x is nonnegative *)
    fun baseKDigits (k, x) =
	if x < 0 then
	    raise Fail "only nonnegative integers are supported"
	else if x = 0 then
	    [0]
	else 
	    let
		fun lp 0 = []
		  | lp x = x mod k :: lp (x div k)
	    in
		lp x
	    end

    (* find the selection path from the root to the ith element of a k-nested n-tuple *)
    fun path (k, n, i) = 
	let 
	    val ds = baseKDigits (k, i)
	    val p = ceilingLog (k, n) - List.length ds
	in
	    List.rev (if p < 1 then
			  ds
		      else
			  ds @ List.tabulate (p, fn _ => 0))
	end

    (* constructs the expression #ix0(#ix1(...(#ixn-1(tup)))) and hands the result to f *)
    fun nestedSelect (tup : BV.var, ixs : int list, f : BV.var -> B.exp) : B.exp =
	let
	    fun mk (prev, _, []) = f prev
	      | mk (prev, BTy.T_Tuple (_, tys), d :: ds) =
		let
		    val ty = List.nth (tys, d)
		    val v = BV.new ("v", ty)
		in
		    B.mkStmt ([v], B.E_Select (d, prev),
			      mk (v, ty, ds))
		end
	in
	    mk (tup, BV.typeOf tup, ixs)
	end

    (* flatten the k-nested ptuple tup of length n *)
    fun flatten (tup : BV.var, k, n, flatTy) : B.exp =
	let 
	    fun lp (i, vs) =
		if i = n then
		    let 
			val flatTup = BV.new ("flattenedTup", flatTy)
		    in
			B.mkStmt ([flatTup], B.E_Alloc (flatTy, List.rev vs),
				 B.mkRet [flatTup])
		    end
		else
		    nestedSelect (tup, path (k, n, i), fn v =>
							  lp (i + 1, v :: vs))
	in
	    lp (0, [])
	end

    (* partition the list xs of length n into length-k sublists annotated with sublist length *)
    (* e.g., partition (2, 3, [1,2,3]) ==> [(2, [1,2]), (1, [3])] *)
    fun partition (k, n, xs) = 
	if n <= k then
	    [(n, xs)]
	else
	    (n - k, List.take (xs, k)) :: partition (k, n - k, List.drop (xs, k))

    val k = 4          (* maximum number of elements in a flat ptuple *)

    fun tr {supportsExceptions, env, es} = 
	if List.length es <= 1 then
	    raise Fail "compiler bug in TranslatePTup: the size of the input ptuple must be > 1"
	else
	    let
		val tr' = if supportsExceptions then
			      trWithExh
			  else 
			      trWithoutExh
		val n = List.length es
	    in
		if n <= k then
		    tr' (env, es)
		else
		    let
			(* decompose a parallel tuple into nested parallel tuples of size k *)
			(* e.g., consider decomp of a parallel tuple where n = 3 and k = 2
			 *   (| e1,e2,e3 |)  ==>  (| (| e1,e2 |),e3 |)
			 *)
			fun decomp (n, es) =
			    if n <= k then
				tr' (env, es)
			    else 
				tr' (env, List.map decomp (partition (k, n, es)))
			val nestedEs = decomp (length es, es)
			val nestedEsV = BV.new ("nestedEsV", List.hd (BOMUtil.typeOfExp nestedEs))
			val flatTy = BTy.T_Tuple (false, List.map (List.hd o BOMUtil.typeOfExp) es)
		    in
			B.mkLet ([nestedEsV], nestedEs, flatten (nestedEsV, k, n, flatTy))
		    end
	    end

  end
