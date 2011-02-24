(* ft-translate-ptup.sml
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
 *)

structure FTTranslatePTup  : sig

  (* precondition: length es > 1 *)
    val tr : {
	  supportsExceptions : bool,
	  env : FTTranslateEnv.env,
	  es : BOM.exp list
	} -> BOM.exp

  end  = struct

    structure B   = BOM
    structure BTy = BOMTy
    structure BU  = BOMUtil
    structure BTy = BOMTy
    structure BV = BOM.Var
    structure E = FTTranslateEnv
    structure Lit = Literal

    (* returns true if the given expression might involve some non-trivial computation, e.g., a computation
     * that is worth running in parallel. *)
    (*
     TODO: avoid generating slow clones for trivial expressions
    fun isNontrivialExp (B.E_Pt (_, e)) = isNontrivialTerm e

    and isNontrivialTerm (B.E_Let (_, e, e')) = isNontrivialExp e orelse isNontrivialExp e'
      | isNontrivialTerm (B.E_Stmt (_, rhs, e)) = isNontrivialRHS rhs orelse isNontrivialExp e
      | isNontrivialTerm (B.E_Fun (_, e)) = isNontrivialExp e
      | isNontrivialTerm (B.E_Cont (_, e)) = isNontrivialExp e
      | isNontrivialTerm (B.E_If (_, e1, e2)) = isNontrivialExp e1 orelse isNontrivialExp e2
      | isNontrivialTerm (B.E_Case (_, pexps, NONE)) = List.exists (isNontrivialExp o #2) pexps
      | isNontrivialTerm (B.E_Case (_, pexps, SOME e)) = List.exists (isNontrivialExp o #2) pexps orelse
							 isNontrivialExp e
      | isNontrivialTerm (B.E_Apply _) = true
      | isNontrivialTerm (B.E_Throw _) = true
      | isNontrivialTerm (B.E_Ret _) = false
      | isNontrivialTerm (B.E_HLOp _) = true

    and isNontrivialRHS (B.E_CCall _) = true
      | isNontrivialRHS _ = false
     *)

    fun varOfLambda (B.FB {f, ...}) = f

    val intTy = BTy.T_Raw RawTypes.T_Int
    fun intRhs x = B.E_Const (Literal.Int (Int.toLarge x), intTy)

    fun mkLets (binds : (BV.var list * BOM.exp) list, e') = 
	List.foldl (fn ((x, e), e') => B.mkLet (x, e, e')) e' binds

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
    fun mkThread (k, exh) = B.mkHLOp (E.findBOMHLOpByPath ["ImplicitThread", "new-thread"], [k], [exh])
  (* scheduling operations *)
    fun spawnThread (k, exh) = B.mkHLOp (E.findBOMHLOpByPath ["ImplicitThread", "spawn-thread"], [k], [exh])
    fun removeThread (thread, exh) = 
	B.mkHLOp (E.findBOMHLOpByPath ["ImplicitThread", "remove-thread-b"], [thread], [exh])
    local
	    fun mkRaiseExn (env, exh) = 
		let
		    val matchExnDCon = 
			(case E.findDCon(env, raise Fail "FIXME" (*Basis.exnMatch*))
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
		    B.mkHLOp(E.findBOMHLOpByPath["SchedulerAction", "stop"], [], []),
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
			       B.mkIf (Prim.I32Eq (nV, nPrev),
				       (* the worker has the complete tuple *)
				       B.mkRet [r],
				       (* other workers are still working *)
				       mkStop (env, exh)))
		end
		    
	    (* we create m slow clones *)
	    val slowClones_1toM = 
		List.tabulate (m, fn i => BV.new ("slowCloneThread_" ^ Int.toString (i + 1), 
						      E.findBOMTyByPath ["ImplicitThread", "thread"]))
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
		    val notStolen_i = BV.new ("notStolen_" ^ Int.toString i, BOMTyUtil.boolTy)
		    val r = BV.new ("r", slowCloneTupleTy)
		in
		    B.mkLet([notStolen_i], removeThread (slowCloneThread_i, exh),			    
			    BOMUtil.mkBoolCase (notStolen_i,
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

	    val cancelables_1toM = List.map (fn _ => BV.new ("ccbl", E.findBOMTyByPath ["Cancelation", "cancelable"])) es_1toM

	    fun mkCancelAll (exh, cs, e) =
		mkLets (List.map (fn c => ([], 
		      B.mkHLOp(E.findBOMHLOpByPath["Cancelation", "cancel"], [c], [exh]))) cs, e)

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
		    val nilV = BV.new ("nilV", BTy.T_Any)
		    val waitOn_i = BV.new ("waitOn" ^  Int.toString i, BTy.T_Fun ([], [], []))
		in
		  B.mkLambda {f = waitOn_i, params = [], exh = [], body = 
                      B.mkStmts ([([r], B.E_Promote r0),
				  ([t], B.E_Select (i, r)),
				  ([nilV], nilVal)],
				 B.mkIf (Prim.Equal (t, nilV),
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
			       B.mkIf (Prim.I32Eq (nV, nPrev), 
				       (* the worker has the complete tuple *)
				       B.mkRet [r],
				       (* other workers are still working *)
				       mkStop (env, exh)))
		end
		    
	    (* we create m = M slow clones *)
	    val slowClones_1toM = 
		List.tabulate (m, fn i => BV.new ("slowCloneThread_" ^ Int.toString (i + 1), 
						      E.findBOMTyByPath ["ImplicitThread", "thread"]))
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
		    val notStolen_i = BV.new ("notStolen_" ^ Int.toString i, BOMTyUtil.boolTy)
		    val r = BV.new ("r", slowCloneTupleTy)
		in
		    B.mkLet([notStolen_i], removeThread (slowCloneThread_i, exh),			    
			    BOMUtil.mkBoolCase (notStolen_i,
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
		mkLets (List.map (fn c => ([c], B.mkHLOp (E.findBOMHLOpByPath ["Cancelation", "new"], [], [exh]))) cancelables_1toM,
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

    fun ceilingDiv (x, y) = ceil (real x / real y)

    (* constructs the expression #ix0(#ix1(...(#ixn-1(tup)))) and hands the result to f *)
    fun nestedSelect (tup : BV.var, ixs : int list, f : BV.var -> B.exp) : B.exp =
	let
	    fun mk (prev, _, []) = f prev
	      | mk (prev, ty as BTy.T_Tuple (_, tys), d :: ds) =
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

    (* returns a list where the ith element is the path to that element in the nested tuple *)
    fun enumerate (k, n) : int list list =
	let
	    fun doit (1, p) = [List.rev p]
	      | doit (n : int, p : int list) =
		let
		    val x = ceilingDiv (n, k)
		    fun partition (i, n) =
			if n > 0 then
			    (Int.min (n, x), i :: p) :: partition (i + 1, n - x)
			else
			    []
		in
		    List.concat (List.map doit (partition (0, n)))
		end
	in
	    doit (n, [])
	end

    (* takes a list of expressions corresponding to the flat, input parallel tuple and builds the nested
     * clone translation where each nested tuple has a size between 2 and k. the nesting structure of this
     * function must match the nesting structure of the enumerate function above, as enumerate is picking
     * apart the nested tuple created here.
     *)
    fun transNestedPTup (tr : B.exp list -> B.exp, k, n, es : B.exp list) =
	let
	    fun doit (1, [e]) = e
	      | doit (n : int, es : B.exp list) =
		let
		    val x = ceilingDiv (n, k)
		    fun partition (i, n, es) =
			if n > 0 then
			    (Int.min (n, x), List.take (es, Int.min (n, x))) :: 
			    partition (i + 1, n - x, List.drop (es, Int.min (n, x)))
			else
			    []
		in
		    tr (List.map doit (partition (0, n, es)))
		end
	in
	    doit (n, es)
	end

    (* flatten the k-nested ptuple tup of length n *)
    fun flatten (tup : BV.var, k, n, flatTy) : B.exp =
	let 
	    fun lp ([], vs) = 
		let 
		    val flatTup = BV.new ("flattenedTup", flatTy)
		in
		    B.mkStmt ([flatTup], B.E_Alloc (flatTy, List.rev vs),
			      B.mkRet [flatTup])
		end
	      | lp (path :: paths, vs) =
		nestedSelect (tup, path, fn v =>
				    lp (paths, v :: vs))
	in
	    lp (enumerate (k, n), [])
	end

    val k = 4          (* maximum number of elements in a flat ptuple *)

    fun tr {supportsExceptions, env, es} = 
	if List.length es <= 1 then
	    raise Fail "compiler bug in TranslatePTup: the size of the input ptuple must be > 1"
	else
	    let
		val n = List.length es
		fun tr' es = if supportsExceptions then
				 trWithExh (env, es)
			     else 
				 trWithoutExh (env, es)
	    in
		if n <= k then
		    tr' es
		else
		    let
			val nestedTuple = transNestedPTup (tr', k, n, es)
			val nestedTupleV = BV.new ("nestedTupleV", List.hd (BOMUtil.typeOfExp nestedTuple))
			val flatTy = BTy.T_Tuple (false, List.map (List.hd o BOMUtil.typeOfExp) es)
		    in
			B.mkLet ([nestedTupleV], nestedTuple, flatten (nestedTupleV, k, n, flatTy))
		    end
	    end

  end
