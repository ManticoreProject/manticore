structure TranslatePTup  : sig

  (* An AST to BOM translation of parrays to ropes. *)
    val tr : {
	  env : TranslateEnv.env,
	  trVar : (TranslateEnv.env * AST.var) -> (BOM.Var.var * TranslateEnv.env),
	  trExp : TranslateEnv.env * AST.exp * (BOM.Var.var -> BOM.exp) -> BOM.exp,
	  es : BOM.exp list
	} -> BOM.exp

  end  = struct

    structure B   = BOM
    structure BTy = BOMTy
    structure BU  = BOMUtil
    structure BTy = BOMTy
    structure BV = BOM.Var
    structure E = TranslateEnv

    val findBOMTy = E.findBOMTyByPath
    val findHLOp = E.findBOMHLOpByPath

    fun varOfLambda (fn B.FB {f, ...}) = f

    val unitV = B.E_Const(Lit.unitLit, BTy.unitTy)
    val zero = B.E_Const (Literal.Int 0, intTy)
    val intTy = BTy.T_Raw RawTypes.T_Int

    fun unitVar () = BV.new("_unit", BTy.unitTy)

    fun someCon () = raise Fail "todo"
    fun optionTy () = raise Fail "todo"

  (* implicit-thread creation *)
    fun mkThread (exh, k) =
	  B.mkHLOp(findHLOp["ImplicitThread", "thread-no-cancelable"], [k], [exh])
  (* scheduling operations *)
    fun pushTl (exh, kLocal) =
	  B.mkHLOp(findHLOp["MultiprogrammedWorkStealing", "push-tl"], [kLocal], [exh])
    fun popTl exh =
	  B.mkHLOp(findHLOp["MultiprogrammedWorkStealing", "pop-tl"], [], [exh])
    fun mkStop (env, exh) = 
	  B.mkLet([unitVar()], 
		    B.mkHLOp(findHLOp["SchedulerAction", "stop"], [], []),
	    mkRaiseExn(env, exh))

    fun tr {env, trVar, trExp, es = []} = 
	let
	    val x = BV.new ("x", BTy.unitTy)
	in
	    B.mkLet ([x], B.E_Const(Lit.unitLit, BTy.unitTy), B.mkRet [x])
	end
      | tr {env, trVar, trExp, es} =
	let
	    val n' = List.length es
	    val nLit = B.E_Const (Literal.Int n', intTy)

	    val tys as ty_0 :: tys_1toN = List.map BOMUtil.typeOfExp es
	    val tupleTy = BTy.T_Tuple (false, tys)
	    val slowCloneTupleTy = BTy.T_Tuple (true, tys)

	    val r = BV.new ("r", slowCloneTupleTy)

	    val thunks as thunk_0 :: thunks1toN = 
		let
		    fun mkThunk e_i =
			let
			    val bodyFn = BV.new("bodyFn", BTy.T_Fun([], [BTy.exhTy], [BOMUtil.typeOfExp e_i]))
			    val (bodyExh, _) = E.newHandler env
			in
			    B.mkLambda {f = bodyFn, params = [], exh = [bodyExh], body = trExp (env, e_i, fn v => B.mkRet [v])}
			end
		in
		    List.map mkThunk es
		end

	    (* store the ith value in the result array *)
	    fun store (i, v_i, exh) =
		B.mkStmts ([([r], B.E_Promote r0),
			    ([v_i'], someCon ())],
			   B.mkStmt ([], B.E_Update (i, r, v_i), B.mkRet []))

	    (* signal that w elements of the tuple have been evaluated *)
	    fun finish w =
		let
		    val nCurr = BV.new ("nCurr", intTy)
		    val eq = BV.new ("eq", BTy.boolTy)
		    val r' = BV.new ("r'", tupleTy)
		    val nV = BV.new ("n", intTy)
		in
		    B.mkStmts ([
		        ([c], B.E_Promote c0),
			([nV], nLit),
		        ([nCurr], B.E_Prim (Prim.I32FetchAndAdd (c, w)))],
			     B.mkLet ([eq], B.E_Prim (Prim.I32Eq (nV, nCurr)),
				      B.mkIf (eq, 
					      (* the worker has the complete tuple *)
					      B.mkStmt ([r'], B.E_Promote r, B.mkThrow (k, [r'])),
					      (* other workers are still working *)
					      mkStop (env, exh))))
		end
		    
	    val slowClones = 
		let
		    (* evaluate the ith thunk and store the result in the result tuple *)
		    fun force (i, thunk_i, exh) = 
			let
			    val BTy.T_Fun (_, _, [ty)) = BV.typeOf thunk_i
			    val v_i = BV.new ("v_i", ty)
			    val v_i' = BV.new ("v_i'", optionTy ())
			in
			    B.mkLet ([v_i], B.mkApply (thunk_i, unitV, [exh]), store (i, v_i, exh))
			end

		    fun slowClone (i, thunk_i) =
			let
			    val slowClone = BV.new ("slowClone", BTy.T_Cont [BTy.unitTy])
			in
			    B.mkLambda {f = slowClone, params = [unitVar ()], exh = [], 
					body = B.mkLet ([], force (i, thunk_i, r, exh), finish 1)}
			end
		in
		    ListPair.mapEq slowClone (List.tabulate (n', fn i => i - 1), List.map varOfLambda thunks1toN)
		end

	    (* we enqueue the slow clones in order from left to right *)
	    val enqueueWork = 
		let
		    fun pushSlowClone (slowClone, e) = 
			let
			    val slowCloneThread = BV.new ("slowCloneThread", 
							  findBOMTy ["ImplicitThread", "thread"])
			in
			    B.mkLet ([], B.mkLet ([slowCloneThread], mkThread (slowClone, exh),
						  pushTl (slowCloneThread, exh), e))
			end
		in
		    List.foldl pushSlowClone (B.mkRet []) (List.map varOfLambda slowClones)
		end

	    (* the fast clone evaluates the elements of the tuple in sequential order until either all
	     * the elements have evaluated or all elements to the right have been stolen. *)
	    fun fastClone (_, [], [], vs) = 
		let
		    val tuple = BV.new ("tuple", tupleTy)
		in
		    B.mkLet ([tuple], B.E_Alloc (tupleTy, List.rev vs), B.mkRet [tuple])
		end
	      | fastClone (i, thunk_i :: thunks, ty_i :: tys, vs) =
		let
		    val v_i = BV.new ("v_i", ty)
		    val stealOccurred = BV.new ("stealOccurred", BTy.boolTy)
		in
		    B.mkLet([stealOccurred], popTl exh,
			    B.mkIf (stealOccurred,
				    B.mkLet ([v_i], B.mkApply (thunk_i, unitV, [exh]),
					     fastClone (i + 1, thunks, v_i :: vs, tys))
				    (* switch to the slow clone *)
				    ListPair.foldlEq (fn (i, v_i, e) => B.mkLet ([], store (i, v_i, exh), e)) 
				        (finish i)
				        (List.tabulate (i, fn i => i), List.rev (v_i :: vs))))
		end
	in
	    B.mkStmts ([
	        ([z], zero),
		([c0], B.E_Alloc (BTy.T_Tuple (true, intTy), z)),
		([r0], B.E_Alloc (slowCloneTupleTy, List.tabulate (n, fn _ => B.E_Const(Lit.unitLit, BTy.unitTy))))
	    ],
                B.mkFun (slowClones,
			 B.mkLet ([], enqueueWork,
				  let
				      val v_0 = BV.new ("v_0", ty_0)
				  in
				      B.mkLet ([v_0], B.mkApply (thunk_0, unitV, [exh]),
					       fastClone (0, thunks1toN, tys1toN, [v_0]))
				  end)))
	end

  end
