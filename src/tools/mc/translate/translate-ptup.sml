structure TranslatePTup  : sig

  (* An AST to BOM translation of parrays to ropes. *)
    val tr : {
	  env : TranslateEnv.env,
	  trVar : (TranslateEnv.env * AST.var) -> (BOM.Var.var * TranslateEnv.env),
	  trExp : TranslateEnv.env * AST.exp * (BOM.Var.var -> BOM.exp) -> BOM.exp,
	  es : BOM.exp list
	} -> BOM.exp

  end  = struct

    fun varOfLambda (fn B.FB {f, ...}) = f

    val unitV = B.E_Const(Lit.unitLit, BTy.unitTy)

    fun unitVar () = BV.new("_unit", BTy.unitTy)

    fun someCon () = raise Fail "todo"
    fun optionTy () = raise Fail "todo"

    fun mkStop (env, exh) = 
	  B.mkLet([unitVar()], 
		    B.mkHLOp(findHLOp["SchedulerAction", "stop"], [], []),
	    mkRaiseExn(env, exh))

    fun tr {env, trVar, trExp, es} =
	let
	    val n' = List.length es

	    val tys = List.map BOMUtil.typeOfExp es
	    val tupleTy = BTy.T_Tuple (false, tys)
	    val idxs = List.tabulate (n', fn i => i - 1)

	    val r = BV.new ("r", BTy.T_Tuple (true, List.map (fn ty => BTy.T_Tuple (false, ty)) tys))

	    fun mkThunk e_i =
		let
		    val bodyFn = BV.new("bodyFn", BTy.T_Fun([], [BTy.exhTy], [BOMUtil.typeOfExp e_i]))
		    val (bodyExh, _) = E.newHandler env
		in
		    B.mkLambda {f = bodyFn, params = [], exh = [bodyExh], body = trExp (env, e_i, fn v => B.mkRet [v])}
		end

	    val thunks = List.map mkThunk es

	    (* construct the result tuple from r *)
	    fun mkTuple exh = 
		let
		    fun mk (i, vs) =
			if i = n' then
			    let
				val tuple = BV.new ("tuple", tupleTy)
			    in
				B.mkLet ([tuple], B.E_Alloc (tupleTy, List.rev vs), B.mkRet [tuple])
			    end
			else
			    B.mkStmt ([v_i], B.E_Select (i, r),
				      mk (i + 1, v_i :: vs))
		in
		    mk (0, [])
		end

	    (* store the ith value in the result array *)
	    fun store (i, v_i, exh) =
		B.mkLet ([v_i'], someCon (),
			 B.mkStmt ([], B.E_Update (i, r, v_i), B.mkRet []))

	    (* evaluate the ith thunk *)
	    fun force (i, thunk_i, exh) = 
		let
		    val BTy.T_Fun (_, _, [ty)) = BV.typeOf thunk_i
		    val v_i = BV.new ("v_i", ty)
		    val v_i' = BV.new ("v_i'", optionTy ())
		in
		    B.mkLet ([v_i], B.mkApply (thunk_i, unitV, [exh]), store (i, v_i, exh))
		end

	    (* signal that w elements of the tuple have been evaluated *)
	    fun finish w =
		let
		    val nCurr = BV.new ("nCurr", BTy.intTy)
		    val eq = BV.new ("eq", BTy.boolTy)
		    val r' = BV.new ("r'", tupleTy)
		in
		    B.mkLet ([nCurr], B.E_Prim (Prim.I32FetchAndAdd (c, w)),
			     B.mkLet ([eq], B.E_Prim (Prim.I32Eq (n, nCurr)),
				      B.mkIf (eq, 
					      (* the worker has the complete tuple *)
					      B.mkLet ([r'], mkTuple (r, n', exh), B.mkThrow (k, [r'])),
					      (* other workers are still working *)
					      mkStop(env, exh))))
		end

	    fun slowClone (i, thunk_i) =
		let
		    val slowClone = BV.new ("slowClone", BTy.T_Cont [BTy.unitTy])
		in
		    B.mkLambda {f = slowClone, params = [unitVar ()], exh = [], 
				body = B.mkLet ([], force (i, thunk_i, r, exh), finish 1)}
		end
		    
	    val slowClones = ListPair.map slowClone (idxs, List.map varOfLambda thunks)

	    fun pushSlowClone (slowClone, e) = B.mkLet ([], pushTl (slowClone, exh), e)

	    val mkWorkAvailable = List.foldl pushSlowClone (B.mkRet []) (List.map varOfLambda slowClones)
				  
	    fun fastClone (_, [], [], vs) = 
		let
		    val tuple = BV.new ("tuple", tupleTy)
		in
		    B.mkLet ([tuple], B.E_Alloc (tupleTy, List.rev vs), B.mkRet [tuple])
		end
	      | fastClone (i, thunk_i :: thunks, ty_i :: tys, vs) =
		let
		    val v_i = BV.new ("v_i", ty)
		in
		    B.mkLet([b], popTl exh,
			    B.mkIf (b,
				    B.mkLet ([v_i], B.mkApply (thunk_i, unitV, [exh]),
					     fastClone (i + 1, thunks, v_i :: vs, tys))
				    (* switch to the slow clone *)
				    ListPair.foldl (fn (i, v_i, e) => B.mkLet ([], store (i, v_i, exh), e)) (mkStop (env, exh))
				        (List.tabulate (i, fn i => i), List.rev (v_i :: vs))))
		end

	    val zero = B.E_Const (Literal.Int 0, intTy)
	    val intTy = BTy.T_Raw RawTypes.T_Int
	in
	    B.mkStmts ([
	        ([z], zero),
		([c0], B.E_Alloc (BTy.T_Tuple (true, intTy), z)),
		([c], B.E_Promote c0)
	    ],
	    raise Fail "todo")
				
	end

  end
