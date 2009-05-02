structure TranslatePTup  : sig

  (* An AST to BOM translation of parrays to ropes. *)
    val tr : {
	  env : TranslateEnv.env,
	  trVar : (TranslateEnv.env * AST.var) -> (BOM.Var.var * TranslateEnv.env),
	  trExp : TranslateEnv.env * AST.exp * (BOM.Var.var -> BOM.exp) -> BOM.exp,
	  x : AST.var,
	  es : AST.exp list
	} -> BOM.exp

  end  = struct

    fun arrayUpdate () = raise Fail "todo"

    val unitV = B.E_Const(Lit.unitLit, BTy.unitTy)

    fun someCon () = raise Fail "todo"
    fun optionTy () = raise Fail "todo"

    fun mkStop (env, exh) = 
	  B.mkLet([unitVar()], 
		    B.mkHLOp(findHLOp["SchedulerAction", "stop"], [], []),
	    mkRaiseExn(env, exh))

    fun mkTuple (r, n', exh) = raise Fail "todo"

    fun force (i, f_i, r, exh) = 
	let
	    val BTy.T_Fun (_, _, [ty)) = BV.typeOf f_i
	    val v = BV.new ("v", ty)
	    val v' = BV.new ("v'", optionTy ())
	in
	    B.mkLet ([v], B.mkApply (f_i, unitV, [exh]),
		     B.mkLet ([v'], someCon (),
			      B.mkLet ([], B.mkApply (arrayUpdate (), [r, v'], [exh])
					   B.mkRet [])))
	end

    fun finish (c, w, n, r, k, n', tupleTy, exh) =
	let
	    val nCurr = BV.new ("nCurr", BTy.intTy)
	    val eq = BV.new ("eq", BTy.boolTy)
	    val r' = BV.new ("r'", tupleTy)
	in
	    B.mkLet ([nCurr], B.E_Prim (Prim.I32FetchAndAdd (c, w)),
		     B.mkLet ([eq], B.E_Prim (Prim.I32Eq (n, nCurr)),
			      B.mkIf (eq, 
				      B.mkLet ([r'], mkTuple (r, n', exh),
					       B.mkThrow (k, [r'])),
				      mkStop(env, exh))))
	end

  end
