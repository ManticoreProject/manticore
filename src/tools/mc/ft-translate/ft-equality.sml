(* ft-equality.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Synthesize equality operations based on type.
 *)

structure FTEquality : sig

    val mkEqual : (FTTranslateEnv.env * AST.var * AST.ty) -> BOM.lambda

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy
    structure P = Prim
    structure BTU = BOMTyUtil

    fun mk (isEq, a, b, ty) = let
	  fun mkTest (eqP, neqP) = let
		val cond = if isEq then eqP else neqP
		val t = BV.new("t", BTU.boolTy)
		val f = BV.new("f", BTU.boolTy)
		in
		  B.mkIf(cond(a, b),
		    B.mkStmt([t], B.E_DCon(BTU.trueDC, []), B.mkRet[t]),
		    B.mkStmt([f], B.E_DCon(BTU.falseDC, []), B.mkRet[f]))
		end
	  fun mkTuple (i, [ty]) = let
		val a' = BV.new("a"^Int.toString i, ty)
		val b' = BV.new("b"^Int.toString i, ty)
		in
		  B.mkStmts([
		      ([a'], B.E_Select(i, a)),
		      ([b'], B.E_Select(i, b))
		    ], mk(isEq, a', b', ty))
		end
	    | mkTuple (i, ty::tys) = let
		val a' = BV.new("a"^Int.toString i, ty)
		val b' = BV.new("b"^Int.toString i, ty)
		val res' = BV.new("res'", BTU.boolTy)
		in
		  B.mkStmts([
		      ([a'], B.E_Select(i, a)),
		      ([b'], B.E_Select(i, b))
		    ],
		    B.mkLet([res'], mk(isEq, a', b', ty),
		      BOMUtil.mkBoolCase(res', mkTuple (i+1, tys), B.mkRet[res'])))
		end
	  in
	    case ty
	     of BTy.T_Any => mkTest (P.Equal, P.NotEqual)
	      | BTy.T_Enum _ => mkTest (P.Equal, P.NotEqual)
	      | BTy.T_Raw rty => (case rty
		   of BTy.T_Byte => raise Fail "Raw T_Byte equality"
		    | BTy.T_Short => raise Fail "Raw T_Short equality"
		    | BTy.T_Int => mkTest (P.I32Eq, P.I32NEq)
		    | BTy.T_Long => mkTest (P.I64Eq, P.I64NEq)
		    | BTy.T_Float => mkTest (P.F32Eq, P.F32NEq)
		    | BTy.T_Double => mkTest (P.F64Eq, P.F64NEq)
		    | BTy.T_Vec128 => raise Fail "Raw T_Vec128 equality"
		  (* end case *))
	      | BTy.T_Tuple(false, tys) => mkTuple (0, tys)
	      | _ => raise Fail(BOMTyUtil.toString ty ^ " not an equality type")
	    (* end case *)
	  end

    fun mkEqual (env, eq, ty) = let
	  val isEq = Var.same(Basis.eq, eq)
	  val ty' = FTTranslateTypes.tr (env, ty)
	  val arg1 = BV.new("a", ty')
	  val arg2 = BV.new("b", ty')
	  val argTy = BTy.T_Tuple(false, [ty', ty'])
	  val arg = BV.new("_arg", argTy)
	  val f = if isEq then "eq" else "neq"
	  in
	    BOM.FB{
		f = BV.new(f, BTy.T_Fun([argTy], [BTy.exhTy], [BTU.boolTy])),
		params = [arg],
		exh = [BV.new("_exh", BTy.exhTy)],
		body = B.mkStmts([
		    ([arg1], B.E_Select(0, arg)),
		    ([arg2], B.E_Select(1, arg))
		  ], mk (isEq, arg1, arg2, ty'))
	      }
	  end

  end

