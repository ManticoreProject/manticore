(* equality.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Synthesize equality operations based on type.
 *)

structure Equality : sig

    val mkEqual : (TranslateEnv.env * AST.var * AST.ty) -> BOM.lambda

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy
    structure P = Prim

    fun mk (isEq, a, b, ty) = let
	  val res = BV.new("res", BTy.boolTy)
	  fun mkPrim (eqP, neqP) = let
		val p = if isEq then eqP else neqP
		in
		  B.mkStmt([res], B.E_Prim(p(a, b)), B.mkRet[res])
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
		val res' = BV.new("res'", BTy.boolTy)
		in
		  B.mkStmts([
		      ([a'], B.E_Select(i, a)),
		      ([b'], B.E_Select(i, b))
		    ], B.mkLet([res'], mk(isEq, a', b', ty),
		      B.mkIf(res',
			(* then *) mkTuple (i+1, tys),
			(* else *) B.mkRet[res'])))
		end
	  in
	    case ty
	     of BTy.T_Any => mkPrim (P.Equal, P.NotEqual)
	      | BTy.T_Enum _ => mkPrim (P.Equal, P.NotEqual)
	      | BTy.T_Raw rty => (case rty
		   of BTy.T_Byte => raise Fail "Byte equality"
		    | BTy.T_Short => raise Fail "Short equality"
		    | BTy.T_Int => mkPrim (P.I32Eq, P.I32NEq)
		    | BTy.T_Long => mkPrim (P.I64Eq, P.I64NEq)
		    | BTy.T_Float => mkPrim (P.F32Eq, P.F32NEq)
		    | BTy.T_Double => mkPrim (P.F64Eq, P.F64NEq)
		    | BTy.T_Vec128 => raise Fail "Vec128 equality"
		  (* end case *))
	      | BTy.T_Tuple(false, tys) => mkTuple (0, tys)
	      | _ => raise Fail(BOMTyUtil.toString ty ^ " not an equality type")
	    (* end case *)
	  end

    fun mkEqual (env, eq, ty) = let
	  val isEq = Var.same(Basis.eq, eq)
	  val ty' = TranslateTypes.tr (env, ty)
	  val arg1 = BV.new("a", ty')
	  val arg2 = BV.new("b", ty')
	  val argTy = BTy.T_Tuple(false, [ty', ty'])
	  val arg = BV.new("_arg", argTy)
	  val f = if isEq then "eq" else "neq"
	  in
	    BOM.FB{
		f = BV.new(f, BTy.T_Fun([argTy], [BTy.exhTy], [BTy.boolTy])),
		params = [arg],
		exh = [BV.new("_exh", BTy.exhTy)],
		body = B.mkStmts([
		    ([arg1], B.E_Select(0, arg)),
		    ([arg2], B.E_Select(1, arg))
		  ], mk (isEq, arg1, arg2, ty'))
	      }
	  end

  end

