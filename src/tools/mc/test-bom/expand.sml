(* expand.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module converts the more compact parse-tree representation of
 BOM code into the internal representation of BOM terms.
 *)
 
 structure Expand =
  struct

    structure PT = BOMPT
    structure P = Prim
    structure Ty = BOMTy
    structure BV = BOM.Var

    datatype prim_info
      = Prim1 of {
	    mk : BOM.var -> BOM.var P.prim,
	    argTy : Ty.ty,
	    resTy : Ty.ty
	  }
      | Prim2 of {
	    mk : BOM.var * BOM.var -> BOM.var P.prim,
	    argTy : Ty.ty * Ty.ty,
	    resTy : Ty.ty
	  }
	  
	  
  (* table mapping primop names to prim_info *)
    val findPrim = let
	  val tbl = AtomTable.mkTable(128, Fail "prim table")
	  val ins = AtomTable.insert tbl
	  val aTy = Ty.T_Any
	  val bTy = Ty.boolTy
	  val i32 = Ty.T_Raw Ty.T_Int
	  val i64 = Ty.T_Raw Ty.T_Long
	  val f32 = Ty.T_Raw Ty.T_Float
	  val f64 = Ty.T_Raw Ty.T_Double
	  fun mk cons (mk, argTy, resTy) = cons {mk=mk, argTy=argTy, resTy  = resTy}
	  in
	    List.app (fn (n, info) => ins(Atom.atom n, info)) [
		("isBoxed",	mk Prim1 (P.isBoxed,	aTy,		bTy)),
		("isUnboxed",	mk Prim1 (P.isUnboxed,	aTy,		bTy)),
		("Equal",	mk Prim2 (P.Equal,	(aTy, aTy),	bTy)),
		("NotEqual",	mk Prim2 (P.NotEqual,	(aTy, aTy),	bTy)),
		("BNot",	mk Prim1 (P.BNot,	bTy,		bTy)),
		("BEq",		mk Prim2 (P.BEq,	(bTy, bTy),	bTy)),
		("BNEq",	mk Prim2 (P.BNEq,	(bTy, bTy),	bTy)),
		("I32Add",	mk Prim2 (P.I32Add,	(i32, i32),	i32)),
		("I32Sub",	mk Prim2 (P.I32Sub,	(i32, i32),	i32)),
		("I32Mul",	mk Prim2 (P.I32Mul,	(i32, i32),	i32)),
		("I32Div",	mk Prim2 (P.I32Div,	(i32, i32),	i32)),
		("I32Mod",	mk Prim2 (P.I32Mod,	(i32, i32),	i32)),
		("I32Neg",	mk Prim1 (P.I32Neg,	i32,		i32)),
		("I32Eq",	mk Prim2 (P.I32Eq,	(i32, i32),	bTy)),
		("I32NEq",	mk Prim2 (P.I32NEq,	(i32, i32),	bTy)),
		("I32Lt",	mk Prim2 (P.I32Lt,	(i32, i32),	bTy)),
		("I32Lte",	mk Prim2 (P.I32Lte,	(i32, i32),	bTy)),
		("I32Gt",	mk Prim2 (P.I32Gt,	(i32, i32),	bTy)),
		("I32Gte",	mk Prim2 (P.I32Gte,	(i32, i32),	bTy)),
		("I64Add",	mk Prim2 (P.I64Add,	(i64, i64),	i64)),
		("I64Sub",	mk Prim2 (P.I64Sub,	(i64, i64),	i64)),
		("I64Mul",	mk Prim2 (P.I64Mul,	(i64, i64),	i64)),
		("I64Div",	mk Prim2 (P.I64Div,	(i64, i64),	i64)),
		("I64Mod",	mk Prim2 (P.I64Mod,	(i64, i64),	i64)),
		("I64Neg",	mk Prim1 (P.I64Neg,	i64,		i64)),
		("I64Eq",	mk Prim2 (P.I64Eq,	(i64, i64),	bTy)),
		("I64NEq",	mk Prim2 (P.I64NEq,	(i64, i64),	bTy)),
		("I64Lt",	mk Prim2 (P.I64Lt,	(i64, i64),	bTy)),
		("I64Lte",	mk Prim2 (P.I64Lte,	(i64, i64),	bTy)),
		("I64Gt",	mk Prim2 (P.I64Gt,	(i64, i64),	bTy)),
		("I64Gte",	mk Prim2 (P.I64Gte,	(i64, i64),	bTy)),
		("F32Add",	mk Prim2 (P.F32Add,	(f32, f32),	f32)),
		("F32Sub",	mk Prim2 (P.F32Sub,	(f32, f32),	f32)),
		("F32Mul",	mk Prim2 (P.F32Mul,	(f32, f32),	f32)),
		("F32Div",	mk Prim2 (P.F32Div,	(f32, f32),	f32)),
		("F32Neg",	mk Prim1 (P.F32Neg,	f32,		f32)),
		("F32Eq",	mk Prim2 (P.F32Eq,	(f32, f32),	bTy)),
		("F32NEq",	mk Prim2 (P.F32NEq,	(f32, f32),	bTy)),
		("F32Lt",	mk Prim2 (P.F32Lt,	(f32, f32),	bTy)),
		("F32Lte",	mk Prim2 (P.F32Lte,	(f32, f32),	bTy)),
		("F32Gt",	mk Prim2 (P.F32Gt,	(f32, f32),	bTy)),
		("F32Gte",	mk Prim2 (P.F32Gte,	(f32, f32),	bTy)),
		("F64Add",	mk Prim2 (P.F64Add,	(f64, f64),	f64)),
		("F64Sub",	mk Prim2 (P.F64Sub,	(f64, f64),	f64)),
		("F64Mul",	mk Prim2 (P.F64Mul,	(f64, f64),	f64)),
		("F64Div",	mk Prim2 (P.F64Div,	(f64, f64),	f64)),
		("F64Neg",	mk Prim1 (P.F64Neg,	f64,		f64)),
		("F64Eq",	mk Prim2 (P.F64Eq,	(f64, f64),	bTy)),
		("F64NEq",	mk Prim2 (P.F64NEq,	(f64, f64),	bTy)),
		("F64Lt",	mk Prim2 (P.F64Lt,	(f64, f64),	bTy)),
		("F64Lte",	mk Prim2 (P.F64Lte,	(f64, f64),	bTy)),
		("F64Gt",	mk Prim2 (P.F64Gt,	(f64, f64),	bTy)),
		("F64Gte",	mk Prim2 (P.F64Gte,	(f64, f64),	bTy))
	      ];
	    AtomTable.find tbl
	  end
	  
	  
  (* some type utilities *)
    fun unwrapType (Ty.T_Wrap rTy) = Ty.T_Raw rTy
      | unwrapType ty = raise Fail(concat["unwrapType(", Ty.toString ty, ")"])

    fun selectType (i, Ty.T_Tuple tys) = List.nth(tys, i)
      | selectType _ = raise Fail "selectType"

    fun lookup (env, x) = (case AtomMap.find(env, x)
	   of NONE => raise Fail("unbound variable " ^ Atom.toString x)
	    | SOME x' => x'
	  (* end case *))

    fun newTmp ty = BOM.Var.new("_t", ty)
    
    fun cvtVarBinds (env, vars) = let
	  fun f ((x, ty), (env, xs)) = let
		val x' = BOM.Var.new(Atom.toString x, ty)
		in
		  (AtomMap.insert(env, x, x'), x'::xs)
		end
	  in
	    List.foldl f (env, []) vars
	  end
 
    fun cvtPat pat = (case pat
           of PT.DConPat(ty, x) => let
                val dataCon = BOMTy.DCon{
                  name = Atom.toString ty,
                  stamp = Stamp.new (),
                  rep = BOMTy.Transparent,
                  argTy = []}
                in
                  BOM.P_DCon(dataCon, [])
                end 
            | PT.ConstPat (const, optTy) => let
                val ty = (case optTy
                  of NONE => Ty.T_Any
                   | SOME rTy => Ty.T_Raw rTy
                  (* end case *))
                in 
                  case const
                    of Literal.Int x => BOM.P_Const (BOM.E_IConst(x, ty))
                     | Literal.String x => BOM.P_Const (BOM.E_SConst x)
                     | Literal.Float x => BOM.P_Const (BOM.E_FConst(x, ty))
                   (* end case *)
                 end
            (* end case *))

    fun cvtExp (env, e) = (case e
	   of PT.Let(lhs, rhs, e) => let
		val (env', lhs') = cvtVarBinds (env, lhs)
		val lhs' = List.rev lhs'
		val e' = cvtExp(env', e)
		in
		  case rhs
		   of PT.SimpleExp e => (case e
			 of PT.Var x => BOM.mkLet(lhs', BOM.mkRet[lookup (env, x)], e')
			  | PT.Select(i, arg) =>
			      cvtSimpleExp (env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Select(i, x), e'))
			  | PT.Cast(ty, arg) =>
			      cvtSimpleExp (env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Cast(ty, x), e'))
	                  | PT.Literal (Literal.Int x, optTy) => let
		              val ty = (case optTy
			        of NONE => Ty.T_Any
			         | SOME rTy => Ty.T_Raw rTy
			        (* end case *))
		              in
		                BOM.mkStmt(lhs', BOM.E_Const (BOM.E_IConst (x, ty)), e')
		              end
	                  | PT.Literal (Literal.String x, _) => BOM.mkStmt(lhs', BOM.E_Const(BOM.E_SConst x), e')
	                  | PT.Literal (Literal.Float x, optTy) => let
		              val ty = (case optTy
			      of NONE => Ty.T_Any
			       | SOME rTy => Ty.T_Raw rTy
			      (* end case *))
		            in
		              BOM.mkStmt(lhs', BOM.E_Const (BOM.E_FConst (x, ty)), e')
		            end
			  | PT.Unwrap arg =>
			      cvtSimpleExp (env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Unwrap x, e'))
			  | PT.Prim(p, args) =>
			      cvtSimpleExps (env, args, fn xs => let
				val rhs = (case (findPrim p, xs)
				       of (NONE, _) => raise Fail("unknown primop " ^ Atom.toString p)
					| (SOME(Prim1{mk, ...}), [x]) => mk x
					| (SOME(Prim2{mk, ...}), [x, y]) => mk(x, y)
					| _ => raise Fail("arity mismatch for primop " ^ Atom.toString p)
				      (* end case *))
				in
				  BOM.mkStmt(lhs', BOM.E_Prim rhs, e')
				end)
			(* end case *))
		    | PT.Alloc args =>
			cvtSimpleExps (env, args,
			  fn xs => BOM.mkStmt(lhs', BOM.E_Alloc(Ty.T_Tuple(List.map BV.typeOf xs), xs), e'))
		    | PT.Wrap arg =>
			cvtSimpleExp (env, arg, fn x => BOM.mkStmt(lhs', BOM.E_Wrap x, e'))
		    | PT.CCall(f, args) =>
			cvtSimpleExps (env, args, fn xs =>
			  BOM.mkStmt(lhs', BOM.E_CCall(lookup(env, f), xs), e'))
		  (* end case *)
		end
	    | PT.Fun(fbs, e) => let
		fun f (fb, (env', cvtBodies)) = let
			val (env'', cvt) = cvtLambda (env', fb, Ty.T_Fun)
			in
			  (env'', cvt::cvtBodies)
			end
		val (envWFBs, cvtBodies) = List.foldl f (env, []) fbs
		in		 
		  BOM.mkFun(
		    List.foldl (fn (cvt, fbs) => cvt envWFBs :: fbs) [] cvtBodies,
		    cvtExp (envWFBs, e))
		end
	    | PT.Cont(fb, e) => let
	      (* NOTE: continuations are permitted to be recursive *)
		val (env', cvtBody) = cvtLambda(env, fb, fn (argTys, _, _) => Ty.T_Cont argTys)
		in
		  BOM.mkCont(cvtBody env', cvtExp(env', e))
		end
	    | PT.If(e1, e2, e3) =>
		cvtSimpleExp (env, e1, fn x => BOM.mkIf(x, cvtExp(env, e2), cvtExp(env, e3)))
	    | PT.Case(arg, e1, e2) => raise Fail "unimplemented" (* FIXME *)
	    | PT.Case(arg, cases, dflt) => 
                cvtSimpleExp (env, arg, fn arg =>
                    BOM.mkCase(
		      arg, 
                      List.map (fn (pat,e) => (cvtPat pat, cvtExp(env,e))) cases,
                      case dflt of NONE => NONE | SOME (_, e) => SOME (cvtExp(env, e))))
	    | PT.Apply(f, args, rets) =>
		cvtSimpleExps (env, args,
		  fn xs => cvtSimpleExps (env, rets,
		    fn ys => BOM.mkApply(lookup(env, f), xs, ys)))
	    | PT.Throw(k, args) =>
		cvtSimpleExps (env, args, fn xs => BOM.mkThrow(lookup(env, k), xs))
	  (* end case *))

    and cvtLambda (env, (f, params, rets, tys, e), tyCon) = let
	  val fnTy = tyCon(List.map #2 params, List.map #2 rets, tys)
	  val f' = BOM.Var.new(Atom.toString f, fnTy)
	  fun doBody env = let
		val (envWParams, params') = cvtVarBinds (env, params)
		val (envWParams, rets') = cvtVarBinds (envWParams, rets)
		in
		  BOM.FB{
		      f = f', params = List.rev params',
		      exh = List.rev rets', body = cvtExp (envWParams, e)
		    }
		end
	  in
	    (AtomMap.insert(env, f, f'), doBody)
	  end

    and cvtSimpleExp (env, e, k : BOM.var -> BOM.exp) = (case e
	   of PT.Var x => k(lookup(env, x))
	    | PT.Select(i, e) =>
		cvtSimpleExp (env, e, fn x => let
		  val tmp = newTmp(selectType(i, BOM.Var.typeOf x))
		  in
		    BOM.mkStmt([tmp], BOM.E_Select(i, x), k tmp)
		  end)
	    | PT.Literal (Literal.Int x, optTy) => let
		  val ty = (case optTy
			 of NONE => Ty.T_Any
			  | SOME rTy => Ty.T_Raw rTy
			(* end case *))
		  val tmp = newTmp ty
		  in
		    BOM.mkStmt([tmp], BOM.E_Const(BOM.E_IConst(x, ty)), k tmp)
		  end
	    | PT.Literal (Literal.Float x, optTy) => let
		  val ty = (case optTy
			 of NONE => Ty.T_Any
			  | SOME rTy => Ty.T_Raw rTy
			(* end case *))
		  val tmp = newTmp ty
		  in
		    BOM.mkStmt([tmp], BOM.E_Const(BOM.E_FConst(x, ty)), k tmp)
		  end
	    | PT.Literal (Literal.String x, _) => let
		  val tmp = newTmp Ty.T_Any
		  in
		    BOM.mkStmt([tmp], BOM.E_Const(BOM.E_SConst x), k tmp)
		  end

	    | PT.Cast(ty, e) =>
		cvtSimpleExp (env, e, fn x => let
		  val tmp = newTmp ty
		  in
		    BOM.mkStmt([tmp], BOM.E_Cast(ty, x), k tmp)
		  end)
	    | PT.Unwrap e =>
		cvtSimpleExp (env, e, fn x => let
		  val tmp = newTmp(unwrapType(BOM.Var.typeOf x))
		  in
		    BOM.mkStmt([tmp], BOM.E_Unwrap x, k tmp)
		  end)
	    | PT.Prim(p, args) =>
		cvtSimpleExps (env, args, fn xs => let
		  val (lhs, rhs) = (case (findPrim p, xs)
			 of (NONE, _) => raise Fail("unknown primop " ^ Atom.toString p)
			  | (SOME(Prim1{mk, resTy, ...}), [x]) => (newTmp resTy, mk x)
			  | (SOME(Prim2{mk, resTy, ...}), [x, y]) => (newTmp resTy, mk(x, y))
			  | _ => raise Fail("arity mismatch for primop " ^ Atom.toString p)
			(* end case *))
		  in
		    BOM.mkStmt([lhs], BOM.E_Prim rhs, k lhs)
		  end)
	  (* end case *))

    and cvtSimpleExps (env, exps, k) = let
	  fun cvt ([], tmps) = k(List.rev tmps)
	    | cvt (e::es, tmps) = cvtSimpleExp (env, e, fn t => cvt(es, t::tmps))
	  in
	    cvt (exps, [])
	  end

   
    fun cvtModule (PT.MODULE{name, externs, body}) = let
	  fun doCFun (CFunctions.CFun{var, name, retTy, argTys, attrs}, (cfs, env)) = let
		val f = BOM.Var.new(Atom.toString var, Ty.T_CFun(CFunctions.CProto(retTy, argTys, attrs)))
		in (
		  BOM.mkCFun{var=f, name=name, retTy=retTy, argTys=argTys, attrs=attrs}::cfs,
		  AtomMap.insert(env, var, f)
		) end
	  val (cfs, env) = List.foldl doCFun ([], AtomMap.empty) externs
	  val (_, cvtBody) = cvtLambda (AtomMap.empty, body, Ty.T_Fun)
	  in
	    BOM.MODULE{name=name, externs=cfs, body=cvtBody env}
	  end

  end
