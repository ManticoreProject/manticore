(* expand.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module converts the parse-tree representation of a HLOp definition file
 * into a list of HLOp definitions.
 *)
 
 structure Expand : sig

  (* an environment to keep track of any imports required by the high-level operator *)
    type import_env = BOM.var CFunctions.c_fun AtomTable.hash_table

    val cvtFile : (import_env * HLOpDefPT.file) -> (BOM.hlop * bool * BOM.lambda) list

  end = struct

    structure PT = HLOpDefPT
    structure P = Prim
    structure Ty = BOMTy
    structure BV = BOM.Var
    structure Env = HLOpEnv
    structure Basis = BOMBasis
    structure ATbl = AtomTable

  (* an environment to keep track of any imports required by the high-level operator *)
    type import_env = BOM.var CFunctions.c_fun AtomTable.hash_table

  (* table mapping primop names to prim_info *)
    structure MkPrim = MakePrimFn (
	type var = BOM.var
	type ty = Ty.ty
	val anyTy = Ty.T_Any
	val boolTy = Ty.boolTy
	val addrTy = Ty.T_Addr(Ty.T_Any)
	val rawTy = Ty.T_Raw)

    datatype prim_info = datatype MkPrim.prim_info
    val findPrim = MkPrim.findPrim

  (* some type utilities *)
    fun unwrapType (Ty.T_Wrap rTy) = Ty.T_Raw rTy
      | unwrapType ty = raise Fail(concat["unwrapType(", Ty.toString ty, ")"])

    fun selectType (i, Ty.T_Tuple(_, tys)) = List.nth(tys, i)
      | selectType _ = raise Fail "selectType"

    fun lookup (env, x) = (case AtomMap.find(env, x)
	   of NONE => raise Fail("unbound variable " ^ Atom.toString x)
	    | SOME x' => x'
	  (* end case *))

    fun newTmp ty = BOM.Var.new("_t", ty)

  (* convert a parse-tree type express to a BOM type *)
    fun cvtTy PT.T_Any = Ty.T_Any
      | cvtTy (PT.T_Enum w) = Ty.T_Enum w
      | cvtTy (PT.T_Raw rty) = Ty.T_Raw rty
      | cvtTy (PT.T_Wrap rty) = Ty.T_Wrap rty
      | cvtTy (PT.T_Tuple(mut, tys)) = Ty.T_Tuple(mut, List.map cvtTy tys)
      | cvtTy (PT.T_Addr ty) = Ty.T_Addr(cvtTy ty)
      | cvtTy (PT.T_Fun(argTys, exhTys, resTys)) =
	  Ty.T_Fun(List.map cvtTy argTys, List.map cvtTy exhTys, List.map cvtTy resTys)
      | cvtTy (PT.T_Cont tys) = Ty.T_Cont(List.map cvtTy tys)
      | cvtTy (PT.T_CFun cproto) = Ty.T_CFun cproto
      | cvtTy (PT.T_VProc) = Ty.T_VProc
      | cvtTy (PT.T_TyCon tyc) = (case Basis.findTyc tyc
	   of SOME tyc => Ty.T_TyCon tyc
	    | NONE => raise Fail(concat["unknown type constructor ", Atom.toString tyc])
	  (* end case *))

    fun cvtVarBinds (env, vars) = let
	  fun f ((x, ty), (env, xs)) = let
		val ty = cvtTy ty
		val x' = BOM.Var.new(Atom.toString x, ty)
		in
		  (AtomMap.insert(env, x, x'), x'::xs)
		end
	  in
	    List.foldl f (env, []) vars
	  end
 
    fun cvtPat (env, PT.DConPat(dc, xs)) = (case Basis.findDCon dc
	   of SOME dc => let
		fun f (PT.WildPat, (env, xs)) = let
		      val x' = BOM.Var.new("_wild", Ty.T_Any)
		      in
			(env, x'::xs)
		      end
		  | f (PT.VarPat(x, ty), (env, xs)) = let
		      val ty = cvtTy ty
		      val x' = BOM.Var.new(Atom.toString x, ty)
		      in
			(AtomMap.insert(env, x, x'), x'::xs)
		      end
		val (env, xs) = List.foldl f (env, []) xs
		in
		  (env, BOM.P_DCon(dc, List.rev xs))
		end
	    | NONE => raise Fail(concat["unknown data constructor ", Atom.toString dc])
	  (* end case *))
      | cvtPat (env, PT.ConstPat(const, ty)) = (env, BOM.P_Const(const, cvtTy ty))

    fun cvtExp (findCFun, env, e) = (case e
	   of PT.Let(lhs, rhs, e) => let
		val (env', lhs') = cvtVarBinds (env, lhs)
		val lhs' = List.rev lhs'
		val e' = cvtExp(findCFun, env', e)
		in
		  case rhs
		   of PT.Exp e => BOM.mkLet(lhs', cvtExp(findCFun, env, e), e')
		    | PT.SimpleExp e => (case e
			 of PT.Var x => BOM.mkLet(lhs', BOM.mkRet[lookup(env, x)], e')
			  | PT.Select(i, arg) =>
			      cvtSimpleExp(findCFun, env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Select(i, x), e'))
			  | PT.AddrOf(i, arg) =>
			      cvtSimpleExp(findCFun, env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_AddrOf(i, x), e'))
			  | PT.Cast(ty, arg) =>
			      cvtSimpleExp(findCFun, env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Cast(cvtTy ty, x), e'))
	                  | PT.Const(lit, ty) => BOM.mkStmt(lhs', BOM.E_Const(lit, cvtTy ty), e')
			  | PT.Unwrap arg =>
			      cvtSimpleExp(findCFun, env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Unwrap x, e'))
			  | PT.Prim(p, args) =>
			      cvtSimpleExps(findCFun, env, args, fn xs => let
				val rhs = (case (findPrim p, xs)
				       of (NONE, _) => raise Fail("unknown primop " ^ Atom.toString p)
					| (SOME(Prim1{mk, ...}), [x]) => mk x
					| (SOME(Prim2{mk, ...}), [x, y]) => mk(x, y)
					| (SOME(Prim3{mk, ...}), [x, y, z]) => mk(x, y, z)
					| _ => raise Fail("arity mismatch for primop " ^ Atom.toString p)
				      (* end case *))
				in
				  BOM.mkStmt(lhs', BOM.E_Prim rhs, e')
				end)
			  | PT.HostVProc => BOM.mkStmt(lhs', BOM.E_HostVProc, e')
			  | PT.VPLoad(offset, vp) =>
			      cvtSimpleExp(findCFun, env, vp, fn vp =>
				BOM.mkStmt(lhs', BOM.E_VPLoad(offset, vp), e'))
			  | PT.VPStore(offset, vp, arg) =>
			      cvtSimpleExp(findCFun, env, vp, fn vp =>
				cvtSimpleExp(findCFun, env, arg, fn x =>
				  BOM.mkStmt(lhs', BOM.E_VPStore(offset, vp, x), e')))
			(* end case *))
		    | PT.Update(i, arg, rhs) =>
			cvtSimpleExp(findCFun, env, arg, fn x =>
			  cvtSimpleExp(findCFun, env, rhs, fn y =>
			    BOM.mkStmt(lhs', BOM.E_Update(i, x, y), e')))
		    | PT.Alloc args => let
			val mut = (case BV.typeOf(hd lhs')
			       of Ty.T_Tuple(true, _) => true
				| _ => false
			      (* end case *))
			in
			  cvtSimpleExps(findCFun, env, args,
			    fn xs => BOM.mkStmt(lhs', BOM.E_Alloc(Ty.T_Tuple(mut, List.map BV.typeOf xs), xs),
				e'))
			end
		    | PT.Wrap arg =>
			cvtSimpleExp(findCFun, env, arg, fn x => BOM.mkStmt(lhs', BOM.E_Wrap x, e'))
		    | PT.CCall(f, args) =>
			cvtSimpleExps(findCFun, env, args,
			  fn xs => BOM.mkStmt(lhs', BOM.E_CCall(findCFun f, xs), e'))
		  (* end case *)
		end
	    | PT.Fun(fbs, e) => let
		fun f (fb, (env', cvtBodies)) = let
			val (env'', cvt) = cvtLambda (findCFun, env', fb, Ty.T_Fun)
			in
			  (env'', cvt::cvtBodies)
			end
		val (envWFBs, cvtBodies) = List.foldl f (env, []) fbs
		in		 
		  BOM.mkFun(
		    List.foldl (fn (cvt, fbs) => cvt envWFBs :: fbs) [] cvtBodies,
		    cvtExp(findCFun, envWFBs, e))
		end
	    | PT.Cont(fb, e) => let
	      (* NOTE: continuations are permitted to be recursive *)
		val (env', cvtBody) = cvtLambda(findCFun, env, fb, fn (argTys, _, _) => Ty.T_Cont argTys)
		in
		  BOM.mkCont(cvtBody env', cvtExp(findCFun, env', e))
		end
	    | PT.If(e1, e2, e3) =>
		cvtSimpleExp(findCFun, env, e1, fn x => BOM.mkIf(x, cvtExp(findCFun, env, e2), cvtExp(findCFun, env, e3)))
	    | PT.Case(arg, cases, dflt) => let
		fun doCase (pat, exp) = let
		      val (env', pat') = cvtPat(env, pat)
		      in
			(pat', cvtExp(findCFun, env', exp))
		      end
                in
		  cvtSimpleExp(findCFun, env, arg, fn arg =>
                    BOM.mkCase(
		      arg, 
                      List.map doCase cases,
                      case dflt
		       of NONE => NONE
			| SOME(PT.WildPat, e) => SOME(cvtExp(findCFun, env, e))
			| SOME(PT.VarPat(x, _), e) =>
			    SOME(cvtExp(findCFun, AtomMap.insert(env, x, arg), e))
		      (* end case *)))
		end
	    | PT.Apply(f, args, rets) =>
		cvtSimpleExps(findCFun, env, args,
		  fn xs => cvtSimpleExps(findCFun, env, rets,
		    fn ys => BOM.mkApply(lookup(env, f), xs, ys)))
	    | PT.Throw(k, args) =>
		cvtSimpleExps(findCFun, env, args, fn xs => BOM.mkThrow(lookup(env, k), xs))
	    | PT.Return args =>
		cvtSimpleExps(findCFun, env, args, fn xs => BOM.mkRet xs)
	    | PT.HLOpApply(hlop, args, rets) => (case HLOpEnv.find hlop
		 of SOME hlop =>
		      cvtSimpleExps(findCFun, env, args,
			fn xs => cvtSimpleExps(findCFun, env, rets,
			  fn ys => BOM.mkHLOp(hlop, xs, ys)))
		  | NONE => raise Fail(concat["unknown high-level op ", Atom.toString hlop])
		(* end case *))
	  (* end case *))

    and cvtLambda (findCFun, env, (f, params, rets, tys, e), tyCon) = let
	  fun cvt (_, ty) = cvtTy ty
	  val fnTy = tyCon(List.map cvt params, List.map cvt rets, List.map cvtTy tys)
	  val f' = BOM.Var.new(Atom.toString f, fnTy)
	  fun doBody env = let
		val (envWParams, params') = cvtVarBinds (env, params)
		val (envWParams, rets') = cvtVarBinds (envWParams, rets)
		in
		  BOM.FB{
		      f = f', params = List.rev params',
		      exh = List.rev rets', body = cvtExp(findCFun, envWParams, e)
		    }
		end
	  in
	    (AtomMap.insert(env, f, f'), doBody)
	  end

    and cvtSimpleExp (findCFun, env, e, k : BOM.var -> BOM.exp) = (case e
	   of PT.Var x => k(lookup(env, x))
	    | PT.Select(i, e) =>
		cvtSimpleExp(findCFun, env, e, fn x => let
		  val tmp = newTmp(selectType(i, BOM.Var.typeOf x))
		  in
		    BOM.mkStmt([tmp], BOM.E_Select(i, x), k tmp)
		  end)
	    | PT.AddrOf(i, e) =>
		cvtSimpleExp(findCFun, env, e, fn x => let
		  val tmp = newTmp(Ty.T_Addr(selectType(i, BOM.Var.typeOf x)))
		  in
		    BOM.mkStmt([tmp], BOM.E_AddrOf(i, x), k tmp)
		  end)
	    | PT.Const(lit, ty) => let
		  val ty = cvtTy ty
		  val tmp = newTmp ty
		  in
		    BOM.mkStmt([tmp], BOM.E_Const(lit, ty), k tmp)
		  end
	    | PT.Cast(ty, e) =>
		cvtSimpleExp(findCFun, env, e, fn x => let
		  val ty = cvtTy ty
		  val tmp = newTmp ty
		  in
		    BOM.mkStmt([tmp], BOM.E_Cast(ty, x), k tmp)
		  end)
	    | PT.Unwrap e =>
		cvtSimpleExp(findCFun, env, e, fn x => let
		  val tmp = newTmp(unwrapType(BOM.Var.typeOf x))
		  in
		    BOM.mkStmt([tmp], BOM.E_Unwrap x, k tmp)
		  end)
	    | PT.Prim(p, args) => let
		fun mkBind xs = (case (findPrim p, xs)
		       of (NONE, _) => (case Basis.findDCon p
			     of NONE => raise Fail("unknown primop " ^ Atom.toString p)
			      | SOME dc =>
				  (newTmp(BOMTyCon.dconResTy dc), BOM.E_DCon(dc, xs))
			    (* end case *))
			| (SOME(Prim1{mk, resTy, ...}), [x]) =>
			    (newTmp resTy, BOM.E_Prim(mk x))
			| (SOME(Prim2{mk, resTy, ...}), [x, y]) =>
			    (newTmp resTy, BOM.E_Prim(mk(x, y)))
			| (SOME(Prim3{mk, resTy, ...}), [x, y, z]) =>
			    (newTmp resTy, BOM.E_Prim(mk(x, y, z)))
			| _ => raise Fail("arity mismatch for primop " ^ Atom.toString p)
		      (* end case *))
		in
		  cvtSimpleExps(findCFun, env, args, fn xs => let
		    val (lhs, rhs) = mkBind xs
		    in
		      BOM.mkStmt([lhs], rhs, k lhs)
		    end)
		end
	    | PT.HostVProc =>  let
		  val tmp = newTmp Ty.T_VProc
		  in
		    BOM.mkStmt([tmp], BOM.E_HostVProc, k tmp)
		  end
	    | PT.VPLoad(offset, vp) =>
		cvtSimpleExp(findCFun, env, vp, fn vp => let
		  val tmp = newTmp(Ty.T_Any)
		  in
		    BOM.mkStmt([tmp], BOM.E_VPLoad(offset, vp), k tmp)
		  end)
	    | PT.VPStore(offset, vp, arg) =>
		raise Fail "VPStore in argument position"
	  (* end case *))

    and cvtSimpleExps (findCFun, env, exps, k) = let
	  fun cvt ([], tmps) = k(List.rev tmps)
	    | cvt (e::es, tmps) = cvtSimpleExp (findCFun, env, e, fn t => cvt(es, t::tmps))
	  in
	    cvt (exps, [])
	  end

    fun cvtFile (importEnv, PT.FILE defs) = let
	(* create a high-level operator *)
	  fun mkHLOp (name, params, exh, retTy) = let
		val attrs = if null retTy then [HLOp.NORETURN] else []
		val paramTys = List.map (fn (_, ty) => HLOp.PARAM(cvtTy ty)) params
		val exh = List.map (fn (_, ty) => cvtTy ty) exh
		val retTy = List.map cvtTy retTy
		in
		  HLOp.new (
		    name,
		    {params=paramTys, exh=exh, results=retTy},
		    attrs)
		end
	(* this is the first pass, which adds C-function prototypes to the import environment
	 * and HLOp signatures to the HLOp environment.
	 *)
	  fun insDef (PT.Import(CFunctions.CFun{var, name, retTy, argTys, attrs})) = (
		case ATbl.find importEnv var
(* FIXME: we probably should check that the existing prototype matches this one! *)
		 of SOME cfun => () (* already defined, so do nothing *)
		  | NONE => let
		      val ty = Ty.T_CFun(CFunctions.CProto(retTy, argTys, attrs))
		      val cf = BOM.mkCFun{
			      var = BOM.Var.new(Atom.toString var, ty),
			      name = name, retTy = retTy, argTys = argTys, attrs = attrs
			    }
		      in
			ATbl.insert importEnv (var, cf)
		      end
		(* end case *))
	    | insDef (PT.Define(_, name, params, exh, retTy, _)) = (case Env.find name
(* FIXME: we probably should check that the existing prototype matches this one! *)
		 of SOME hlop => () (* already defined, so do nothing *)
		  | NONE => Env.define (mkHLOp(name, params, exh, retTy))
		(* end case *))
	  fun findCFun name = (case ATbl.find importEnv name
		 of NONE => raise Fail("Unknown C function " ^ Atom.toString name)
		  | SOME(CFunctions.CFun{var, ...}) => var
		(* end case *))
	(* this is the second pass, which converts actual HLOp definitions to BOM lambdas *)
	  fun cvtDefs [] = []
	    | cvtDefs (PT.Define(inline, name, params, exh, retTy, SOME e)::defs) = let
		val hlop = valOf(Env.find name)
		val (env, doBody) = cvtLambda (findCFun, AtomMap.empty, (name, params, exh, retTy, e), Ty.T_Fun)
		val lambda = doBody env
		in
		  (hlop, inline, lambda) :: cvtDefs defs
		end
	    | cvtDefs (_::defs) = cvtDefs defs
	  in
	    List.app insDef defs;
	    cvtDefs defs
	  end		

  end
