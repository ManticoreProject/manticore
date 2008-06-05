(* translate-prim.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translation of inline BOM code.
 *)

structure TranslatePrim : sig

    val cvtTy : BOMParseTree.ty * TranslateEnv.env -> BOMTy.ty


  end = struct

    structure BPT = BOMParseTree
    structure E = TranslateEnv
    structure P = Prim
    structure BTy = BOMTy
    structure BV = BOM.Var

  (* table mapping primop names to prim_info *)
    structure MkPrim = MakePrimFn (
	type var = BOM.var
	type ty = BTy.ty
	val anyTy = BTy.T_Any
	val boolTy = BTy.boolTy
	val addrTy = BTy.T_Addr(BTy.T_Any)
	val rawTy = BTy.T_Raw)

    datatype prim_info = datatype MkPrim.prim_info
    val findPrim = MkPrim.findPrim

  (* some type utilities *)
    val unwrapType = BOMTyUtil.unwrap
    val selectType = BOMTyUtil.select

    fun newTmp ty = BV.new("_t", ty)

  (* convert a parse-tree type express to a BOM type *)
    fun cvtTy (env, ty) = (case ty
	   of BPT.T_Any => BTy.T_Any
	    | (BPT.T_Enum w) => BTy.T_Enum w
	    | (BPT.T_Raw rty) => BTy.T_Raw rty
	    | (BPT.T_Tuple(mut, tys)) => BTy.T_Tuple(mut, cvtTys(env, tys))
	    | (BPT.T_Addr ty) => BTy.T_Addr(cvtTy(env, ty))
	    | (BPT.T_Fun(argTys, exhTys, resTys)) =>
		BTy.T_Fun(cvtTys(env, argTys), cvtTys(env, exhTys), cvtTys(env, resTys))
	    | (BPT.T_Cont tys) => BTy.T_Cont(cvtTys(env, tys))
	    | (BPT.T_CFun cproto) => BTy.T_CFun cproto
	    | (BPT.T_VProc) => BTy.T_VProc
	    | (BPT.T_TyCon tyc) => (case E.findBOMTy(env, tyc)
		 of SOME ty => ty
		  | NONE => raise (fail(env, ["unknown BOM type ", Atom.toString tyc])
		(* end case *))
	  (* end case *))

    and cvtTys (env, tys) = List.map (fn ty => cvtTy(env, ty)) tys

    fun cvtVarPats (env, vpats) = let
	  fun f (BPT.WildPat ty, (env, xs)) = let
		val ty = (case ty
		       of NONE => BTy.T_Any
			| SOME ty => cvtTy(env, ty)
		      (* end case *))
		val x' = BOM.Var.new("_wild", BTy.T_Any)
		in
		  (env, x'::xs)
		end
	    | f (BPT.VarPat(x, ty), (env, xs)) = let
		val ty = cvtTy(env, ty)
		val x' = BOM.Var.new(Atom.toString x, ty)
		in
		  (insertVar(env, x, x'), x'::xs)
		end
	  val (env, xs) = List.foldl f (env, []) vpats
	  in
	    (env, List.rev xs)
	  end

    fun cvtPat (env, BPT.DConPat(dc, xs)) = (case E.findBOMCon dc
	   of SOME dc => let
		val (env, xs) = cvtVarPats (env, xs)
		in
		  (env, BOM.P_DCon(dc, xs))
		end
	    | NONE => raise (fail(env, ["unknown BOM data constructor ", Atom.toString dc]))
	  (* end case *))
      | cvtPat (env, BPT.ConstPat(const, ty)) = (env, BOM.P_Const(const, cvtTy(env, ty)))

    fun cvtExp (findCFun, env, e) = (case e
	   of BPT.Let(lhs, rhs, e) => let
		val (env', lhs') = cvtVarPats (env, lhs)
		val e' = cvtExp(findCFun, env', e)
		in
		  case rhs
		   of BPT.Exp e => BOM.mkLet(lhs', cvtExp(findCFun, env, e), e')
		    | BPT.SimpleExp e => (case e
			 of BPT.Var x => BOM.mkLet(lhs', BOM.mkRet[lookup(env, x)], e')
			  | BPT.Select(i, arg) =>
			      cvtSimpleExp(findCFun, env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Select(i, x), e'))
			  | BPT.AddrOf(i, arg) =>
			      cvtSimpleExp(findCFun, env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_AddrOf(i, x), e'))
			  | BPT.Cast(ty, arg) =>
			      cvtSimpleExp(findCFun, env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Cast(cvtTy(env, ty), x), e'))
(* FIXME: we should check that lit and ty match! *)
	                  | BPT.Const(lit, ty) => BOM.mkStmt(lhs', BOM.E_Const(lit, cvtTy(env, ty)), e')
			  | BPT.MLString s => let
			      val t1 = BV.new("_data", BTy.T_Any)
(* FIXME: the type used for the length should be architecture dependent *)
			      val t2 = BV.new("_len", BTy.T_Raw BTy.T_Int)
			      in
				BOM.mkStmts([
				    ([t1], BOM.E_Const(Literal.String s, BTy.T_Any)),
				    ([t2], BOM.E_Const(Literal.Int(IntInf.fromInt(size s)), BTy.T_Raw BTy.T_Int))
				  ],
				BOM.mkLet(lhs', BOM.mkHLOp(HLOpEnv.stringLitOp, [t1, t2], []), e'))
			      end
			  | BPT.Unwrap arg =>
			      cvtSimpleExp(findCFun, env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.unwrap x, e'))
			  | BPT.Prim(p, args) =>
			      cvtSimpleExps(findCFun, env, args, fn xs => let
				val rhs = (case (findPrim p, xs)
				       of (NONE, _) => (case Basis.findDCon p
					     of NONE => raise (fail(env, ["unknown primop ", Atom.toString p]))
					      | SOME dc => BOM.E_DCon(dc, xs)
					    (* end case *))
					| (SOME(Prim1{mk, ...}), [x]) => BOM.E_Prim(mk x)
					| (SOME(Prim2{mk, ...}), [x, y]) => BOM.E_Prim(mk(x, y))
					| (SOME(Prim3{mk, ...}), [x, y, z]) => BOM.E_Prim(mk(x, y, z))
					| _ => raise (fail(env, ["arity mismatch for primop ", Atom.toString p]))
				      (* end case *))
				in
				  BOM.mkStmt(lhs', rhs, e')
				end)
			  | BPT.HostVProc => BOM.mkStmt(lhs', BOM.E_HostVProc, e')
			  | BPT.VPLoad(offset, vp) =>
			      cvtSimpleExp(findCFun, env, vp, fn vp =>
				BOM.mkStmt(lhs', BOM.E_VPLoad(offset, vp), e'))
			  | BPT.VPStore(offset, vp, arg) =>
			      cvtSimpleExp(findCFun, env, vp, fn vp =>
				cvtSimpleExp(findCFun, env, arg, fn x =>
				  BOM.mkStmt(lhs', BOM.E_VPStore(offset, vp, x), e')))
			(* end case *))
		    | BPT.Update(i, arg, rhs) =>
			cvtSimpleExp(findCFun, env, arg, fn x =>
			  cvtSimpleExp(findCFun, env, rhs, fn y =>
			    BOM.mkStmt(lhs', BOM.E_Update(i, x, y), e')))
		    | BPT.Alloc args => let
			val mut = (case BV.typeOf(hd lhs')
			       of BTy.T_Tuple(true, _) => true
				| _ => false
			      (* end case *))
			in
			  cvtSimpleExps(findCFun, env, args,
			    fn xs => BOM.mkStmt(lhs', BOM.E_Alloc(BTy.T_Tuple(mut, List.map BV.typeOf xs), xs),
				e'))
			end
		    | BPT.GAlloc args => let
			val mut = (case BV.typeOf(hd lhs')
			       of BTy.T_Tuple(true, _) => true
				| _ => false
			      (* end case *))
			in
			  cvtSimpleExps(findCFun, env, args,
			    fn xs => BOM.mkStmt(lhs', BOM.E_GAlloc(BTy.T_Tuple(mut, List.map BV.typeOf xs), xs),
				e'))
			end
		    | BPT.Promote arg =>
			cvtSimpleExp(findCFun, env, arg, fn x => BOM.mkStmt(lhs', BOM.E_Promote x, e'))
		    | BPT.Wrap arg =>
			cvtSimpleExp(findCFun, env, arg, fn x => BOM.mkStmt(lhs', BOM.wrap x, e'))
		    | BPT.CCall(f, args) =>
			cvtSimpleExps(findCFun, env, args,
			  fn xs => BOM.mkStmt(lhs', BOM.E_CCall(findCFun f, xs), e'))
		  (* end case *)
		end
	    | BPT.Fun(fbs, e) => let
		fun f (fb, (env', cvtBodies)) = let
			val (env'', cvt) = cvtLambda (findCFun, env', fb, BTy.T_Fun)
			in
			  (env'', cvt::cvtBodies)
			end
		val (envWFBs, cvtBodies) = List.foldl f (env, []) fbs
		in		 
		  BOM.mkFun(
		    List.foldl (fn (cvt, fbs) => cvt envWFBs :: fbs) [] cvtBodies,
		    cvtExp(findCFun, envWFBs, e))
		end
	    | BPT.Cont(fb, e) => let
	      (* NOTE: continuations are permitted to be recursive *)
		val (env', cvtBody) = cvtLambda(findCFun, env, fb, fn (argTys, _, _) => BTy.T_Cont argTys)
		in
		  BOM.mkCont(cvtBody env', cvtExp(findCFun, env', e))
		end
	    | BPT.If(e1, e2, e3) =>
		cvtSimpleExp(findCFun, env, e1, fn x => BOM.mkIf(x, cvtExp(findCFun, env, e2), cvtExp(findCFun, env, e3)))
	    | BPT.Case(arg, cases, dflt) => let
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
			| SOME(BPT.WildPat _, e) => SOME(cvtExp(findCFun, env, e))
			| SOME(BPT.VarPat(x, _), e) =>
			    SOME(cvtExp(findCFun, insertVar(env, x, arg), e))
		      (* end case *)))
		end
	    | BPT.Apply(f, args, rets) =>
		cvtSimpleExps(findCFun, env, args,
		  fn xs => cvtSimpleExps(findCFun, env, rets,
		    fn ys => BOM.mkApply(lookup(env, f), xs, ys)))
	    | BPT.Throw(k, args) =>
		cvtSimpleExps(findCFun, env, args, fn xs => BOM.mkThrow(lookup(env, k), xs))
	    | BPT.Return args =>
		cvtSimpleExps(findCFun, env, args, fn xs => BOM.mkRet xs)
	    | BPT.HLOpApply(hlop, args, rets) => (case HLOpEnv.find hlop
		 of SOME hlop =>
		      cvtSimpleExps(findCFun, env, args,
			fn xs => cvtSimpleExps(findCFun, env, rets,
			  fn ys => BOM.mkHLOp(hlop, xs, ys)))
		  | NONE => raise (fail(env, ["unknown high-level op ", Atom.toString hlop]))
		(* end case *))
	  (* end case *))

    and cvtLambda (findCFun, env, (f, params, rets, tys, e), tyCon) = let
	  val (envWParams, params) = cvtVarPats (env, params)
	  val (envWParams, rets) = cvtVarPats (envWParams, rets)
	  val fnTy = tyCon(List.map BV.typeOf params, List.map BV.typeOf rets, cvtTys(env, tys))
	  val f' = BOM.Var.new(Atom.toString f, fnTy)
	  val envWParams = insertVar(envWParams, f, f')
	  fun doBody env = BOM.FB{
		  f = f', params = params, exh = rets, body = cvtExp(findCFun, envWParams, e)
		}
	  in
	    (insertVar(env, f, f'), doBody)
	  end

    and cvtSimpleExp (findCFun, env, e, k : BOM.var -> BOM.exp) = (case e
	   of BPT.Var x => k(lookup(env, x))
	    | BPT.Select(i, e) =>
		cvtSimpleExp(findCFun, env, e, fn x => let
		  val tmp = newTmp(selectType(BOM.Var.typeOf x, i))
		  in
		    BOM.mkStmt([tmp], BOM.E_Select(i, x), k tmp)
		  end)
	    | BPT.AddrOf(i, e) =>
		cvtSimpleExp(findCFun, env, e, fn x => let
		  val tmp = newTmp(BTy.T_Addr(selectType(BOM.Var.typeOf x, i)))
		  in
		    BOM.mkStmt([tmp], BOM.E_AddrOf(i, x), k tmp)
		  end)
	    | BPT.Const(lit, ty) => let
		val ty = cvtTy(env, ty)
		val tmp = newTmp ty
		in
(* FIXME: we should check that lit and ty match! *)
		  BOM.mkStmt([tmp], BOM.E_Const(lit, ty), k tmp)
		end
	    | BPT.MLString s => let
		val t1 = BV.new("_data", BTy.T_Any)
(* FIXME: the type used for the length should be architecture dependent *)
		val t2 = BV.new("_len", BTy.T_Raw BTy.T_Int)
		in
		  BOM.mkStmts([
		      ([t1], BOM.E_Const(Literal.String s, BTy.T_Any)),
		      ([t2], BOM.E_Const(Literal.Int(IntInf.fromInt(size s)), BTy.T_Raw BTy.T_Int))
		    ], BOM.mkHLOp(HLOpEnv.stringLitOp, [t1, t2], []))
		end
	    | BPT.Cast(ty, e) =>
		cvtSimpleExp(findCFun, env, e, fn x => let
		  val ty = cvtTy(env, ty)
		  val tmp = newTmp ty
		  in
		    BOM.mkStmt([tmp], BOM.E_Cast(ty, x), k tmp)
		  end)
	    | BPT.Unwrap e =>
		cvtSimpleExp(findCFun, env, e, fn x => let
		  val tmp = newTmp(unwrapType(BOM.Var.typeOf x))
		  in
		    BOM.mkStmt([tmp], BOM.unwrap x, k tmp)
		  end)
	    | BPT.Prim(p, args) => let
		fun mkBind xs = (case (findPrim p, xs)
		       of (NONE, _) => (case Basis.findDCon p
			     of NONE => raise (fail(env, ["unknown primop ", Atom.toString p]))
			      | SOME dc =>
				  (newTmp(BOMTyCon.dconResTy dc), BOM.E_DCon(dc, xs))
			    (* end case *))
			| (SOME(Prim1{mk, resTy, ...}), [x]) =>
			    (newTmp resTy, BOM.E_Prim(mk x))
			| (SOME(Prim2{mk, resTy, ...}), [x, y]) =>
			    (newTmp resTy, BOM.E_Prim(mk(x, y)))
			| (SOME(Prim3{mk, resTy, ...}), [x, y, z]) =>
			    (newTmp resTy, BOM.E_Prim(mk(x, y, z)))
			| _ => raise (fail(env, ["arity mismatch for primop ", Atom.toString p]))
		      (* end case *))
		in
		  cvtSimpleExps(findCFun, env, args, fn xs => let
		    val (lhs, rhs) = mkBind xs
		    in
		      BOM.mkStmt([lhs], rhs, k lhs)
		    end)
		end
	    | BPT.HostVProc =>  let
		  val tmp = newTmp BTy.T_VProc
		  in
		    BOM.mkStmt([tmp], BOM.E_HostVProc, k tmp)
		  end
	    | BPT.VPLoad(offset, vp) =>
		cvtSimpleExp(findCFun, env, vp, fn vp => let
		  val tmp = newTmp(BTy.T_Any)
		  in
		    BOM.mkStmt([tmp], BOM.E_VPLoad(offset, vp), k tmp)
		  end)
	    | BPT.VPStore(offset, vp, arg) =>
		raise (fail(env, ["VPStore in argument position"]))
	  (* end case *))

    and cvtSimpleExps (findCFun, env, exps, k) = let
	  fun cvt ([], tmps) = k(List.rev tmps)
	    | cvt (e::es, tmps) = cvtSimpleExp (findCFun, env, e, fn t => cvt(es, t::tmps))
	  in
	    cvt (exps, [])
	  end

    fun tyOfPat' env = let
	  fun doit (BPT.WildPat NONE) = BTy.T_Any
	    | doit (BPT.WildPat(SOME ty)) = cvtTy(env, ty)
	    | doit (BPT.VarPat(_, ty)) = cvtTy(env, ty)
	   in
	     doit
	   end
				     
    fun cvtPrototypes {fileName, pt=BPT.FILE defs} = let 
	  fun cvtDefines (BPT.Define(_, name, params, exh, retTy, _), (env, defs)) = let
		val tyOfPat = tyOfPat' env
		val paramTys = List.map (fn p => HLOp.PARAM (tyOfPat p)) params
		val exhTys = List.map tyOfPat exh
		val (retTy, attrs) = (case retTy
		       of NONE => ([], [HLOp.NORETURN])
			| SOME tys => (cvtTys (env, tys), [])
		      (* end case *))
		val hlop = HLOp.new (
		      name,
		      {params=paramTys, exh=exhTys, results=retTy},
		      attrs) 
		in	       
		  (env, hlop :: defs)
		end
	    | cvtDefines (BPT.TypeDef(id, ty), (env, defs)) = (insertTy(env, id, cvtTy(env, ty)), defs)
	    | cvtDefines (_, (env, defs)) = (env, defs)
	  val (_, defs) = List.foldl cvtDefines (emptyEnv fileName, []) defs
	  in
	    defs
	  end (* cvtPrototypes *)

    fun cvtFile (importEnv, fileName, BPT.FILE defs) = let
	(* this is the first pass, which adds C-function prototypes to the import environment,
	 * defined types to the translation environment, and HLOp signatures to the HLOp
         * environment.
	 *)
	  fun insDef (BPT.Extern(CFunctions.CFun{var, name, retTy, argTys, attrs, varArg}), env) = (
		case ATbl.find importEnv var
(* FIXME: we probably should check that the existing prototype matches this one! *)
		 of SOME cfun => env (* already defined, so do nothing *)
		  | NONE => let
		      val ty = BTy.T_CFun(CFunctions.CProto(retTy, argTys, attrs))
		      val cf = BOM.mkCFun{
			      var = BOM.Var.new(Atom.toString var, ty),
			      name = name, retTy = retTy, argTys = argTys, attrs = attrs, varArg = varArg
			    }
		      in
			ATbl.insert importEnv (var, cf); env
		      end
		(* end case *))
	    | insDef (BPT.TypeDef(id, ty), env) = insertTy(env, id, cvtTy(env, ty))
	    | insDef (BPT.Define(_, name, params, exh, retTy, _), env) = (case Env.find name
(* FIXME: we probably should check that the existing prototype matches this one! *)
		 of SOME hlop => env (* already defined, so do nothing *)
		  | NONE => let
		    (* create a high-level operator *)
		      val (retTy, attrs) = (case retTy
			     of NONE => ([], [HLOp.NORETURN])
			      | SOME tys => (cvtTys(env, tys), [])
			    (* end case *))		      
		      val tyOfPat = tyOfPat' env
		      val paramTys = List.map (fn p => HLOp.PARAM(tyOfPat p)) params
		      val exhTys = List.map tyOfPat exh
		      in
			Env.define (HLOp.new (
			  name,
			  {params=paramTys, exh=exhTys, results=retTy},
			  attrs));
			env
		      end
		(* end case *);
		env)
	  val env = List.foldl insDef (emptyEnv fileName) defs
	(* this is the second pass, which converts actual HLOp definitions to BOM lambdas *)
	  fun cvtDefs [] = []
	    | cvtDefs (BPT.Define(inline, name, params, exh, retTy, SOME e)::defs) = let
		val hlop = valOf(Env.find name)
		val retTy = (case retTy of NONE => [] | SOME tys => tys)
		val cfuns = VTbl.mkTable (16, Fail "cfun table")
		fun findCFun name = (case ATbl.find importEnv name
		       of NONE => raise (fail(env, ["Unknown C function ", Atom.toString name]))
			| SOME(cf as CFunctions.CFun{var, ...}) => (
			  (* increment the count of references to the C function *)
			    case VTbl.find cfuns var
			     of NONE => VTbl.insert cfuns (var, 1)
			      | SOME n => VTbl.insert cfuns (var, n+1)
			    (* end case *);
			    var)
		      (* end case *))
		val (env, doBody) = cvtLambda (findCFun, env, (name, params, exh, retTy, e), BTy.T_Fun)
		val lambda = doBody env
		val def = {
			name = hlop,
			inline = inline,
			def = lambda,
			externs = VTbl.listItemsi cfuns
		      }
		in
		  def :: cvtDefs defs
		end
	    | cvtDefs (_::defs) = cvtDefs defs
	  in
	    cvtDefs defs
	  end		

  end

