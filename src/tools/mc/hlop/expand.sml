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

    type hlop_def = {
	name : BOM.hlop,			(* the HLOp's identifier *)
	inline : bool,				(* should the HLOp be inlined? *)
	def : BOM.lambda,			(* the HLOps definition *)
	externs : (BOM.var * int) list		(* list of external variables (i.e., C functions) *)
						(* that def references paired with a count of the *)
						(* number of references *)
      }

    val cvtFile : (import_env * string * HLOpDefPT.file) -> hlop_def list

    val cvtPrototypes : {fileName : string, pt : HLOpDefPT.file} -> HLOp.hlop list

  end = struct

    structure PT = HLOpDefPT
    structure P = Prim
    structure BTy = BOMTy
    structure BV = BOM.Var
    structure Env = HLOpEnv
    structure Basis = BOMBasis
    structure AMap = AtomMap
    structure ATbl = AtomTable
    structure VTbl = BV.Tbl

  (* an environment to keep track of any imports required by the high-level operator *)
    type import_env = BOM.var CFunctions.c_fun AtomTable.hash_table

    type hlop_def = {
	name : BOM.hlop,			(* the HLOp's identifier *)
	inline : bool,				(* should the HLOp be inlined? *)
	def : BOM.lambda,			(* the HLOps definition *)
	externs : (BOM.var * int) list		(* list of external variables (i.e., C functions) *)
						(* that def references paired with a count of the *)
						(* number of references *)
      }

  (* environment for the translation *)
    datatype env = E of {
	file : string,
	tyEnv : BTy.ty AMap.map,
	vEnv : BOM.var AMap.map
      }

    fun emptyEnv s = E{file = s, tyEnv = AMap.empty, vEnv = AMap.empty}
    fun insertTy (E{file, tyEnv, vEnv}, id, ty) =
	  E{file=file, tyEnv=AMap.insert(tyEnv, id, ty), vEnv=vEnv}
    fun findTy (E{tyEnv, ...}, id) = AMap.find(tyEnv, id)
    fun insertVar (E{file, tyEnv, vEnv}, id, x) =
	  E{file=file, tyEnv=tyEnv, vEnv=AMap.insert(vEnv, id, x)}
    fun findVar (E{vEnv, ...}, id) = AMap.find(vEnv, id)

    fun fail (E{file, ...}, msg) = Fail(concat(file :: ": " :: msg))

    fun lookup (env, x) = (case findVar(env, x)
	   of NONE => raise (fail (env, ["unbound variable ", Atom.toString x]))
	    | SOME x' => x'
	  (* end case *))

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
	   of PT.T_Any => BTy.T_Any
	    | (PT.T_Enum w) => BTy.T_Enum w
	    | (PT.T_Raw rty) => BTy.T_Raw rty
	    | (PT.T_Tuple(mut, tys)) => BTy.T_Tuple(mut, cvtTys(env, tys))
	    | (PT.T_Addr ty) => BTy.T_Addr(cvtTy(env, ty))
	    | (PT.T_Fun(argTys, exhTys, resTys)) =>
		BTy.T_Fun(cvtTys(env, argTys), cvtTys(env, exhTys), cvtTys(env, resTys))
	    | (PT.T_Cont tys) => BTy.T_Cont(cvtTys(env, tys))
	    | (PT.T_CFun cproto) => BTy.T_CFun cproto
	    | (PT.T_VProc) => BTy.T_VProc
	    | (PT.T_TyCon tyc) => (case findTy(env, tyc)
		 of SOME ty => ty
		  | NONE => (case Basis.findTyc tyc
		       of SOME tyc => BTy.T_TyCon tyc
			| NONE => raise (fail(env, ["unknown type ", Atom.toString tyc]))
		      (* end case *))
		(* end case *))
	  (* end case *))

    and cvtTys (env, tys) = List.map (fn ty => cvtTy(env, ty)) tys

    fun cvtVarPats (env, vpats) = let
	  fun f (PT.WildPat ty, (env, xs)) = let
		val ty = (case ty
		       of NONE => BTy.T_Any
			| SOME ty => cvtTy(env, ty)
		      (* end case *))
		val x' = BOM.Var.new("_wild", BTy.T_Any)
		in
		  (env, x'::xs)
		end
	    | f (PT.VarPat(x, ty), (env, xs)) = let
		val ty = cvtTy(env, ty)
		val x' = BOM.Var.new(Atom.toString x, ty)
		in
		  (insertVar(env, x, x'), x'::xs)
		end
	  val (env, xs) = List.foldl f (env, []) vpats
	  in
	    (env, List.rev xs)
	  end

    fun cvtPat (env, PT.DConPat(dc, xs)) = (case Basis.findDCon dc
	   of SOME dc => let
		val (env, xs) = cvtVarPats (env, xs)
		in
		  (env, BOM.P_DCon(dc, xs))
		end
	    | NONE => raise (fail(env, ["unknown data constructor ", Atom.toString dc]))
	  (* end case *))
      | cvtPat (env, PT.ConstPat(const, ty)) = (env, BOM.P_Const(const, cvtTy(env, ty)))

    fun cvtExp (findCFun, env, e) = (case e
	   of PT.Let(lhs, rhs, e) => let
		val (env', lhs') = cvtVarPats (env, lhs)
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
				BOM.mkStmt(lhs', BOM.E_Cast(cvtTy(env, ty), x), e'))
(* FIXME: we should check that lit and ty match! *)
	                  | PT.Const(lit, ty) => BOM.mkStmt(lhs', BOM.E_Const(lit, cvtTy(env, ty)), e')
			  | PT.MLString s => let
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
			  | PT.Unwrap arg =>
			      cvtSimpleExp(findCFun, env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.unwrap x, e'))
			  | PT.Prim(p, args) =>
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
			       of BTy.T_Tuple(true, _) => true
				| _ => false
			      (* end case *))
			in
			  cvtSimpleExps(findCFun, env, args,
			    fn xs => BOM.mkStmt(lhs', BOM.E_Alloc(BTy.T_Tuple(mut, List.map BV.typeOf xs), xs),
				e'))
			end
		    | PT.GAlloc args => let
			val mut = (case BV.typeOf(hd lhs')
			       of BTy.T_Tuple(true, _) => true
				| _ => false
			      (* end case *))
			in
			  cvtSimpleExps(findCFun, env, args,
			    fn xs => BOM.mkStmt(lhs', BOM.E_GAlloc(BTy.T_Tuple(mut, List.map BV.typeOf xs), xs),
				e'))
			end
		    | PT.Promote arg =>
			cvtSimpleExp(findCFun, env, arg, fn x => BOM.mkStmt(lhs', BOM.E_Promote x, e'))
		    | PT.Wrap arg =>
			cvtSimpleExp(findCFun, env, arg, fn x => BOM.mkStmt(lhs', BOM.wrap x, e'))
		    | PT.CCall(f, args) =>
			cvtSimpleExps(findCFun, env, args,
			  fn xs => BOM.mkStmt(lhs', BOM.E_CCall(findCFun f, xs), e'))
		  (* end case *)
		end
	    | PT.Fun(fbs, e) => let
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
	    | PT.Cont(fb, e) => let
	      (* NOTE: continuations are permitted to be recursive *)
		val (env', cvtBody) = cvtLambda(findCFun, env, fb, fn (argTys, _, _) => BTy.T_Cont argTys)
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
			| SOME(PT.WildPat _, e) => SOME(cvtExp(findCFun, env, e))
			| SOME(PT.VarPat(x, _), e) =>
			    SOME(cvtExp(findCFun, insertVar(env, x, arg), e))
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
	   of PT.Var x => k(lookup(env, x))
	    | PT.Select(i, e) =>
		cvtSimpleExp(findCFun, env, e, fn x => let
		  val tmp = newTmp(selectType(BOM.Var.typeOf x, i))
		  in
		    BOM.mkStmt([tmp], BOM.E_Select(i, x), k tmp)
		  end)
	    | PT.AddrOf(i, e) =>
		cvtSimpleExp(findCFun, env, e, fn x => let
		  val tmp = newTmp(BTy.T_Addr(selectType(BOM.Var.typeOf x, i)))
		  in
		    BOM.mkStmt([tmp], BOM.E_AddrOf(i, x), k tmp)
		  end)
	    | PT.Const(lit, ty) => let
		val ty = cvtTy(env, ty)
		val tmp = newTmp ty
		in
(* FIXME: we should check that lit and ty match! *)
		  BOM.mkStmt([tmp], BOM.E_Const(lit, ty), k tmp)
		end
	    | PT.MLString s => let
		val t1 = BV.new("_data", BTy.T_Any)
(* FIXME: the type used for the length should be architecture dependent *)
		val t2 = BV.new("_len", BTy.T_Raw BTy.T_Int)
		in
		  BOM.mkStmts([
		      ([t1], BOM.E_Const(Literal.String s, BTy.T_Any)),
		      ([t2], BOM.E_Const(Literal.Int(IntInf.fromInt(size s)), BTy.T_Raw BTy.T_Int))
		    ], BOM.mkHLOp(HLOpEnv.stringLitOp, [t1, t2], []))
		end
	    | PT.Cast(ty, e) =>
		cvtSimpleExp(findCFun, env, e, fn x => let
		  val ty = cvtTy(env, ty)
		  val tmp = newTmp ty
		  in
		    BOM.mkStmt([tmp], BOM.E_Cast(ty, x), k tmp)
		  end)
	    | PT.Unwrap e =>
		cvtSimpleExp(findCFun, env, e, fn x => let
		  val tmp = newTmp(unwrapType(BOM.Var.typeOf x))
		  in
		    BOM.mkStmt([tmp], BOM.unwrap x, k tmp)
		  end)
	    | PT.Prim(p, args) => let
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
	    | PT.HostVProc =>  let
		  val tmp = newTmp BTy.T_VProc
		  in
		    BOM.mkStmt([tmp], BOM.E_HostVProc, k tmp)
		  end
	    | PT.VPLoad(offset, vp) =>
		cvtSimpleExp(findCFun, env, vp, fn vp => let
		  val tmp = newTmp(BTy.T_Any)
		  in
		    BOM.mkStmt([tmp], BOM.E_VPLoad(offset, vp), k tmp)
		  end)
	    | PT.VPStore(offset, vp, arg) =>
		raise (fail(env, ["VPStore in argument position"]))
	  (* end case *))

    and cvtSimpleExps (findCFun, env, exps, k) = let
	  fun cvt ([], tmps) = k(List.rev tmps)
	    | cvt (e::es, tmps) = cvtSimpleExp (findCFun, env, e, fn t => cvt(es, t::tmps))
	  in
	    cvt (exps, [])
	  end

    fun tyOfPat' env = let
	  fun doit (PT.WildPat NONE) = BTy.T_Any
	    | doit (PT.WildPat(SOME ty)) = cvtTy(env, ty)
	    | doit (PT.VarPat(_, ty)) = cvtTy(env, ty)
	   in
	     doit
	   end
				     
    fun cvtPrototypes {fileName, pt=PT.FILE defs} = let 
	  fun cvtDefines (PT.Define(_, name, params, exh, retTy, _), (env, defs)) = let
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
	    | cvtDefines (PT.TypeDef(id, ty), (env, defs)) = (insertTy(env, id, cvtTy(env, ty)), defs)
	    | cvtDefines (_, (env, defs)) = (env, defs)
	  val (_, defs) = List.foldl cvtDefines (emptyEnv fileName, []) defs
	  in
	    defs
	  end (* cvtPrototypes *)

    fun cvtFile (importEnv, fileName, PT.FILE defs) = let
	(* this is the first pass, which adds C-function prototypes to the import environment,
	 * defined types to the translation environment, and HLOp signatures to the HLOp
         * environment.
	 *)
	  fun insDef (PT.Extern(CFunctions.CFun{var, name, retTy, argTys, attrs, varArg}), env) = (
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
	    | insDef (PT.TypeDef(id, ty), env) = insertTy(env, id, cvtTy(env, ty))
	    | insDef (PT.Define(_, name, params, exh, retTy, _), env) = (case Env.find name
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
(* FIXME: I don't know if rewrites should extend this environment or not. -JDR *)
	    | insDef (PT.RWImport(_), env) = env
	  val env = List.foldl insDef (emptyEnv fileName) defs
	(* this is the second pass, which converts actual HLOp definitions to BOM lambdas *)
	  fun cvtDefs [] = []
	    | cvtDefs (PT.Define(inline, name, params, exh, retTy, SOME e)::defs) = let
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
