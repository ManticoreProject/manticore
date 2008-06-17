(* translate-prim.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translation of inline BOM code.
 *)

structure TranslatePrim : sig

    val cvtRhs : ProgramParseTree.PML2.BOMParseTree.prim_val_rhs -> BOM.exp

  end = struct

    structure BPT = ProgramParseTree.PML2.BOMParseTree
    structure PTVar = ProgramParseTree.Var
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

    fun fail ss = raise Fail (String.concat ss)

    fun newTmp ty = BV.new("_t", ty)

  (* convert a parse-tree type express to a BOM type *)
    fun cvtTy (ty) = (case ty
	   of BPT.T_Any => BTy.T_Any
	    | (BPT.T_Enum w) => BTy.T_Enum w
	    | (BPT.T_Raw rty) => BTy.T_Raw rty
	    | (BPT.T_Tuple(mut, tys)) => BTy.T_Tuple(mut, cvtTys(tys))
	    | (BPT.T_Addr ty) => BTy.T_Addr(cvtTy(ty))
	    | (BPT.T_Fun(argTys, exhTys, resTys)) =>
		BTy.T_Fun(cvtTys(argTys), cvtTys(exhTys), cvtTys(resTys))
	    | (BPT.T_Cont tys) => BTy.T_Cont(cvtTys(tys))
	    | (BPT.T_CFun cproto) => BTy.T_CFun cproto
	    | (BPT.T_VProc) => BTy.T_VProc
	    | (BPT.T_TyCon tyc) => (case E.findBOMTy tyc
                    of NONE => raise Fail ""
		     | SOME ty => ty
		   (* end case *))
	  (* end case *))

    and cvtTys (tys) = List.map (fn ty => cvtTy(ty)) tys

    fun cvtVarPats vpats = let
	  fun f (BPT.P_VPMark {tree, span}) = f tree
	    | f (BPT.P_Wild ty) = let
		val ty = (case ty
		       of NONE => BTy.T_Any
			| SOME ty => cvtTy(ty)
		      (* end case *))
		val x' = BOM.Var.new("_wild", BTy.T_Any)
		in
		  x'
		end
	    | f (BPT.P_Var(x, ty)) = let
		val ty = cvtTy(ty)
		val x' = BOM.Var.new(PTVar.nameOf x, ty)
		in
		  E.insertBOMVar(x, x');
		  x'
		end
	  in
	    List.map f vpats
	  end

    fun cvtPat (BPT.P_DCon(dc, xs)) = (case E.findBOMCon dc
	   of SOME dc => let
		val (xs) = cvtVarPats (xs)
		in
		  (BOM.P_DCon(dc, xs))
		end
	    | NONE => raise Fail(String.concat ["unknown BOM data constructor ", PTVar.nameOf dc])
	  (* end case *))
      | cvtPat (BPT.P_Const(const, ty)) = (BOM.P_Const(const, cvtTy(ty)))

    fun lookup v = (case E.findBOMVar v
           of NONE => raise Fail(String.concat ["unknown BOM variable ", PTVar.nameOf v])
	    | SOME v => v
           (* end case *))

    fun cvtExp (findCFun, e) = (case e
	   of BPT.E_Mark {tree, span} => cvtExp(findCFun, tree)
	    | BPT.E_Let(lhs, rhs, e) => let
		val (lhs') = cvtVarPats (lhs)
		val e' = cvtExp(findCFun, e)
		in
		  case rhs
		   of BPT.RHS_Mark {tree, span} => cvtExp(findCFun, BPT.E_Let(lhs, tree, e))
		    | BPT.RHS_Exp e => BOM.mkLet(lhs', cvtExp(findCFun, e), e')
		    | BPT.RHS_SimpleExp e'' => (case e''
			 of BPT.SE_Mark {tree, span} =>
			      cvtExp(findCFun, BPT.E_Let(lhs, BPT.RHS_SimpleExp tree, e))
			  | BPT.SE_Var x => BOM.mkLet(lhs', BOM.mkRet[lookup(x)], e')
			  | BPT.SE_Select(i, arg) =>
			      cvtSimpleExp(findCFun, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Select(i, x), e'))
			  | BPT.SE_AddrOf(i, arg) =>
			      cvtSimpleExp(findCFun, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_AddrOf(i, x), e'))
			  | BPT.SE_Cast(ty, arg) =>
			      cvtSimpleExp(findCFun, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Cast(cvtTy(ty), x), e'))
(* FIXME: we should check that lit and ty match! *)
	                  | BPT.SE_Const(lit, ty) => BOM.mkStmt(lhs', BOM.E_Const(lit, cvtTy(ty)), e')
			  | BPT.SE_MLString s => let
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
			  | BPT.SE_Unwrap arg =>
			      cvtSimpleExp(findCFun, arg, fn x =>
				BOM.mkStmt(lhs', BOM.unwrap x, e'))
			  | BPT.SE_Prim(p, args) =>
			      cvtSimpleExps(findCFun, args, fn xs => let
				val rhs = (case (findPrim p, xs)
				       of (NONE, _) => (case BOMBasis.findDCon p
					     of NONE => raise (fail(["unknown primop ", Atom.toString p]))
					      | SOME dc => BOM.E_DCon(dc, xs)
					    (* end case *))
					| (SOME(Prim1{mk, ...}), [x]) => BOM.E_Prim(mk x)
					| (SOME(Prim2{mk, ...}), [x, y]) => BOM.E_Prim(mk(x, y))
					| (SOME(Prim3{mk, ...}), [x, y, z]) => BOM.E_Prim(mk(x, y, z))
					| _ => raise (fail(["arity mismatch for primop ", Atom.toString p]))
				      (* end case *))
				in
				  BOM.mkStmt(lhs', rhs, e')
				end)
			  | BPT.SE_HostVProc => BOM.mkStmt(lhs', BOM.E_HostVProc, e')
			  | BPT.SE_VPLoad(offset, vp) =>
			      cvtSimpleExp(findCFun, vp, fn vp =>
				BOM.mkStmt(lhs', BOM.E_VPLoad(offset, vp), e'))
			  | BPT.SE_Alloc args => let
				val mut = (case BV.typeOf(hd lhs')
					    of BTy.T_Tuple(true, _) => true
					     | _ => false
					  (* end case *))
 			        in
			          cvtSimpleExps(findCFun, args,
			             fn xs => BOM.mkStmt(lhs', BOM.E_Alloc(BTy.T_Tuple(mut, List.map BV.typeOf xs), xs),
				      e'))
			        end
			(* end case *))
		    | BPT.RHS_Update(i, arg, rhs) => 
			cvtSimpleExp(findCFun, arg, fn x =>
			  cvtSimpleExp(findCFun, rhs, fn y =>
			    BOM.mkStmt(lhs', BOM.E_Update(i, x, y), e')))
		    | BPT.RHS_VPStore(offset, vp, arg) =>
  		        cvtSimpleExp(findCFun, vp, fn vp =>
				cvtSimpleExp(findCFun, arg, fn x =>
				  BOM.mkStmt(lhs', BOM.E_VPStore(offset, vp, x), e')))
		    | BPT.RHS_Promote arg =>
			cvtSimpleExp(findCFun, arg, fn x => BOM.mkStmt(lhs', BOM.E_Promote x, e'))
		    | BPT.RHS_CCall(f, args) =>
			cvtSimpleExps(findCFun, args,
			  fn xs => BOM.mkStmt(lhs', BOM.E_CCall(findCFun f, xs), e'))
		  (* end case *)
		end
	    | BPT.E_Fun(fbs, e) => let
		fun f (fb, cvtBodies) = let
			val cvt = cvtLambda (findCFun, fb, BTy.T_Fun)
			in
			  cvt::cvtBodies
			end
		val cvtBodies = List.foldl f ([]) fbs
		in		 
		  BOM.mkFun(
		    List.foldl (fn (cvt, fbs') => cvt () :: fbs') [] cvtBodies,
		    cvtExp(findCFun, e))
		end
	    | BPT.E_Cont(fb, e) => let
	      (* NOTE: continuations are permitted to be recursive *)
		val cvtBody = cvtLambda(findCFun, fb, fn (argTys, _, _) => BTy.T_Cont argTys)
		in
		  BOM.mkCont(cvtBody(), cvtExp(findCFun, e))
		end
	    | BPT.E_If(e1, e2, e3) =>
		cvtSimpleExp(findCFun, e1, fn x => BOM.mkIf(x, cvtExp(findCFun, e2), cvtExp(findCFun, e3)))
	    | BPT.E_Case(arg, cases, dflt) => let
		fun doCase (pat, exp) = let
		      val (pat') = cvtPat(pat)
		      in
			(pat', cvtExp(findCFun, exp))
		      end
                in
		  cvtSimpleExp(findCFun, arg, fn arg =>
                    BOM.mkCase(
		      arg, 
                      List.map doCase cases,
                      case dflt
		       of NONE => NONE
			| SOME(BPT.P_Wild _, e) => SOME(cvtExp(findCFun, e))
			| SOME(BPT.P_Var(x, _), e) => (
			    E.insertBOMVar(x, arg);
			    SOME(cvtExp(findCFun, e)))
		      (* end case *)))
		end
	    | BPT.E_Apply(f, args, rets) =>
		cvtSimpleExps(findCFun, args,
		  fn xs => cvtSimpleExps(findCFun, rets,
		    fn ys => BOM.mkApply(lookup(f), xs, ys)))
	    | BPT.E_Throw(k, args) =>
		cvtSimpleExps(findCFun, args, fn xs => BOM.mkThrow(lookup(k), xs))
	    | BPT.E_Return args =>
		cvtSimpleExps(findCFun, args, fn xs => BOM.mkRet xs)
	    | BPT.E_HLOpApply(hlop, args, rets) => (case HLOpEnv.find hlop
		 of SOME hlop =>
		      cvtSimpleExps(findCFun, args,
			fn xs => cvtSimpleExps(findCFun, rets,
			  fn ys => BOM.mkHLOp(hlop, xs, ys)))
		  | NONE => raise (fail(["unknown high-level op ", Atom.toString hlop]))
		(* end case *))
	  (* end case *))

    and cvtLambda (findCFun, (f, params, rets, tys, e), tyCon) = let
	  val params = cvtVarPats params
	  val rets = cvtVarPats rets
	  val fnTy = tyCon(List.map BV.typeOf params, List.map BV.typeOf rets, cvtTys tys)
	  val f' = BOM.Var.new(PTVar.nameOf f, fnTy)
	  val _ = E.insertBOMVar(f, f')
	  fun doBody () = BOM.FB{
		  f = f', params = params, exh = rets, body = cvtExp(findCFun, e)
		}
	  in
	    E.insertBOMVar(f, f');
	    doBody
	  end

    and cvtSimpleExp (findCFun, e, k : BOM.var -> BOM.exp) = (case e
	   of BPT.SE_Mark {tree, span} => cvtSimpleExp(findCFun, tree, k)
	    | BPT.SE_Var x => k(lookup(x))
	    | BPT.SE_Alloc args => 
	      (* NOTE: nested tuples are always immutable *)
		cvtSimpleExps(findCFun, args, fn xs => let
                   val mut = false
                   val tys = List.map BV.typeOf xs
	           val tmp = newTmp(BTy.T_Tuple(mut, tys))
		   in
		      BOM.mkStmt([tmp], BOM.E_Alloc(BTy.T_Tuple(mut, tys), xs), k tmp)
                   end)
	    | BPT.SE_Select(i, e) =>
		cvtSimpleExp(findCFun, e, fn x => let
		  val tmp = newTmp(selectType(BOM.Var.typeOf x, i))
		  in
		    BOM.mkStmt([tmp], BOM.E_Select(i, x), k tmp)
		  end)
	    | BPT.SE_AddrOf(i, e) =>
		cvtSimpleExp(findCFun, e, fn x => let
		  val tmp = newTmp(BTy.T_Addr(selectType(BOM.Var.typeOf x, i)))
		  in
		    BOM.mkStmt([tmp], BOM.E_AddrOf(i, x), k tmp)
		  end)
	    | BPT.SE_Const(lit, ty) => let
		val ty = cvtTy(ty)
		val tmp = newTmp ty
		in
(* FIXME: we should check that lit and ty match! *)
		  BOM.mkStmt([tmp], BOM.E_Const(lit, ty), k tmp)
		end
	    | BPT.SE_MLString s => let
		val t1 = BV.new("_data", BTy.T_Any)
(* FIXME: the type used for the length should be architecture dependent *)
		val t2 = BV.new("_len", BTy.T_Raw BTy.T_Int)
		in
		  BOM.mkStmts([
		      ([t1], BOM.E_Const(Literal.String s, BTy.T_Any)),
		      ([t2], BOM.E_Const(Literal.Int(IntInf.fromInt(size s)), BTy.T_Raw BTy.T_Int))
		    ], BOM.mkHLOp(HLOpEnv.stringLitOp, [t1, t2], []))
		end
	    | BPT.SE_Cast(ty, e) =>
		cvtSimpleExp(findCFun, e, fn x => let
		  val ty = cvtTy(ty)
		  val tmp = newTmp ty
		  in
		    BOM.mkStmt([tmp], BOM.E_Cast(ty, x), k tmp)
		  end)
	    | BPT.SE_Unwrap e =>
		cvtSimpleExp(findCFun, e, fn x => let
		  val tmp = newTmp(unwrapType(BOM.Var.typeOf x))
		  in
		    BOM.mkStmt([tmp], BOM.unwrap x, k tmp)
		  end)
	    | BPT.SE_Prim(p, args) => let
		fun mkBind xs = (case (findPrim p, xs)
		       of (NONE, _) => (case BOMBasis.findDCon p
			     of NONE => raise (fail(["unknown primop ", Atom.toString p]))
			      | SOME dc =>
				  (newTmp(BOMTyCon.dconResTy dc), BOM.E_DCon(dc, xs))
			    (* end case *))
			| (SOME(Prim1{mk, resTy, ...}), [x]) =>
			    (newTmp resTy, BOM.E_Prim(mk x))
			| (SOME(Prim2{mk, resTy, ...}), [x, y]) =>
			    (newTmp resTy, BOM.E_Prim(mk(x, y)))
			| (SOME(Prim3{mk, resTy, ...}), [x, y, z]) =>
			    (newTmp resTy, BOM.E_Prim(mk(x, y, z)))
			| _ => raise (fail(["arity mismatch for primop ", Atom.toString p]))
		      (* end case *))
		in
		  cvtSimpleExps(findCFun, args, fn xs => let
		    val (lhs, rhs) = mkBind xs
		    in
		      BOM.mkStmt([lhs], rhs, k lhs)
		    end)
		end
	    | BPT.SE_HostVProc =>  let
		  val tmp = newTmp BTy.T_VProc
		  in
		    BOM.mkStmt([tmp], BOM.E_HostVProc, k tmp)
		  end
	    | BPT.SE_VPLoad(offset, vp) =>
		cvtSimpleExp(findCFun, vp, fn vp => let
		  val tmp = newTmp(BTy.T_Any)
		  in
		    BOM.mkStmt([tmp], BOM.E_VPLoad(offset, vp), k tmp)
		  end)
	  (* end case *))

    and cvtSimpleExps (findCFun, exps, k) = let
	  fun cvt ([], tmps) = k(List.rev tmps)
	    | cvt (e::es, tmps) = cvtSimpleExp (findCFun, e, fn t => cvt(es, t::tmps))
	  in
	    cvt (exps, [])
	  end
(*
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
	    | cvtDefines (BPT.TypeDef(id, ty), (env, defs)) = (insertTy(id, cvtTy(env, ty)), defs)
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
	    | insDef (BPT.TypeDef(id, ty), env) = insertTy(id, cvtTy(env, ty))
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
*)

    fun findCFun _ = raise Fail ""

    fun cvtRhs rhs = (case rhs
           of BPT.VarPrimVal v => BOM.mkRet [lookup v]
	    | BPT.LambdaPrimVal fb => let
		  val lambda = cvtLambda (findCFun, fb, BTy.T_Fun)
		  val l as BOM.FB{f, ...} = lambda()
		  in
		      BOM.mkFun([l], BOM.mkRet [f])
		  end
           (* end case *))

  end

