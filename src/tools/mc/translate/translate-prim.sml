(* translate-prim.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translation of inline BOM code.
 *)

structure TranslatePrim : sig

  (* convert a right-hand side inline BOM declaration to an expression *)
    val cvtRhs : (TranslateEnv.env * 
		  Var.var * 
		  Types.ty_scheme * 
		  ProgramParseTree.PML2.BOMParseTree.prim_val_rhs) 
		 -> (TranslateEnv.env * BOM.Var.var * BOM.exp) option

  (* convert BOM definitions. this process occurs silently, by adding
   * definitions to environments and caches.
   *)
    val cvtCode : (TranslateEnv.env * ProgramParseTree.PML2.BOMParseTree.code) 
		    -> BOM.lambda list

  end = struct

    structure BPT = ProgramParseTree.PML2.BOMParseTree
    structure PTVar = ProgramParseTree.Var
    structure E = TranslateEnv
    structure P = Prim
    structure BTy = BOMTy
    structure BV = BOM.Var
    structure ATbl = AtomTable
    structure VTbl = BV.Tbl

  (* table mapping primop names to prim_info *)
    structure MkPrim = MakePrimFn (
	type var = BOM.var
	type ty = BTy.ty
	val anyTy = BTy.T_Any
	val unitTy = BTy.unitTy
	val boolTy = BTy.boolTy
	val addrTy = BTy.T_Addr(BTy.T_Any)
	val rawTy = BTy.T_Raw)

    datatype prim_info = datatype MkPrim.prim_info

    val findPrim = MkPrim.findPrim o Atom.atom o PTVar.nameOf

  (* some type utilities *)
    val unwrapType = BOMTyUtil.unwrap
    val selectType = BOMTyUtil.select

    fun fail ss = Fail (String.concat ss)

    fun newTmp ty = BV.new("_t", ty)

    fun useCFun var = BOM.Var.addToCount(var, 1)

  (* globally accessible translation environment *)
    local
    val translateEnv : TranslateEnv.env option ref = ref NONE
    in

      fun getTranslateEnv () = (
	  case !translateEnv
	   of NONE => raise Fail "uninitialized translation environment"
	    | SOME env => env
          (* end case *))

    (* takes a translation environment and a function, and evaluates the function with the environment
     * set in the global variable above.
     *)
      fun withTranslateEnv env f = (
	  translateEnv := SOME env;
	  let val x = f()
	  in
	      translateEnv := NONE;
	      x
	  end)

    end
    fun cvtTy ty = TranslateTypes.cvtPrimTy (getTranslateEnv(), ty)
    fun cvtTys ty = TranslateTypes.cvtPrimTys (getTranslateEnv(), ty)

  (* find a data constructor that is defined in PML code *)
    fun findCon con = (case ModuleEnv.getValBind con
	   of SOME (ModuleEnv.Con dcon) =>
	        SOME(TranslateTypes.trDataCon(getTranslateEnv(), dcon))
	    | _ => NONE
          (* end case *))

    fun cvtVarPats vpats = let
	  fun f (BPT.P_VPMark {tree, span}) = f tree
	    | f (BPT.P_Wild ty) = let
		val ty = (case ty
		       of NONE => BTy.T_Any
			| SOME ty => cvtTy ty
		      (* end case *))
		val x' = BOM.Var.new("_wild", ty)
		in
		  x'
		end
	    | f (BPT.P_Var(x, ty)) = let
		val ty = cvtTy ty
		val x' = BOM.Var.new(PTVar.nameOf x, ty)
		in
		  E.insertBOMVar(x, x');
		  x'
		end
	  in
	    List.map f vpats
	  end

    fun lookupVar v = (
	case E.findBOMVar v
         of NONE => raise Fail(String.concat ["unknown BOM variable ", PTVar.nameOf v])
	  | SOME v => v
        (* end case *))

    datatype var_or_dcon
      = Var of BOM.Var.var
      | Con of E.con_bind

    fun lookupVarOrDCon v = (case E.findBOMVar v
	   of SOME v => Var v (* parameter-bound variable *)
	    | NONE => (case ModuleEnv.getValBind v
		 of SOME(ModuleEnv.Con c) => Con(TranslateTypes.trDataCon(getTranslateEnv(), c))
		  | NONE => raise Fail(String.concat ["unknown BOM variable ", PTVar.nameOf v])
		(* end case *))
	  (* end case *))

  (* convert a variable expression to either an ordinary variable or a nullary constructor *)
    fun cvtVar (x, k) = (case lookupVarOrDCon x
	   of Var x => k x
	    | Con(E.Const dc) => let
		val t = BOM.Var.new("con_"^PTVar.nameOf x, BOMTyCon.dconResTy dc)
		in
		  BOM.mkStmt([t], BOM.E_DCon(dc, []), k t)
		end
	    | Con(E.DCon _) => raise Fail "impossible"
	    | Con(E.ExnConst dc') => let
		val t = BV.new("exn_" ^ PTVar.nameOf x, BTy.exnTy)
		in
		  BOM.mkStmt([t], BOM.E_DCon(dc', []), k t)
		end
	    | Con(E.Lit lit) => let
		val t = BOM.Var.new(PTVar.nameOf x, #2 lit)
		in
		  BOM.mkStmt([t], BOM.E_Const lit, k t)
		end
	  (* end case *))

    fun cvtPat (BPT.P_PMark {tree, span}) = cvtPat tree
      | cvtPat (BPT.P_DCon(dc, xs)) = (case findCon dc
	   of SOME(E.Const dc) => BOM.P_DCon(dc, []) (* nullary constructor *)
	    | SOME(E.DCon(dc, _)) => let (* non-nullary constructor *)
		val xs = cvtVarPats xs
		in
		  BOM.P_DCon(dc, xs)
		end
	    | SOME(E.ExnConst _) => raise Fail "FIXME"
	    | SOME(E.Lit lit) => BOM.P_Const lit
	    | NONE => raise Fail(String.concat["unknown BOM nullary data constructor ", PTVar.nameOf dc])
	  (* end case *))
      | cvtPat (BPT.P_Const(const, ty)) = (BOM.P_Const(const, cvtTy ty))

    (* PML imports
     *
     * One property prevents us from simply importing the variable directly: hlops
     * expansion happens later, so getting the scope right would be difficult. Instead
     * we do something like closure conversion. i.e.,
     *
     *   define @f (params, ... / exh) : ty =
     *     stm1
     *     let x : ty' = pmlval p
     *     stm2
     *   ;
     *
     * ==> (rewrites to)
     *
     *   define @f (params, ..., p' : ty' / exh) : ty =
     *     stm1
     *     stm2
     *   ;
     * where p' is a fresh BOM variable. We must also pass the PML value at the call 
     * sites. This strategy unfortunately precludes rewrite rules for the HLOp.
     *
     *)
    local
	(* for each import, we record two variables: the fresh binding (p' in the example)
	 * and the actual binding (p in the example).
	 *)
	val pmlImports : (BOM.var * BOM.var) list ref = ref []
	fun findVar ([], v) = NONE
	  | findVar ((b, a) :: xs, v) = if BV.same(a, v)
              then SOME b
	      else findVar(xs, v)
    in
    fun addPMLImport actual = (case findVar (!pmlImports, actual)
            of NONE => let
		   val binding = BV.new(BV.nameOf actual, BV.typeOf actual)
                   in
		      pmlImports := (binding, actual) :: !pmlImports;
		      binding
	           end
	     | SOME binding => binding
           (* end case *))
    fun getPMLImports () = let
	    val imports = !pmlImports
            in
	       pmlImports := [];
	       imports
	    end
    end

    fun cvtExp (findCFun, e) = (case e
	   of BPT.E_Mark {tree, span} => cvtExp(findCFun, tree)
	    | BPT.E_Let(lhs, BPT.RHS_Mark{tree, span}, e'') => 
	        cvtExp(findCFun, BPT.E_Let(lhs, tree, e''))
	    | BPT.E_Let(lhs, BPT.RHS_SimpleExp (BPT.SE_Mark{tree, span}), e') => 
	        cvtExp(findCFun, BPT.E_Let(lhs, BPT.RHS_SimpleExp tree, e'))
	    | BPT.E_Let(lhs, rhs, body) => let
		val lhs' = cvtVarPats lhs
		val body' = cvtExp(findCFun, body)
		in
		  case rhs
		   of BPT.RHS_Mark _ => raise Fail "Mark" (* FIXME *)
		    | BPT.RHS_Exp e'' => BOM.mkLet(lhs', cvtExp(findCFun, e''), body')
		    | BPT.RHS_SimpleExp e'' => (case e''
			 of BPT.SE_Mark _ => raise Fail "Mark" (* FIXME *)
			  | BPT.SE_Var x => BOM.mkLet(lhs', cvtVar(x, fn x => BOM.mkRet[x]), body') 
			  | BPT.SE_Alloc args => let
			      val mut = (case BV.typeOf(hd lhs')
				      of BTy.T_Tuple(mut, _) => mut
				       | _ => false (* is this case an error? *)
				    (* end case *))
			      in
				cvtSimpleExps(findCFun, args,
				  fn xs => BOM.mkStmt(lhs', BOM.E_Alloc(BTy.T_Tuple(mut, List.map BV.typeOf xs), xs),
				    body'))
			      end
			  | BPT.SE_Wrap e =>
			      cvtSimpleExp (findCFun, e,
				fn x => BOM.mkStmt(lhs', BOM.E_Alloc(BOMTyUtil.wrap(BV.typeOf x), [x]), body'))
			  | BPT.SE_Select(i, arg) =>
			      cvtSimpleExp(findCFun, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Select(i, x), body'))
			  | BPT.SE_AddrOf(i, arg) =>
			      cvtSimpleExp(findCFun, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_AddrOf(i, x), body'))
			  | BPT.SE_Cast(ty, arg) =>
			      cvtSimpleExp(findCFun, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Cast(cvtTy ty, x), body'))
(* FIXME: we should check that lit and ty match! *)
	                  | BPT.SE_Const(lit, ty) => BOM.mkStmt(lhs', BOM.E_Const(lit, cvtTy ty), body')
			  | BPT.SE_MLString s => let
			      val t1 = BV.new("_data", BTy.T_Any)
			      val t2 = BV.new("_len", TranslateTypes.stringLenBOMTy())
			      val t3 = BV.new("_slit", TranslateTypes.stringBOMTy())
			      in
				BOM.mkStmts([
				    ([t1], BOM.E_Const(Literal.String s, BTy.T_Any)),
				    ([t2], BOM.E_Const(Literal.Int(IntInf.fromInt(size s)), TranslateTypes.stringLenBOMTy())),
				    ([t3], BOM.E_Alloc(TranslateTypes.stringBOMTy(), [t1, t2]))
				  ],
				BOM.mkLet(lhs', BOM.mkRet[t3], body'))
			      end
			  | BPT.SE_Unwrap arg =>
			      cvtSimpleExp(findCFun, arg, fn x =>
				BOM.mkStmt(lhs', BOM.unwrap x, body'))
			  | BPT.SE_Prim(p, args) =>
			      cvtSimpleExps(findCFun, args, fn xs => let
				val rhs = (case (findPrim p, xs)
				       of (NONE, _) => (case findCon p
					     of NONE => raise (fail(["unknown data constructor ", PTVar.toString p]))
					      | SOME(E.DCon (dc, _)) => BOM.E_DCon(dc, xs)
					    (* end case *))
					| (SOME(Prim0{con, ...}), []) => BOM.E_Prim con
					| (SOME(Prim1{mk, ...}), [x]) => BOM.E_Prim(mk x)
					| (SOME(Prim2{mk, ...}), [x, y]) => BOM.E_Prim(mk(x, y))
					| (SOME(Prim3{mk, ...}), [x, y, z]) => BOM.E_Prim(mk(x, y, z))
					| _ => raise (fail(["arity mismatch for primop ", PTVar.toString p]))
				      (* end case *))
				in
				  BOM.mkStmt(lhs', rhs, body')
				end)
			  | BPT.SE_HostVProc => BOM.mkStmt(lhs', BOM.E_HostVProc, body')
			  | BPT.SE_VPLoad(offset, vp) =>
			      cvtSimpleExp(findCFun, vp, fn vp =>
				BOM.mkStmt(lhs', BOM.E_VPLoad(offset, vp), body'))
			(* end case *))
		    | BPT.RHS_Update(i, arg, rhs) => 
			cvtSimpleExp(findCFun, arg, fn x =>
			  cvtSimpleExp(findCFun, rhs, fn y =>
			    BOM.mkStmt(lhs', BOM.E_Update(i, x, y), body')))
		    | BPT.RHS_VPStore(offset, vp, arg) =>
  		        cvtSimpleExp(findCFun, vp, fn vp =>
				cvtSimpleExp(findCFun, arg, fn x =>
				  BOM.mkStmt(lhs', BOM.E_VPStore(offset, vp, x), body')))
		    | BPT.RHS_Promote arg =>
			cvtSimpleExp(findCFun, arg, fn x => BOM.mkStmt(lhs', BOM.E_Promote x, body'))
		    | BPT.RHS_CCall(f, args) => let
			val cfun = findCFun f
		        in
		          useCFun cfun;
			  cvtSimpleExps(findCFun, args,
			    fn xs => BOM.mkStmt(lhs', BOM.E_CCall(cfun, xs), body'))
		        end
		    | BPT.RHS_PMLVar pmlVar => raise Fail "TODO: implement pmlvar"
(* TODO: pmlvars are unsafe within the bodies of hlops. Fix this problem before re-enabling the code below. *)
	          (*let
		       (* see comments above on pml imports for an explanation *)
			val v = (
			    case E.findBOMPMLVar pmlVar
			     of NONE => raise Fail (String.concat ["compiler bug: unbound PML variable ", PTVar.toString pmlVar])
			      | SOME v => v
				(* end case *))
		        in
			   BOM.mkLet(lhs', BOM.mkRet [v], body')
			end *)
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
		      let fun lp NONE = NONE
			    | lp (SOME(BPT.P_VPMark {tree, ...}, e)) = lp(SOME (tree, e))
			    | lp (SOME(BPT.P_Wild _, e)) = SOME(cvtExp(findCFun, e))
			    | lp (SOME(BPT.P_Var(x, _), e)) = (
			      E.insertBOMVar(x, arg);
			      SOME(cvtExp(findCFun, e)))
		      in lp dflt end))
		end
	    | BPT.E_Apply(f, args, rets) =>
		cvtSimpleExps(findCFun, args,
		  fn xs => cvtSimpleExps(findCFun, rets,
		    fn ys => BOM.mkApply(lookupVar(f), xs, ys)))
	    | BPT.E_Throw(k, args) =>
		cvtSimpleExps(findCFun, args, fn xs => BOM.mkThrow(lookupVar(k), xs))
	    | BPT.E_Return args =>
		cvtSimpleExps(findCFun, args, fn xs => BOM.mkRet xs)
	    | BPT.E_HLOpApply(hlop, args, rets) => (case E.findBOMHLOp hlop
		 of SOME hlop =>
		      cvtSimpleExps(findCFun, args,
			fn xs => cvtSimpleExps(findCFun, rets,
			  fn ys => BOM.mkHLOp(hlop, xs, ys)))
		  | NONE => raise (fail(["unknown high-level op ", PTVar.nameOf hlop]))
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
	    | BPT.SE_Var x => cvtVar(x, k)
	    | BPT.SE_Alloc args => 
	      (* NOTE: nested tuples are always immutable *)
		cvtSimpleExps(findCFun, args, fn xs => let
                   val mut = false
                   val tys = List.map BV.typeOf xs
	           val tmp = newTmp(BTy.T_Tuple(mut, tys))
		   in
		      BOM.mkStmt([tmp], BOM.E_Alloc(BTy.T_Tuple(mut, tys), xs), k tmp)
                   end)
	    | BPT.SE_Wrap e =>
		cvtSimpleExp (findCFun, e, fn x => let
		  val tmp = newTmp(BV.typeOf x)
		  in
		    BOM.mkStmt([tmp], BOM.E_Alloc(BOMTyUtil.wrap(BV.typeOf x), [x]), k tmp)
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
		val ty = cvtTy ty
		val tmp = newTmp ty
		in
(* FIXME: we should check that lit and ty match! *)
		  BOM.mkStmt([tmp], BOM.E_Const(lit, ty), k tmp)
		end
	    | BPT.SE_MLString s => let
		val t1 = BV.new("_data", BTy.T_Any)
		val t2 = BV.new("_len", TranslateTypes.stringLenBOMTy())
		val t3 = BV.new("_slit", TranslateTypes.stringBOMTy())
		in
		  BOM.mkStmts([
		      ([t1], BOM.E_Const(Literal.String s, BTy.T_Any)),
		      ([t2], BOM.E_Const(Literal.Int(IntInf.fromInt(size s)), TranslateTypes.stringLenBOMTy())),
		      ([t3], BOM.E_Alloc(TranslateTypes.stringBOMTy(), [t1, t2]))
		    ],
		  k t3)
		end
	    | BPT.SE_Cast(ty, e) =>
		cvtSimpleExp(findCFun, e, fn x => let
		  val ty = cvtTy ty
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
		       of (NONE, _) => (case findCon p
			     of SOME (E.DCon (dc, rep)) =>
				  (newTmp(BOMTyCon.dconResTy dc), BOM.E_DCon(dc, xs))
			      | _ => raise (fail(["unknown data constructor ", PTVar.toString p]))
			    (* end case *))
			| (SOME(Prim1{mk, resTy, ...}), [x]) =>
			    (newTmp resTy, BOM.E_Prim(mk x))
			| (SOME(Prim2{mk, resTy, ...}), [x, y]) =>
			    (newTmp resTy, BOM.E_Prim(mk(x, y)))
			| (SOME(Prim3{mk, resTy, ...}), [x, y, z]) =>
			    (newTmp resTy, BOM.E_Prim(mk(x, y, z)))
			| _ => raise (fail(["arity mismatch for primop ", PTVar.toString p]))
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

    fun findCFun name = (case E.findBOMCFun name
            of NONE => raise (fail(["Unknown C function ", PTVar.toString name]))
	     | SOME(cf as CFunctions.CFun{var, ...}) => var
            (* end case *))

    fun etaExpand (name, l) = let
	    val BOM.FB { f=f', params=params', exh=exh', ...} = BOMUtil.copyLambda l
	    val l' = BOM.FB{ f=f', params=params', exh=exh', body=BOM.mkHLOp(name, params', exh') }
	    in
	       BOM.mkFun([l'], BOM.mkRet [f'])
	    end

  (* create a list of temporary BOM variables *)
    fun mkVars baseName ts = let
	  fun build ([], _) = []
	    | build (t::ts, n) = let
		val x = baseName ^ Int.toString n
		in
		    BV.new (x, t) :: build (ts, n+1)
		end
	  in
	    build (ts, 0)
	  end

  (* explicitly cast a BOM expression *)
    fun mkCast (e, origTy, newTy) = 
	(* FIXME: relax this test to a "match" *)
	if BOMTyUtil.equal (origTy, newTy)
	then e (* cast is unnecessary *)
	else
	    let val x = BV.new ("x", origTy)
		val c = BV.new ("c", newTy)
	    in
		BOM.mkLet ([x], e,
		  BOM.mkStmt ([c], BOM.E_Cast (newTy, x),
		    BOM.mkRet [c]))
	    end

  (* check that a BOM type is compatible with the given PML type *)
    fun chkConstraintTy (x, bomTy, pmlTy) = 
	if (BOMTyUtil.equal(pmlTy, bomTy))
	   then ()
	else raise Fail (String.concatWith "\n" [
			 "incorrect BOM type for "^Var.nameOf x^": ",
			 "BOM type = "^BOMTyUtil.toString bomTy,
			 "PML type = "^BOMTyUtil.toString pmlTy
			])
  
    fun cvtRhs (env, x, pmlTy, rhs) = withTranslateEnv env (fn () => let
	  val x' = BOM.Var.new(Var.nameOf x, TranslateTypes.trScheme(env, pmlTy))
	  (* check that the RHS matches the constraining type *)
	  val pmlTy = TranslateTypes.trScheme(env, pmlTy)
          in
	    case rhs
	     of BPT.VarPrimVal v => let
		  val bomVar = lookupVar v
		  in
		    chkConstraintTy (x, BOM.Var.typeOf bomVar, pmlTy);
		    SOME (E.insertVar(env, x, x'), x', BOM.mkRet [bomVar])
		  end
	      | BPT.LambdaPrimVal fb => raise Fail "todo"
	      | BPT.HLOpPrimVal hlop => (
		  case E.findBOMHLOpDef hlop
		   of SOME{name, path, inline, def as BOM.FB{f, ...}, externs, pmlImports} => let
		      (* to synthesize polymorphism for the return type, we eta expand and cast to the instantiated type. *)
			fun mkFB instTy = let
			    val BOMTy.T_Fun(paramTys, exhTys, [retTy]) = BOM.Var.typeOf f
			    val params = mkVars "arg" paramTys
			    val exh = mkVars "exh" exhTys
			    val h = BOM.mkHLOp(name, params, exh)
			    val f = BOM.Var.new(BOM.Var.nameOf f, BTy.T_Fun(paramTys, exhTys, [instTy]))
			    in
			      BOM.FB{f=f, params=params, exh=exh, body=mkCast(h, retTy, instTy)}
			    end
			in
			  chkConstraintTy (x, BOM.Var.typeOf f, pmlTy);
			  SOME (E.insertFun(env, x, mkFB), x', etaExpand(name, def))
			end
		  (* end case *))
	    (* end case *)
          end)

    fun tyOfPat (BPT.P_VPMark {tree, span}) = tyOfPat tree
      | tyOfPat (BPT.P_Wild NONE) = BTy.T_Any
      | tyOfPat (BPT.P_Wild(SOME ty)) = cvtTy ty
      | tyOfPat (BPT.P_Var(_, ty)) = cvtTy ty

    (* this is the first pass, which binds C-function prototypes, adds defined types to the translation
     * environment, and adds HLOp signatures to the HLOp environment.
     *)
    fun insDef importEnv (BPT.D_Mark {span, tree}) = insDef importEnv tree
      | insDef importEnv (BPT.D_Extern(CFunctions.CFun{var, name, retTy, argTys, attrs, varArg})) = (
	case E.findBOMCFun var
	 (* FIXME: we probably should check that the existing prototype matches this one! *)
	 of SOME cfun => () (* already defined, so do nothing *)
	  | NONE => let
		val ty = BTy.T_CFun(CFunctions.CProto(retTy, argTys, attrs))
		val cf = BOM.mkCFun{
			 var = BOM.Var.new(PTVar.nameOf var, ty),
			 name = name, retTy = retTy, argTys = argTys, attrs = attrs, varArg = varArg
			 }
	        in
		   ATbl.insert importEnv (Atom.atom (PTVar.nameOf var), cf);
		   E.insertBOMCFun(importEnv, var, cf)
	        end
         (* end case *))
      | insDef importEnv (BPT.D_TypeDef(id, ty)) = E.insertBOMTyDef(id, cvtTy ty)
      | insDef importEnv (BPT.D_Define(_, name, params, exh, retTy, _)) = let
	(* create a high-level operator *)
	    val (retTy, attrs) = (case retTy
				   of NONE => ([], [HLOp.NORETURN])
				    | SOME tys => (cvtTys tys, [])
				 (* end case *))		      
	    val paramTys = List.map (fn p => HLOp.PARAM(tyOfPat p)) params
	    val exhTys = List.map tyOfPat exh
	    val hlop = HLOp.new (
		          Atom.atom (PTVar.nameOf name),
			  {params=paramTys, exh=exhTys, results=retTy},
			  attrs)
	    in 
	        E.insertBOMHLOp(name, hlop)
	    end

  (* this is the second pass, which converts actual HLOp definitions to BOM lambdas *)
    fun cvtDefs importEnv [] = []
      | cvtDefs importEnv (BPT.D_Mark {span, tree}::defs) = cvtDefs importEnv (tree::defs)
      | cvtDefs importEnv (BPT.D_Define(inline, name, params, exh, retTy, SOME e)::defs) = let
	    val hlop = Option.valOf(E.findBOMHLOp name)
	    val retTy = (case retTy of NONE => [] | SOME tys => tys)
	    val cfuns = VTbl.mkTable (16, Fail "cfun table")
	    fun findCFun' name = let
		    val var = findCFun name
		    in
		      (* increment the count of references to the C function *)
		      case VTbl.find cfuns var
		       of NONE => VTbl.insert cfuns (var, 1)
			| SOME n => VTbl.insert cfuns (var, n+1)
		      (* end case *);
		      var
                    end
	    val doBody = cvtLambda (findCFun', (name, params, exh, retTy, e), BTy.T_Fun)
	    val lambda = doBody ()
	    val def = {
		   name = hlop,
		   path = BindingEnv.getHLOpPath name,
		   inline = inline,
		   def = lambda,
		   pmlImports = getPMLImports(),
		   externs = VTbl.listItemsi cfuns
	        }
	    in
	       E.insertBOMHLOpDef(name, def);
	       def :: cvtDefs importEnv defs
	    end
      | cvtDefs importEnv (_::defs) = cvtDefs importEnv defs

    fun cvtCode (env, code) = withTranslateEnv env (fn () => let
	    val importEnv = E.getImportEnv env
	    val _ = List.app (insDef importEnv) code
	    val defs = cvtDefs importEnv code
	    in
	       HLOpEnv.addDefs defs;
	       List.map #def (List.filter (not o #inline) defs)
            end)

  end

