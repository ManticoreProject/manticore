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

    val errStrm = ref(Error.mkErrStream "<bogus>")
    fun error (span, msg) = Error.errorAt (!errStrm, span, msg)
  (* check for errors and report them if there are any *)
    fun checkForErrors errStrm = (
	  Error.report (TextIO.stdErr, errStrm);
	  if Error.anyErrors errStrm
	    then OS.Process.exit OS.Process.failure
	    else ())

  (* some type utilities *)
    val unwrapType = BOMTyUtil.unwrap
    val selectType = BOMTyUtil.select

    fun fail ss = Fail (String.concat ss)

    fun newTmp ty = BV.new("_t", ty)

    fun useCFun var = BOM.Var.addToCount(var, 1)

    fun findCFun name = (case E.findBOMCFun name
            of NONE => raise (fail(["Unknown C function ", PTVar.toString name]))
	     | SOME(cf as CFunctions.CFun{var, ...}) => var
            (* end case *))

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

    fun mkPrim (p, xs) = 
	(case (findPrim p, xs)
	  of (NONE, _) => 
	     (case findCon p
	       of NONE => raise (fail(["unknown data constructor ", PTVar.toString p]))
		| SOME (E.DCon (dc, _)) => BOM.E_DCon(dc, xs)
	     (* end case *))
	   | (SOME(Prim0{con, ...}), []) => BOM.E_Prim con
	   | (SOME(Prim1{mk, ...}), [x]) => BOM.E_Prim(mk x)
	   | (SOME(Prim2{mk, ...}), [x, y]) => BOM.E_Prim(mk(x, y))
	   | (SOME(Prim3{mk, ...}), [x, y, z]) => BOM.E_Prim(mk(x, y, z))
	   | _ => raise (fail(["arity mismatch for primop ", PTVar.toString p]))
	(* end case *))

    fun tyOfPrim p = 
	(case findPrim p
	  of NONE => 
	     (case findCon p
	       of SOME (E.DCon (dc, rep)) => BOMTyCon.dconResTy dc
		| _ => raise (fail(["unknown data constructor ", PTVar.toString p]))
	     (* end case *))
	   | SOME(Prim1{mk, resTy, ...}) => resTy
	   | SOME(Prim2{mk, resTy, ...}) => resTy
	   | SOME(Prim3{mk, resTy, ...}) => resTy
	   | _ => raise (fail(["arity mismatch for primop ", PTVar.toString p]))
	(* end case *))

  (* generate a dynamic check that the given variable is a valid pointer to the global heap *)
  (* the argument pointsToHeapObject must be false whenever the pointer might point into the middle
   * of a heap object. *)
    fun checkGlobalPtr (loc, v, pointsToHeapObject) =
	let val self = newTmp BTy.T_VProc
	    val t = newTmp BTy.T_Any
	    val locS = Error.locToString(Error.location(!errStrm, loc))
	    val checkCFun = if pointsToHeapObject then "CheckGlobalPtr" else "CheckGlobalAddr"
	in
	    [([self], BOM.E_HostVProc),
	     ([t], BOM.E_Const(Literal.String (locS ^ " at " ^ BOM.Var.toString v), BTy.T_Any)),
	     ([], BOM.E_CCall(findCFun(BasisEnv.getCFunFromBasis [checkCFun]), [self, v, t]))]
	end

    fun cvtPrim (loc, lhs, p, xs, body) = 
	if not(Controls.get BasicControl.debug) then 	
	    BOM.mkStmts ([(lhs, mkPrim(p, xs))], body)
	else
	    (case mkPrim(p, xs)
	      of prim as BOM.E_Prim(Prim.CAS(x, new, old)) =>
		 BOM.mkStmts (checkGlobalPtr(loc, x, false) @
			      checkGlobalPtr(loc, new, false) @
			      checkGlobalPtr(loc, old, false) @
		              [(lhs, prim)],
			      body)
	       | prim as BOM.E_Prim(Prim.BCAS(x, new, old)) =>
		 BOM.mkStmts (checkGlobalPtr(loc, x, false) @
			      checkGlobalPtr(loc, new, false) @
			      checkGlobalPtr(loc, old, false) @
		              [(lhs, prim)],
			      body)
	       | prim => BOM.mkStmts ([(lhs, mkPrim(p, xs))], body)
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

    fun cvtExp (loc, findCFun, e) = let
	  fun cvt (loc, e) = (case e
	     of BPT.E_Mark {tree, span} => cvt(loc, tree)
	      | BPT.E_Let(lhs, BPT.RHS_Mark{tree, span}, e'') => 
		  cvt(loc, BPT.E_Let(lhs, tree, e''))
	      | BPT.E_Let(lhs, BPT.RHS_SimpleExp (BPT.SE_Mark{tree, span}), e') => 
		  cvt(loc, BPT.E_Let(lhs, BPT.RHS_SimpleExp tree, e'))
	      | BPT.E_Let(lhs, rhs, body) => let
		  val lhs' = cvtVarPats lhs
		  val body' = cvt(loc, body)
		  in
		    case rhs
		     of BPT.RHS_Mark _ => raise Fail "Mark" (* FIXME *)
		      | BPT.RHS_Exp e'' => BOM.mkLet(lhs', cvt(loc, e''), body')
		      | BPT.RHS_SimpleExp e'' => (case e''
			   of BPT.SE_Mark _ => raise Fail "Mark" (* FIXME *)
			    | BPT.SE_Var x => BOM.mkLet(lhs', cvtVar(x, fn x => BOM.mkRet[x]), body') 
			    | BPT.SE_Alloc args => let
				val ty = BV.typeOf(hd lhs')
				in
				  cvtSimpleExps(loc, findCFun, args,
				    fn xs => BOM.mkStmt(lhs', BOM.E_Alloc(ty, xs),
				      body'))
				end
			    | BPT.SE_Wrap e =>
				cvtSimpleExp (loc, findCFun, e,
				  fn x => BOM.mkStmt(lhs', BOM.E_Alloc(BOMTyUtil.wrap(BV.typeOf x), [x]), body'))
			    | BPT.SE_Select(i, arg) =>
				cvtSimpleExp(loc, findCFun, arg, fn x =>
				  BOM.mkStmt(lhs', BOM.E_Select(i, x), body'))
			    | BPT.SE_AddrOf(i, arg) =>
				cvtSimpleExp(loc, findCFun, arg, fn x =>
				  BOM.mkStmt(lhs', BOM.E_AddrOf(i, x), body'))
			    | BPT.SE_Cast(ty, arg) =>
				cvtSimpleExp(loc, findCFun, arg, fn x =>
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
				cvtSimpleExp(loc, findCFun, arg, fn x =>
				  BOM.mkStmt(lhs', BOM.unwrap x, body'))
			    | BPT.SE_Prim(p, args) =>
				cvtSimpleExps(loc, findCFun, args, fn xs => cvtPrim(loc, lhs', p, xs, body'))
			    | BPT.SE_HostVProc => BOM.mkStmt(lhs', BOM.E_HostVProc, body')
			    | BPT.SE_VPLoad(offset, vp) =>
				cvtSimpleExp(loc, findCFun, vp, fn vp =>
				  BOM.mkStmt(lhs', BOM.E_VPLoad(offset, vp), body'))
			  (* end case *))
		      | BPT.RHS_Update(i, arg, rhs) => 
			cvtSimpleExp(loc, findCFun, arg, fn x =>
			  cvtSimpleExp(loc, findCFun, rhs, fn y =>
			    if not(Controls.get BasicControl.debug) then 
				BOM.mkStmt(lhs', BOM.E_Update(i, x, y), body')
			    else
				BOM.mkStmts(checkGlobalPtr(loc, x, true) @
					    checkGlobalPtr(loc, y, false) @
					    [(lhs', BOM.E_Update(i, x, y))],
					    body')))
		      | BPT.RHS_VPStore(offset, vp, arg) =>
			  cvtSimpleExp(loc, findCFun, vp, fn vp =>
				  cvtSimpleExp(loc, findCFun, arg, fn x =>
				    BOM.mkStmt(lhs', BOM.E_VPStore(offset, vp, x), body')))
		      | BPT.RHS_Promote arg =>
			  cvtSimpleExp(loc, findCFun, arg, fn x => BOM.mkStmt(lhs', BOM.E_Promote x, body'))
		      | BPT.RHS_CCall(f, args) => let
			  val cfun = findCFun f
			  in
			    useCFun cfun;
			    cvtSimpleExps(loc, findCFun, args,
			      fn xs => BOM.mkStmt(lhs', BOM.E_CCall(cfun, xs), body'))
			  end
		  end 
	      | BPT.E_Fun(fbs, e) => let
		  fun f (fb, cvtBodies) = let
			  val cvt = cvtLambda (loc, findCFun, fb, BTy.T_Fun)
			  in
			    cvt::cvtBodies
			  end
		  val cvtBodies = List.foldl f ([]) fbs
		  in		 
		    BOM.mkFun(
		      List.foldl (fn (cvt, fbs') => cvt () :: fbs') [] cvtBodies,
		      cvt(loc, e))
		  end
	      | BPT.E_Cont(fb, e) => let
		(* NOTE: continuations are permitted to be recursive *)
		  val cvtBody = cvtLambda(loc, findCFun, fb, fn (argTys, _, _) => BTy.T_Cont argTys)
		  in
		    BOM.mkCont(cvtBody(), cvt(loc, e))
		  end
	      | BPT.E_If(e1, e2, e3) =>
		  cvtSimpleExp(loc, findCFun, e1, fn x => BOM.mkIf(x, cvt(loc, e2), cvt(loc, e3)))
	      | BPT.E_Case(arg, cases, dflt) => let
		  fun doCase (pat, exp) = let
			val (pat') = cvtPat(pat)
			in
			  (pat', cvt(loc, exp))
			end		
		  in
		    cvtSimpleExp(loc, findCFun, arg, fn arg =>
		      BOM.mkCase(
			arg, 
			List.map doCase cases,
			let fun lp NONE = NONE
			      | lp (SOME(BPT.P_VPMark {tree, ...}, e)) = lp(SOME (tree, e))
			      | lp (SOME(BPT.P_Wild _, e)) = SOME(cvt(loc, e))
			      | lp (SOME(BPT.P_Var(x, _), e)) = (
				E.insertBOMVar(x, arg);
				SOME(cvt(loc, e)))
			in lp dflt end))
		  end
	      | BPT.E_Apply(f, args, rets) =>
		  cvtSimpleExps(loc, findCFun, args,
		    fn xs => cvtSimpleExps(loc, findCFun, rets,
		      fn ys => BOM.mkApply(lookupVar(f), xs, ys)))
	      | BPT.E_Throw(k, args) =>
		  cvtSimpleExps(loc, findCFun, args, fn xs => BOM.mkThrow(lookupVar(k), xs))
	      | BPT.E_Return args =>
		  cvtSimpleExps(loc, findCFun, args, fn xs => BOM.mkRet xs)
	      | BPT.E_HLOpApply(hlop, args, rets) => (case E.findBOMHLOp hlop
		   of SOME hlop =>
			cvtSimpleExps(loc, findCFun, args,
			  fn xs => cvtSimpleExps(loc, findCFun, rets,
			    fn ys => BOM.mkHLOp(hlop, xs, ys)))
		    | NONE => raise (fail(["unknown high-level op ", PTVar.nameOf hlop]))
		  (* end case *))
	    (* end case *))
      in
	  cvt(loc, e)
      end

    and cvtLambda (loc, findCFun, (f, params, rets, tys, e), tyCon) = let
	  val params = cvtVarPats params
	  val rets = cvtVarPats rets
	  val fnTy = tyCon(List.map BV.typeOf params, List.map BV.typeOf rets, cvtTys tys)
	  val f' = BOM.Var.new(PTVar.nameOf f, fnTy)
	  val _ = E.insertBOMVar(f, f')
	  fun doBody () = BOM.FB{
		  f = f', params = params, exh = rets, body = cvtExp(loc, findCFun, e)
		}
	  in
	    E.insertBOMVar(f, f');
	    doBody
	  end

    and cvtSimpleExp (loc, findCFun, e, k : BOM.var -> BOM.exp) = let
	  fun cvt (loc, e, k) = (case e
	     of BPT.SE_Mark {tree, span} => cvt(loc, tree, k)
	      | BPT.SE_Var x => cvtVar(x, k)
	      | BPT.SE_Alloc args => 
		(* NOTE: nested tuples are always immutable *)
		  cvtSimpleExps(loc, findCFun, args, fn xs => let
		     val mut = false
		     val tys = List.map BV.typeOf xs
		     val tmp = newTmp(BTy.T_Tuple(mut, tys))
		     in
			BOM.mkStmt([tmp], BOM.E_Alloc(BTy.T_Tuple(mut, tys), xs), k tmp)
		     end)
	      | BPT.SE_Wrap e =>
		  cvt (loc, e, fn x => let
		    val tmp = newTmp(BV.typeOf x)
		    in
		      BOM.mkStmt([tmp], BOM.E_Alloc(BOMTyUtil.wrap(BV.typeOf x), [x]), k tmp)
		    end)
	      | BPT.SE_Select(i, e) =>
		  cvt(loc, e, fn x => let
		    val tmp = newTmp(selectType(BOM.Var.typeOf x, i))
		    in
		      BOM.mkStmt([tmp], BOM.E_Select(i, x), k tmp)
		    end)
	      | BPT.SE_AddrOf(i, e) =>
		  cvt(loc, e, fn x => let
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
		  cvt(loc, e, fn x => let
		    val ty = cvtTy ty
		    val tmp = newTmp ty
		    in
		      BOM.mkStmt([tmp], BOM.E_Cast(ty, x), k tmp)
		    end)
	      | BPT.SE_Unwrap e =>
		  cvt(loc, e, fn x => let
		    val tmp = newTmp(unwrapType(BOM.Var.typeOf x))
		    in
		      BOM.mkStmt([tmp], BOM.unwrap x, k tmp)
		    end)
	      | BPT.SE_Prim(p, args) => 
		  cvtSimpleExps(loc, findCFun, args, fn xs => 
		    let val lhs' = newTmp(tyOfPrim p)
		    in
			cvtPrim(loc, [lhs'], p, xs, k lhs')
		    end)
	      | BPT.SE_HostVProc =>  let
		    val tmp = newTmp BTy.T_VProc
		    in
		      BOM.mkStmt([tmp], BOM.E_HostVProc, k tmp)
		    end
	      | BPT.SE_VPLoad(offset, vp) =>
		  cvt(loc, vp, fn vp => let
		    val tmp = newTmp(BTy.T_Any)
		    in
		      BOM.mkStmt([tmp], BOM.E_VPLoad(offset, vp), k tmp)
		    end)
	    (* end case *))
      in
	cvt(loc, e, k)
      end

    and cvtSimpleExps (loc, findCFun, exps, k) = let
	  fun cvt ([], tmps) = k(List.rev tmps)
	    | cvt (e::es, tmps) = cvtSimpleExp (loc, findCFun, e, fn t => cvt(es, t::tmps))
	  in
	    cvt (exps, [])
	  end

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

  (* lifting utility for inline BOM functions *)
(* TODO: finish this implementation *)
    local 

  (* create an expressin that selects the ith element from the tuple x *)
    fun select (i, x) = raise Fail "todo"

  (* lift the parameters of a BOM function to PML *)
    fun liftPrimParams (params, pmlParamTys, bomParamTys) = let
        (* lift a parameter based on the type *)
	  fun liftParam (param, pmlParamTy, bomParamTy) = (
	        case (pmlParamTy, bomParamTy)
		 of (BOMTy.T_Tuple(_, [BOMTy.T_Raw BOMTy.T_Int]), BOMTy.T_Int) => select(0, param)
		  | _ => raise Fail "todo"
            	(* end case *))
          in
	    case (params, pmlParamTys, bomParamTys)
	     of ([param], [pmlParamTy], [bomParamTy]) => liftParam(param, pmlParamTy, bomParamTy)
	      | (params, pmlParamTys, bomParamTys) => raise Fail "todo"
             (* end case *)
          end

    in
  (* utility for lifting an inline BOM function to a PML function, i.e.,

      define @length (arr : array / exh : exh) : int =
	...
      ;

      val length : array -> int = _lift_prim(@length)

      ===>

      define @length (arr : array / exh : exh) : int =
	...
      ;

      define @length-w (arr : array / exh : exh) : ml_int =
	let len : int = @length(arr / exh)
	return(alloc(len))
      ;

      val length : array -> int = _lift_prim(@length-w)
   *)
    fun liftPrim (name, pmlTy, params, exh, bomParamTys, bomExnTys, bomRetTy) = let
	  val pmlFunTy as BOMTy.T_Fun(pmlParamTys, pmlExnTys, [pmlRetTy]) = TranslateTypes.tr(getTranslateEnv(), pmlTy)
	  val params' = List.map BV.copy params
	  val exh' = List.map BV.copy exh
	  val wrapper = BV.new(BV.nameOf name^"-wrapper", pmlFunTy)
	  val wrapperBody = let
	        val args = liftPrimParams(params, pmlParamTys, bomParamTys)
	        in
	          raise Fail "todo"
	        end
          in
	    BOM.FB{f=wrapper, params=params', exh=exh', body=wrapperBody}
	  end
    end  (* local *)
  
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
		   of SOME{name, path, inline, def as BOM.FB{f, ...}, externs} => let
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
		    | NONE => raise Fail ("TranslatePrim.cvtRhs: compiler bug, missing hlop "^Var.toString x)
		  (* end case *))
	    (* end case *)
          end)

    fun tyOfPat (BPT.P_VPMark {tree, span}) = tyOfPat tree
      | tyOfPat (BPT.P_Wild NONE) = BTy.T_Any
      | tyOfPat (BPT.P_Wild(SOME ty)) = cvtTy ty
      | tyOfPat (BPT.P_Var(_, ty)) = cvtTy ty

  (* resolve a PML identifier to its BOM binding occurrence *)
    fun lookupPMLId pmlId = (
	  case ModuleEnv.getValBind pmlId
	   of SOME(ModuleEnv.Var pmlVar) => (
	      case TranslateEnv.lookupVar(getTranslateEnv(), pmlVar)
	       of TranslateEnv.Var bomVar => bomVar
		| _ => raise Fail "compiler bug: cannot find pmlId in translate environment"
	      (* end case *))
	    | _ => raise Fail "compiler bug: cannot find pmlId in module environment"
          (* end case *))

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
	    val paramTys = List.map (HLOp.PARAM o tyOfPat) params
	    val exhTys = List.map tyOfPat exh
	    val hlop = HLOp.new (
		          Atom.atom (PTVar.nameOf name),
			  {params=paramTys, exh=exhTys, results=retTy},
			  attrs)
	    in 
	        E.insertBOMHLOp(name, hlop)
	    end
      | insDef importEnv (BPT.D_ImportML(inline, hlopId, pmlId)) = let
	  val fTy as BTy.T_Fun([paramTy], [exhTy], [retTy]) = BV.typeOf(lookupPMLId pmlId)
	  val hlop = HLOp.new (
		Atom.atom (PTVar.nameOf hlopId),
		{params=[HLOp.PARAM paramTy], exh=[exhTy], results=[retTy]},
		[])
	  in
	    E.insertBOMHLOp(hlopId, hlop)
	  end

  (* this is the second pass, which converts actual HLOp definitions to BOM lambdas *)
    fun cvtDefs importEnv [] = []
      | cvtDefs importEnv (BPT.D_Mark {span, tree}::defs) = cvtDefs importEnv (tree::defs)
      | cvtDefs importEnv (BPT.D_Define(inline, hlopId, params, exh, retTy, SOME e)::defs) = let
	    val _ = (case PTVar.getErrorStream hlopId
		      of NONE => ()
		       | SOME strm => errStrm := strm)
	    val hlop = Option.valOf(E.findBOMHLOp hlopId)
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
	    val doBody = cvtLambda ((0,0), findCFun', (hlopId, params, exh, retTy, e), BTy.T_Fun)
	    val lambda = doBody ()
	    val def = {
		   name = hlop,
		   path = BindingEnv.getHLOpPath hlopId,
		   inline = inline,
		   def = lambda,
		   externs = VTbl.listItemsi cfuns
	        }
	    val _ = E.insertBOMHLOpDef(hlopId, def)
	    val defs = def :: cvtDefs importEnv defs
	    in
	       checkForErrors(!errStrm);
	       defs
	    end
      | cvtDefs importEnv (BPT.D_ImportML(inline, hlopId, pmlId)::defs) = let
	  val hlop = Option.valOf(E.findBOMHLOp hlopId)
	  val bomVar = lookupPMLId pmlId
	  val fTy as BTy.T_Fun([paramTy], [exhTy], [retTy]) = BV.typeOf bomVar
	  val f = BV.new(PTVar.nameOf hlopId, fTy)
	  val param = BV.new("_arg", paramTy)
	  val exh = BV.new("_exh", exhTy)
	  val body = BOM.mkApply(bomVar, [param], [exh])
	  val def = {
		name = hlop,
		path = BindingEnv.getHLOpPath hlopId,
		inline = inline,
		def = BOM.mkLambda{f=f, params=[param], exh=[exh], body=body},
		externs = []
	      }
	  in
	    E.insertBOMHLOpDef(hlopId, def);
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

