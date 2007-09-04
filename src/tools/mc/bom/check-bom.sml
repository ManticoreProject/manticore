(* check-bom.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check various invariants in BOM representation.
 *)

structure CheckBOM : sig

    val check : string * BOM.module -> bool

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure VTbl = BV.Tbl
    structure BTy = BOMTy

    val v2s = BV.toString
    fun vl2s xs = String.concat["(", String.concatWith "," (List.map v2s xs), ")"]

(**** Fluet version ****
    structure Ty = BOMTy
    structure V = B.Var
    structure VSet = V.Set

    fun addFB (B.FB{f, ...}, env) = VSet.add(env, f)

    fun addVars (env, xs) = VSet.addList(env, xs)

    fun addPat (env, B.P_DCon (_, xs)) = addVars (env, xs)
      | addPat (env, B.P_Const _) = env

    fun check (s : string, B.MODULE{name,externs,body}) = let
	  val anyErr = ref false
	  fun err msg = (
		anyErr := true;
		print(concat("Error: " :: msg));
		print "\n")
	  fun chkVar (env, x, cxt) = if VSet.member(env, x)
		then ()
		else err["variable ", v2s x, " is unbound ", cxt]
	  fun chkVars (env, xs, cxt) = List.app (fn x => chkVar(env, x, cxt)) xs
	(* match the parameter types against arguments *)
	  fun checkArgs (paramTys, args, cxt) = let
		fun chk ([], []) = ()
		  | chk (l, []) = err[Int.toString(length l), " too few arguments in ", cxt]
		  | chk ([], l) = err[Int.toString(length l), " too many arguments in ", cxt]
		  | chk (ty::tys, x::xs) = (
		      if (Ty.match(V.typeOf x, ty))
			then ()
			else err[
			    "type mismatch in ", cxt, "\n  expected  ", Ty.toString ty,
			    "\n  but found ", v2s x, ":", Ty.toString(V.typeOf x)
			  ];
		      chk(tys, xs))
		in
		  chk (paramTys, args)
		end
	  fun chkExp (env, B.E_Pt(_,e)) = (case e
                 of B.E_Let(lhs, rhs, e) => (
                      chkExp (addVars(env, lhs), e))
		  | B.E_Stmt(lhs, rhs, e) => (
		      chkRHS(env, lhs, rhs);
		      chkExp (addVars(env, lhs), e))
		  | B.E_Fun(fbs, e) => let
		      val env = List.foldl addFB env fbs
		      in
			List.app (fn fb => chkFB(env, fb)) fbs;
			chkExp(env, e)
		      end
		  | B.E_Cont(fb, e) => let
		      val env = addFB (fb, env)
		      in
			chkFB(env, fb); chkExp(env, e)
		      end
		  | B.E_If(x, e1, e2) => (chkVar(env, x, "If"); chkExp(env, e1); chkExp(env, e2))
		  | B.E_Case(x, cases, dflt) => (
		      chkVar(env, x, "Switch");
		      List.app (fn (p, e) => chkExp (addPat (env, p), e)) cases;
		      Option.app (fn e => chkExp (env, e)) dflt)
		  | B.E_Apply(f, args, exhs) => (
		      chkVar (env, f, "Apply");
		      case V.typeOf f
		       of Ty.T_Fun(argTys, exhTys, _) => (
			    chkVars (env, args, "Apply args");
			    chkVars (env, exhs, "Apply exhs");
			    checkArgs (argTys, args, concat["Apply ", v2s f, " args"]);
			    checkArgs (exhTys, exhs, concat["Apply ", v2s f, " exhs"]))
			| ty => err[v2s f, ":", Ty.toString ty, " is not a function"]
		      (* end case *))
		  | B.E_Throw(k, args) => (
		      chkVar (env, k, "Throw");
		      case V.typeOf k
		       of Ty.T_Cont(argTys) => (
			    chkVars (env, args, "Throw args");
			    checkArgs (argTys, args, "Throw " ^ v2s k))
			| ty => err[v2s k, ":", Ty.toString ty, " is not a continuation"]
		      (* end case *))
                  | B.E_Ret(args) => (
                      chkVars (env, args, "Return"))
                  | B.E_HLOp(hlop as HLOp.HLOp {sign as {params as argTys, exh as exhTys, 
                                                         results as resTys}, ...},
                             args,exhs) => (
                      chkVars (env, args, "HLOP args");
                      chkVars (env, exhs, "HLOP exhs");
                      let 
                      val argTys = List.map (fn HLOp.PARAM ty => ty 
                                              | _ => raise Fail "B.E_HLop: argTys") argTys 
                      in
                      checkArgs (argTys, args, "HLOP " ^ (HLOp.toString hlop))
                      end;
		      checkArgs (exhTys, exhs, "HLOP " ^ (HLOp.toString hlop))
		(* end case *)))
	  and chkRHS (env, lhs, rhs) = (case (List.map V.typeOf lhs, rhs)
		 of ([ty], B.E_Const(_, ty')) => (
		      if Ty.equal(ty', ty)
			then ()
			else err["type mismatch in Const"])
		  | ([ty], B.E_Cast(ty', x)) => (
		      chkVar (env, x, "Cast");
		      if Ty.match(ty', ty) andalso Ty.validCast(V.typeOf x, ty')
			then ()
			else err["type mismatch in Cast"])
		  | ([ty], B.E_Select(i, x)) => (
                      chkVar(env, x, "Select");
                      case V.typeOf x
                       of Ty.T_Tuple(_, tys) => if Ty.equal(ty, List.nth (tys, i))
                                                  then ()
                                                  else err["type mismatch in Select"]
			| ty => err[v2s x, ":", Ty.toString ty, " is not a tuple"]
		      (* end case *))
		  | ([], B.E_Update(i, x, y)) => (
                      chkVar(env, x, "Update");
                      chkVar(env, y, "Update");
                      case V.typeOf x
                       of Ty.T_Tuple(true, tys) => if Ty.equal(V.typeOf y, List.nth (tys, i))
                                                     then ()
                                                     else err["type mismatch in Update"]
			| ty => err[v2s x, ":", Ty.toString ty, " is not a mutable tuple"]
		      (* end case *))
		  | ([ty], B.E_AddrOf(i, x)) => (
                      chkVar(env, x, "AddrOf");
                      case V.typeOf x
                       of Ty.T_Tuple(_, tys) => if Ty.equal(ty, Ty.T_Addr(List.nth (tys, i)))
                                                  then ()
                                                  else err["type mismatch in AddrOf"]
			| ty => err[v2s x, ":", Ty.toString ty, " is not a tuple"]
		      (* end case *))
		  | ([ty], B.E_Alloc (_, xs)) => (
                      chkVars(env, xs, "Alloc");
                      if Ty.equal(ty, Ty.T_Tuple(true, List.map V.typeOf xs))
                         orelse Ty.equal(ty, Ty.T_Tuple(false, List.map V.typeOf xs))
                        then ()
                        else err["type mismatch in Alloc: ", vl2s xs])
		  | ([ty], B.E_Prim p) => chkVars(env, PrimUtil.varsOf p, PrimUtil.nameOf p)
                  | ([ty], B.E_DCon (dcon, args)) => chkVars(env, args, BOMTyCon.dconName dcon)
		  | ([ty], B.E_CCall(cf, args)) => (
		      chkVar(env, cf, "CCall"); chkVars(env, args, "CCall args"))
		  | ([], B.E_CCall(cf, args)) => (
		      chkVar(env, cf, "CCall"); chkVars(env, args, "CCall args"))
		  | ([ty], B.E_HostVProc) => ()
		  | ([ty], B.E_VPLoad(n, vp)) => chkVar(env, vp, "VPLoad")
		  | ([], B.E_VPStore(n, vp, x)) => (
		      chkVar(env, vp, "VPStore"); chkVar(env, x, "VPStore"))
		  | _ => err["bogus rhs for ", vl2s lhs]
		(* end case *))
	  and chkFB (env, fb as B.FB{f, params, exh, body}) = (let
		fun chk ([], []) = ()
		  | chk (_, []) = err["too few parameters in ", v2s f]
		  | chk ([], _) = err["too many parameters in ", v2s f]
		  | chk (ty::tys, x::xs) = (
		      if (Ty.equal(V.typeOf x, ty))
			then ()
			else err[
			    "type mismatch in ", v2s f, "\n  expected  ", Ty.toString ty,
			    "\n  but found ", v2s x, ":", Ty.toString(V.typeOf x)
			  ];
		      chk(tys, xs))
                in
		case V.typeOf f
		 of Ty.T_Fun(argTys, exhTys, retTys) => (
			chk(argTys, params);
			chk(exhTys, exh))
                  | Ty.T_Cont(argTys) => (
			chk(argTys, params);
			chk([], exh))
		  | ty => err[
			"expected function/continuation type for ",
			v2s f, ":", Ty.toString(V.typeOf f)
		      ]
		(* end case *);
		chkExp (addVars(addVars(env, params), exh), body)
                end)
	  val env = List.foldl
		(fn (cf, env) => VSet.add(env, CFunctions.varOf cf))
		  VSet.empty externs
	  in
	    chkFB (env, body);
	    if !anyErr then raise Fail "broken BOM" else ();
            !anyErr
	  end (* check *)
**** Fluet version ****)

(**** Reppy version ****)
  (* placeholder for testing variable kind equality *)
    fun eqVK _ = true

    fun vkToString B.VK_None = "VK_None"
      | vkToString (B.VK_Let rhs) =
	  concat["VK_Let(", BOMUtil.expToString rhs, ")"]
      | vkToString (B.VK_RHS rhs) =
	  concat["VK_RHS(", BOMUtil.rhsToString rhs, ")"]
      | vkToString B.VK_Param = "VK_Param"
      | vkToString (B.VK_Fun _) = "VK_Fun"
      | vkToString (B.VK_Cont _) = "VK_Cont"
      | vkToString (B.VK_Extern x) = concat["VK_Extern(", x, ")"]

  (* the context of a BOM expression *)
    datatype context
      = TAIL of (B.var * int)
      | BIND of B.var list

    fun check (phase, module) = let
	  val B.MODULE{name, externs, body} = module
	  val anyErrors = ref false
	(* report an error *)
	  fun pr s = TextIO.output(TextIO.stdErr, concat s)
	  fun error msg = (
		if !anyErrors then ()
		else (
		  pr ["***** Bogus BOM in ", Atom.toString name, " after ", phase, " *****\n"];
		  anyErrors := true);
		pr ("** " :: msg))
	(* a table mapping variables to their census counts *)
	  val counts = VTbl.mkTable (256, Fail "count table")
	  fun insert x = (case VTbl.find counts x
		 of SOME _ => error["multiple bindings of ", v2s x, "\n"]
		  | NONE => VTbl.insert counts (x, {
			appCnt = BV.appCntOf x,
			useCnt = BV.useCount x
		      })
		(* end case *))
	(* match a list of variables to a context *)
	  fun chkContext (cxt, xs) = let
		val n = List.length xs
		in
		  case cxt
		   of TAIL(f, arity) => if (arity <> n)
			then error [
			    "arity mismatch in ", v2s f, ": returning ", vl2s xs,
			    " for ", Int.toString arity, " expected results\n"
			  ]
			else ()
		    | BIND ys => if (List.length ys <> n)
			then error [
			    "arity mismatch: ", vl2s ys, " = ", vl2s xs, "\n"
			  ]
			else ()
		  (* end case *)
		end
	(* match a tail application to a context *)
	  fun chkApplyContext (cxt, f) = let
		val (_, _, rng) = BTy.asFunTy(BV.typeOf f)
		val n = List.length rng
		in
		  case cxt
		   of TAIL(g, arity) => if (arity <> n)
			then error [
			    "arity mismatch in ", v2s g, ": returning <", Int.toString n,
			    " results> for ", Int.toString arity, " expected results\n"
			  ]
			else ()
		    | BIND ys => if (List.length ys <> n)
			then error [
			    "arity mismatch: ", vl2s ys, " = <", Int.toString n, " results>\n"
			  ]
			else ()
		  (* end case *)
		end
	(* create a tail context *)
	  fun tailContext f = let
		val (_, _, rng) = BTy.asFunTy(BV.typeOf f)
		in
		  TAIL(f, List.length rng)
		end
	(* create a bind context *)
	  fun bindContext vl = BIND vl
	(* Check that a variable is bound *)
	  fun chkVar x = if VTbl.inDomain counts x
		then ()
		else error["unbound variable ", v2s x, "\n"]
	  fun chkBinding (x, binding) = if eqVK(BV.kindOf x, binding)
		then ()
		else error[
		    "binding of ", v2s x, " is ",
		    vkToString(BV.kindOf x), " (expected ",
		    vkToString binding, ")\n"
		  ]
	  fun chkBindings (lhs, binding) =
		List.app (fn x => chkBinding(x, binding)) lhs
(* FIXME: we should check the kind of the xs, but we don't have a kind for pattern-bound
 * variables yet!
 *)
	  fun chkPat (B.P_DCon(_, xs)) = List.app insert xs
	    | chkPat (B.P_Const _) = ()
	(* *)
	  fun insertFB (B.FB{f, ...}) = insert f
	  fun chkFB (lambda as B.FB{f, params, exh, body}) = (
		chkBinding (f, B.VK_Fun lambda);
		chkBindings (params, B.VK_Param);
		chkBindings (exh, B.VK_Param);
		List.app insert params;
		List.app insert exh;
		chkE (tailContext f, body))
	  and chkE (cxt, B.E_Pt(_, t)) = (case t
		 of B.E_Let(lhs, rhs, e) => (
		      chkBindings (lhs, B.VK_Let rhs);
		      chkE(bindContext lhs, rhs);
		      List.app insert lhs;
		      chkE(cxt, e))
		  | B.E_Stmt(lhs, rhs, e) => (
		      chkBindings (lhs, B.VK_RHS rhs);
		      chkRHS (lhs, rhs);
		      List.app insert lhs;
		      chkE(cxt, e))
		  | B.E_Fun(fbs, e) => (
		      List.app insertFB fbs;
		      chkE(cxt, e);
		      List.app chkFB fbs)
		  | B.E_Cont(fb as B.FB{f, params, exh, body}, e) => (
		      chkBinding (f, B.VK_Cont fb);
		      chkBindings (params, B.VK_Param);
		      insert f;
		      chkE(cxt, e);
		      List.app insert params;
		      if not(null exh)
			then error[
			    "continuation ", v2s f, " has non-empty return list"
			  ]
			else ();
		      chkE(cxt, body))
		  | B.E_If(x, e1, e2) => (chkVar x; chkE(cxt, e1); chkE(cxt, e2))
		  | B.E_Case(x, cases, dflt) => let
		      fun chk' (pat, e) = (chkPat pat; chkE(cxt, e))
		      in
			chkVar x;
			List.app chk' cases;
			Option.app (fn e => chkE(cxt, e)) dflt
		      end
		  | B.E_Apply(f, args, rets) => (
		      chkVar f;
		      List.app chkVar args;
		      List.app chkVar rets;
		      chkApplyContext (cxt, f))
		  | B.E_Throw(k, args) => (
		      chkVar k; List.app chkVar args)
		  | B.E_Ret args => (
		      List.app chkVar args;
		      chkContext (cxt, args))
		  | B.E_HLOp(hlop, args, rets) => (
		      List.app chkVar args;
		      List.app chkVar rets)
(* FIXME: check the hlop result against the context *)
		(* end case *))
	  and chkRHS (lhs, rhs) = BOMUtil.appRHS chkVar rhs
	(* check the module's main function *)
	  fun chkFB (lambda as B.FB{f, params, exh, body}) = (
		chkBinding (f, B.VK_Fun lambda);
		chkBindings (params, B.VK_Param);
		chkBindings (exh, B.VK_Param);
		List.app insert params;
		List.app insert exh;
		chkE(tailContext f, body))
	(* check an external function *)
	  fun chkExtern (CFunctions.CFun{var, name, ...}) = (
		insert var;
		case BV.kindOf var
		 of B.VK_Extern _ => ()
		  | vk => error[
			"extern ", v2s var, " has kind ", vkToString vk
		      ]
		(* end case *))
	(* check old counts against new counts *)
	  fun checkCnt (x, {appCnt, useCnt}) =
		if (appCnt <> BV.appCntOf x) orelse (useCnt <> BV.useCount x)
		  then error[
		      "inconsistant counts for ", v2s x, ": recorded <",
		      Int.toString useCnt, ":", Int.toString appCnt,
		      "> vs. actual <", Int.toString(BV.useCount x), ":",
		      Int.toString(BV.appCntOf x), ">\n"
		    ]
		  else ()
	  in
	  (* record census counts and do initial checking *)
	    List.app chkExtern externs;
	    chkFB body; insertFB body;
	  (* recompute census information *)
	    Census.census module;
	  (* check new and old census information *)
	    VTbl.appi checkCnt counts;
if !anyErrors
  then (
    print "******************** broken BOM ********************\n";
    PrintBOM.print module;
    print "********************\n";
    raise Fail "broken BOM")
  else ();
	  (* return the error status *)
	    !anyErrors
	  end
(**** Reppy version ****)

    val check = BasicControl.mkTracePass {
	    passName = "bom-check",
	    pass = check,
	    verbose = 2
	  }

  end
