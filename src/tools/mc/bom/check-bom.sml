(* check-bom.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CheckBOM : sig

    val check : BOM.module -> unit

  end = struct

    structure B = BOM
    structure V = B.Var
    structure Ty = BOMTy
    structure VSet = V.Set

    val v2s = V.toString
    fun vl2s [] = "()"
      | vl2s [x] = v2s x
      | vl2s (x::xs) = let
	  fun f (x, l) = "," :: v2s x :: l
	  in
	    String.concat("[" :: v2s x :: List.foldr f ["]"] xs)
	  end

    fun addFB (B.FB{f, ...}, env) = VSet.add(env, f)

    fun addVars (env, xs) = VSet.addList(env, xs)

    fun addPat (env, B.P_DCon (_, xs)) = addVars (env, xs)
      | addPat (env, B.P_Const _) = env

    fun check (B.MODULE{name,externs,body}) = let
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
                        else err["type mismatch in Alloc"])
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
	    if !anyErr then raise Fail "broken BOM" else ()
	  end (* check *)

    fun check (m : BOM.module) = ()

    val check =
       BasicControl.mkTracePass
       {passName = "BOMCheck",
        pass = check,
        verbose = 2}

  end
