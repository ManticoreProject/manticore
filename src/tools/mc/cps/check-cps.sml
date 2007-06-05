(* check-cps.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CheckCPS : sig

    val check : CPS.module -> unit

  end = struct

    structure C = CPS
    structure V = C.Var
    structure Ty = CPSTy
    structure VSet = V.Set

    val v2s = V.toString
    fun vl2s [] = "()"
      | vl2s [x] = v2s x
      | vl2s (x::xs) = let
	  fun f (x, l) = "," :: v2s x :: l
	  in
	    String.concat("[" :: v2s x :: List.foldr f ["]"] xs)
	  end

    fun typeOfFB (CPS.FB{params, rets, ...}) =
	  Ty.T_Fun(List.map V.typeOf params, List.map V.typeOf rets)

    fun addFB (C.FB{f, ...}, env) = VSet.add(env, f)

    fun addVars (env, xs) = VSet.addList(env, xs)

    fun check (C.MODULE{name, externs, body}) = let
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
		  | chk (_, []) = err["too few arguments in ", cxt]
		  | chk ([], _) = err["too many arguments in ", cxt]
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
	  fun chkExp (env, e) = (case e
		 of C.Let(lhs, rhs, e) => (
		      chkRHS(env, lhs, rhs);
		      chkExp (addVars(env, lhs), e))
		  | C.Fun(fbs, e) => let
		      val env = List.foldl addFB env fbs
		      in
			List.app (fn fb => chkFB(env, fb)) fbs;
			chkExp(env, e)
		      end
		  | C.Cont(fb, e) => let
		      val env = addFB (fb, env)
		      in
			chkFB(env, fb); chkExp(env, e)
		      end
		  | C.If(x, e1, e2) => (chkVar(env, x, "If"); chkExp(env, e1); chkExp(env, e2))
		  | C.Switch(x, cases, dflt) => (
		      chkVar(env, x, "Switch");
		      List.app (fn (_, e) => chkExp (env, e)) cases;
		      Option.app (fn e => chkExp (env, e)) dflt)
		  | C.Apply(f, args, rets) => (
		      chkVar (env, f, "Apply");
		      case V.typeOf f
		       of Ty.T_Fun(argTys, retTys) => (
			    chkVars (env, args, "Apply args");
			    chkVars (env, rets, "Apply rets");
			    checkArgs (argTys, args, concat["Apply ", v2s f, " args"]);
			    checkArgs (retTys, rets, concat["Apply ", v2s f, " rets"]))
			| ty => err[v2s f, ":", Ty.toString ty, " is not a function"]
		      (* end case *))
		  | C.Throw(k, args) => (
		      chkVar (env, k, "Throw");
		      case V.typeOf k
		       of Ty.T_Fun(argTys, []) => (
			    chkVars (env, args, "Throw args");
			    checkArgs (argTys, args, "Throw " ^ v2s k))
			| ty => err[v2s k, ":", Ty.toString ty, " is not a continuation"]
		      (* end case *))
		(* end case *))
	  and chkRHS (env, lhs, rhs) = (case (List.map V.typeOf lhs, rhs)
		 of (tys, C.Var xs) => chkVars(env, xs, "Var")
		  | ([ty], C.Cast(ty', x)) => (
		      chkVar (env, x, "Cast");
		      if Ty.match(ty', ty) andalso Ty.validCast(V.typeOf x, ty')
			then ()
			else err["type mismatch in Cast"])
		  | ([ty], C.Const(_, ty')) => ()
		  | ([ty], C.Select(i, x)) => chkVar(env, x, "Select")
		  | ([], C.Update(i, x, y)) => (
		      chkVar(env, x, "Update"); chkVar(env, y, "Update"))
		  | ([ty], C.AddrOf(i, x)) => chkVar(env, x, "AddrOf")
		  | ([ty], C.Alloc xs) => chkVars(env, xs, "Alloc")
		  | ([ty], C.Wrap x) => chkVar(env, x, "Wrap")
		  | ([ty], C.Unwrap x) => chkVar(env, x, "Unwrap")
		  | ([ty], C.Prim p) => chkVars(env, PrimUtil.varsOf p, PrimUtil.nameOf p)
		  | ([ty], C.CCall(cf, args)) => (
		      chkVar(env, cf, "CCall"); chkVars(env, args, "CCall args"))
		  | ([ty], C.HostVProc) => ()
		  | ([ty], C.VPLoad(n, vp)) => chkVar(env, vp, "VPLoad")
		  | ([], C.VPStore(n, vp, x)) => (
		      chkVar(env, vp, "VPStore"); chkVar(env, x, "VPStore"))
		  | _ => err["bogus rhs for ", vl2s lhs]
		(* end case *))
	  and chkFB (env, fb as C.FB{f, params, rets, body}) = (
		case V.typeOf f
		 of Ty.T_Fun(argTys, retTys) => let
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
			chk(argTys, params);
			chk(retTys, rets)
		      end
		  | ty => err[
			"expected function/continuation type for ",
			v2s f, ":", Ty.toString(V.typeOf f)
		      ]
		(* end case *);
		chkExp (addVars(addVars(env, params), rets), body))
	  val env = List.foldl
		(fn (cf, env) => VSet.add(env, CFunctions.varOf cf))
		  VSet.empty externs
	  in
	    chkFB (env, body);
	    if !anyErr then raise Fail "broken CPS" else ()
	  end (* check *)

  end
