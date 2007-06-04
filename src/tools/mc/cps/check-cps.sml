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

    fun typeOfFB (CPS.FB{params, rets, ...}) =
	  Ty.T_Fun(List.map V.typeOf params, List.map V.typeOf rets)

    fun addFB (C.FB{f, ...}, env) = VSet.add(env, f)

    fun check (C.MODULE{name, externs, body}) = let
	  val anyErr = ref false
	  fun err msg = (
		anyErr := true;
		print(concat("Error: " :: msg);
		print "\n")
	  fun chkVar (env, x) = if VSet.member(env, x)
		then ()
		else err["variable ", v2s x, " unbound"]
	  fun chkVars (env, xs) = List.app (fn x => chkVar(env, x)) xs
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
		      val env = addFB(fb, env)
		      in
			chkFB(env, fb); chkExp(env, e)
		      end
		  | C.If(x, e1, e2) => (chkVar(env, x); chkExp(env, e1); chkExp(env, e2))
		  | C.Switch(x, cases, dflt) => (
		      chkVar(env, x);
		      List.app (fn (_, e) => chkExp (env, e)) cases;
		      Option.app (fn e => chkExp (env, e)) dflt)
		  | C.Apply(f, args, rets) = (case V.typeOf f
		       of Ty.T_Fun(argTys, retTys) => (
			    chkVars (env, args);
			    chkVars (env, rets);
			    if matchTys(argTys, List.map V.typeOf args)
			    andalso matchTys(retTys, List.map V.typeOf rets)
			      then ()
			      else err[])
			| ty => err[]
		      (* end case *))
		  | C.Throw(k, args) = (case V.typeOf k
		       of Ty.T_Fun(argTys, []) =>
			| ty => err[]
		      (* end case *))
		(* end case *))
	  and chkRHS (env, lhs, rhs) = (case (List.map V.typeOf lhs, rhs)
		 of (tys, C.Var xs)
		  | ([ty], C.Cast(ty', x)) => (
		      if Ty.match(ty', ty) andalso Ty.validCast(V.typeOf x, ty')
			then ()
			else err[]
		  | ([ty], C.Const(_, ty')) =>
		  | ([ty], C.Select(i, x)) =>
		  | ([], C.Update(i, x, y)) =>
		  | ([ty], C.AddrOf(i, x)) =>
		  | ([ty], C.Alloc xs) =>
		  | ([ty], C.Wrap x) =>
		  | ([ty], C.Unwrap x) =>
		  | ([ty], C.Prim p) =>
		  | ([ty], C.CCall(cf, args)) =>
		  | ([ty], C.HostVProc) =>
		  | ([ty], C.VPLoad(n, vp)) =>
		  | ([], C.VPStore(n, vp, x)) =>
		  | _ => err[]
		(* end case *))
	  and chkFB (env, fb as C.FB{f, params, rets, body}) = (
(* compare typeOf f with typeOfFB fb *)
		chkExp (VSet.addList(VSet.addList(env, params), rets), body))
	  in
	  end (* check *)

  end
