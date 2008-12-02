(* cps-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CPSUtil : sig

    val appRHS : (CPS.var -> unit) -> CPS.rhs -> unit
    val mapRHS : (CPS.var -> CPS.var) -> CPS.rhs -> CPS.rhs

    val rhsToString : CPS.rhs -> string

    val applyToBoundVars : (CPS.var -> unit) -> CPS.module -> unit

  end = struct

    structure C = CPS

    val v2s = C.Var.toString
    fun vl2s xs = String.concatWith ", " (List.map v2s xs)
    val p2s = PrimUtil.fmt v2s

    fun appRHS f rhs = (case rhs
	   of C.Var xs => List.app f xs
	    | C.Cast(ty, x) => f x
	    | C.Const(lit, ty) => ()
	    | C.Select(i, x) => f x
	    | C.Update(i, x, y) => (f x; f y)
	    | C.AddrOf(i, x) => f x
	    | C.Alloc(_, xs) => List.app f xs
	    | C.Promote x => f x
	    | C.Prim p => PrimUtil.app f p
	    | C.CCall(cf, xs) => (f cf; List.app f xs)
	    | C.HostVProc => ()
	    | C.VPLoad(n, x) => f x
	    | C.VPStore(n, x, y) => (f x; f y)
	  (* end case *))

    fun mapRHS f rhs = (case rhs
	   of C.Var xs => C.Var(List.map f xs)
	    | C.Cast(ty, x) => C.Cast(ty, f x)
	    | C.Const(lit, ty) => rhs
	    | C.Select(i, x) => C.Select(i, f x)
	    | C.Update(i, x, y) => C.Update(i, f x, f y)
	    | C.AddrOf(i, x) => C.AddrOf(i, f x)
	    | C.Alloc(ty, xs) => C.Alloc(ty, List.map f xs)
	    | C.Promote x => C.Promote(f x)
	    | C.Prim p => C.Prim(PrimUtil.map f p)
	    | C.CCall(cf, xs) => C.CCall(f cf, List.map f xs)
	    | C.HostVProc => rhs
	    | C.VPLoad(n, x) => C.VPLoad(n, f x)
	    | C.VPStore(n, x, y) => C.VPStore(n, f x, f y)
	  (* end case *))

    fun rhsToString (C.Var xs) = concat["Var(", vl2s xs, ")"]
      | rhsToString (C.Cast(ty, x)) = concat["Cast(", CPSTyUtil.toString ty, ", ", v2s x, ")"]
      | rhsToString (C.Const(lit, ty)) = concat["Const(", Literal.toString lit, ", ", CPSTyUtil.toString ty, ")"]
      | rhsToString (C.Select(i, x)) = concat["Select(", Int.toString i, ", ", v2s x, ")"]
      | rhsToString (C.Update(i, x, y)) = concat["Update(", Int.toString i, ", ", v2s x,  ", ", v2s y, ")"]
      | rhsToString (C.AddrOf(i, x)) = concat["AddrOf(", Int.toString i, ", ", v2s x, ")"]
      | rhsToString (C.Alloc(ty, xs)) = concat["Alloc(", CPSTyUtil.toString ty, ", ", vl2s xs, ")"]
      | rhsToString (C.Promote x) = concat["Promote(", v2s x, ")"]
      | rhsToString (C.Prim p) = p2s p
      | rhsToString (C.CCall(cf, xs)) = concat["CCall(", v2s cf, ", [", vl2s xs, "])"]
      | rhsToString (C.HostVProc) = "HostVProc"
      | rhsToString (C.VPLoad(n, x)) = concat["VPLoad(", IntInf.toString n, ", ", v2s x, ")"]
      | rhsToString (C.VPStore(n, x, y)) = concat["VPStore(", IntInf.toString n, v2s x,  ", ", v2s y, ")"]

    fun applyToBoundVars func (C.MODULE{externs, body, ...}) = let
	  fun applyToFBs fbs = (
		List.app (fn (C.FB{f, ...}) => func f) fbs;
		List.app (fn (C.FB{params, rets, body, ...}) => (
		    List.app func params;
		    List.app func rets;
		    applyToExp body)
		  ) fbs)
	  and applyToExp (C.Exp(_, e)) = (case e
		 of (C.Let(lhs, rhs, e)) => (
		      List.app func lhs;
		      applyToExp e)
		  | (C.Fun(fbs, e)) => (
		      applyToFBs fbs;
		      applyToExp e)
		  | (C.Cont(fb, e)) => (
		      applyToFBs [fb];
		      applyToExp e)
		  | (C.If(x, e1, e2)) => (applyToExp e1; applyToExp e2)
		  | (C.Switch(x, cases, dflt)) => (
		      List.app (fn (_, e) => applyToExp e) cases;
		      Option.app applyToExp dflt)
		  | (C.Apply _) => ()
		  | (C.Throw _) => ()
		(* end case *))
	  in
	    List.app (fn (CFunctions.CFun{var, ...}) => func var) externs;
	    applyToFBs [body]
	  end

  end
