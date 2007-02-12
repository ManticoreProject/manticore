(* check-cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check a CFG module for well-formedness
 *)

structure CheckCFG : sig

    val check : CFG.module -> unit

  end = struct

    structure VSet = CFG.Var.Set
    structure LSet = CFG.Label.Set

    fun error msg = TextIO.output(TextIO.stdErr, concat("Error: " :: msg @ ["\n"]))

    fun bindVar (env, x) = VSet.add(env, x)

    fun bindVars (env, xs) = VSet.addList(env, xs)

    fun check (m as CFG.MODULE{name, code}) = let
	(* construct a set of the bound labels in the module *)
	  val lSet = List.foldl
		(fn (f as CFG.FUNC{lab, ...}, lset) => LSet.add(lset, lab))
		  LSet.empty code
	  fun chk (CFG.FUNC{lab, entry, body, exit}) = let
		fun chkVar (env, x) = if VSet.member(env, x)
		      then ()
		      else error[
			  "unbound variable ", CFG.Var.toString x, " in ",
			  CFG.Label.toString lab, ".", Atom.toString name
			]
		fun chkVars (env, xs) = List.app (fn x => chkVar(env, x)) xs
		fun chkLabel l = (case (CFG.Label.kindOf l, LSet.member(lSet, l))
		       of (CFG.LK_None, _) => error [
			      "label ", CFG.Label.toString l, "has no kind in ",
			      CFG.Label.toString lab, ".", Atom.toString name
			    ]
			| (CFG.LK_Extern _, false) => ()
			| (CFG.LK_Extern _, true) => error [
			      "exported local label ", CFG.Label.toString l,
			      " in ", CFG.Label.toString lab, ".", Atom.toString name
			    ]
			| (CFG.LK_Local _, true) => ()
			| (CFG.LK_Local _, false) => error[
			      "reference to unbound label ", CFG.Label.toString l,
			      " in ", CFG.Label.toString lab, ".", Atom.toString name
			    ]
		      (* end case *))
		fun chkEntry (CFG.StdFunc{clos, arg, ret, exh}) =
		      bindVars(VSet.empty, [clos, arg, ret, exh])
		  | chkEntry (CFG.StdCont{clos, arg}) =
		      bindVars(VSet.empty, [clos, arg])
		  | chkEntry (CFG.KnownFunc params) =
		      bindVars(VSet.empty, params)
		  | chkEntry (CFG.Block params) =
		      bindVars(VSet.empty, params)
		fun chkExp (e, env) = (case e
		       of CFG.E_Var(lhs, rhs) => (
			    chkVars (env, rhs);
			    bindVars (env, lhs))
			| CFG.E_Enum(x, _) => bindVar (env, x)
			| CFG.E_Cast(x, ty, y) => (
(* FIXME: check that x and y have same kinds and that x has type ty *)
			    chkVar (env, y);
			    bindVar (env, x))
			| CFG.E_Label(x, lab) => (
			    chkLabel lab;
			    bindVar (env, x))
			| CFG.E_Literal(x, _) => bindVar (env, x)
			| CFG.E_Select(x, i, y) => (
(* FIXME: check the type of y *)
			    chkVar (env, y);
			    bindVar (env, x))
			| CFG.E_Alloc(x, ys) => (
(* FIXME: check the type of x *)
			    chkVars (env, ys);
			    bindVar (env, x))
			| CFG.E_Wrap(x, y) => (
(* FIXME: check the type of y *)
			    chkVar (env, y);
			    bindVar (env, x))
			| CFG.E_Unwrap(x, y) => (
(* FIXME: check the type of y *)
			    chkVar (env, y);
			    bindVar (env, x))
			| CFG.E_Prim(x, p) => (
			    chkVars (env, PrimUtil.varsOf p);
			    bindVar (env, x))
			| CFG.E_CCall(lhs, f, args) => (
			    chkVars (env, f::args);
			    bindVars (env, lhs))
		      (* end case *))
		fun chkExit (env, xfer) = (case xfer
		       of CFG.StdApply{f, clos, arg, ret, exh} => (
(* FIXME: check type of f *)
			    chkVars (env, [f, clos, arg, ret, exh]))
			| CFG.StdThrow{k, clos, arg} => (
(* FIXME: check type of k *)
			    chkVars (env, [k, clos, arg]))
			| CFG.Apply{f, args} => (
(* FIXME: check type of f *)
			    chkVars (env, f::args))
			| CFG.Goto jmp => chkJump (env, jmp)
			| CFG.If(x, j1, j2) => (
(* FIXME: check type of x *)
			    chkVar (env, x);
			    chkJump (env, j1);
			    chkJump (env, j2))
			| CFG.Switch(x, cases, dflt) => (
			    chkVar (env, x);
			    List.app (fn (_, j) => chkJump(env, j)) cases;
			    Option.app (fn j => chkJump(env, j)) dflt)
			| CFG.HeapCheck{szb, gc, nogc} => (
			    chkJump (env, gc);
			    chkJump (env, nogc))
		      (* end case *))
		and chkJump (env, (lab, args)) = (
(* FIXME: check the type of the label *)
		      chkLabel lab;
		      chkVars (env, args))
		val env = chkEntry entry
		val env = List.foldl chkExp env body
		in
		  chkExit (env, exit)
		end (* chk *)
	  in
	    List.app chk code
	  end (* check *)

  end
