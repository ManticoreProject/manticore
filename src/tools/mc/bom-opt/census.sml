(* census.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Initialize the use counts of the variables in a BOM module.
 *)

structure Census : sig

    val census : BOM.module -> unit

  end = struct

    val clr = B.Var.clrCount
    fun inc x = B.Var.addCount(x, 1)

  (* record an application use *)
    fun appUse x = inc x

    fun doE (B.E_Pt(_, t)) = (case t
	   of B.E_Let(lhs, e1, e2) => (List.app clr lhs; doE e1; doE e2)
	    | B.E_Stmt(lhs, rhs, e) => (List.app clr lhs; doRHS rhs; doE e)
	    | B.E_Fun of (lambda list * exp)
	    | B.E_Cont of (lambda * exp)
	    | B.E_If of (var * exp * exp)
	    | B.E_Case(x, cases, dflt) =>
	    | B.E_Apply(k, xs, ys) => (appUse k; List.app inc xs; List.app inc ys)
	    | B.E_Throw(k, xs) => (appUse k; List.app inc xs)
	    | B.E_Ret xs = List.app inc xs
	    | B.E_HLOp(_, xs, ys) => (List.app inc xs; List.app inc ys)
	  (* end case *))
  end
