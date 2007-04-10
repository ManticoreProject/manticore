(* case-simplify.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This phase simplifies cases and applications of data constructors in
 * the BOM representation.  The resulting code will be free of data constructors
 * and all cases will be over enumeration tags.
 *)

structure CaseSimplify : sig

    val transform : BOM.module -> BOM.module

  end = struct

    structure B = BOM
    structure BV = BOM.Var

    fun xformE (B.E_Pt(_, e)) = (case e
	   of B.E_Let(lhs, e1, e2) => B.mkLet(lhs, xformE exp, xformE e2)
	    | B.E_Stmt(lhs, B.E_DCon(dc, xs), e) => ??
	    | B.E_Stmt(lhs, rhs, e) => B.mkStmt(lhs, rhs, xformE e)
	    | B.E_Fun(fbs, e) => B.mkFun(List.map xformLambda fbs, xformE e)
	    | B.E_Cont(fb, e) => B.mkCont(List.map xformLamda
	    | B.E_If(x, e1, e2) => B.E_If(x, xformE e1, xformE x2)
	    | B.E_Case(x, rules, dflt) => ??
	    | e => B.mkExp e
	  (* end case *))

    and xformLambda (B.FB{f, params, exh, body}) =
	  B.FB{f = f, params = params, exh = exh, body = xformE body}

    fun transform (B.MODULE{name, externs, body}) =
	  B.mkModule(name, externs, xformLambda body)

  end
