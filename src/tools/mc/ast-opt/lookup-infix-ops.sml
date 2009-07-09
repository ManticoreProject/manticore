(* lookup-infix-ops.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure LookupInfixOps : sig

    val tr : AST.exp -> AST.exp

  end = struct

    fun tr e = let
       (* lookupInfixOp : AST.var -> AST.var *)
	 fun lookupInfixOp (f : AST.var) : AST.var = 
	     if Var.same (f, Basis.psub) then BasisEnv.getVarFromBasis ["PArray", "sub"]
	     else f
	 in
	   SubstVar.exp lookupInfixOp e
	 end

  end
