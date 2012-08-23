(* inline.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Flattening transformation utilities in BOM.
 *
 *)

structure FlattenUtil : sig

  type env = BOMUtil.subst

  val trVar     : env -> BOM.var -> BOM.var
  val trVars    : env -> BOM.var list -> BOM.var list

end = struct

  structure B  = BOM
  structure U  = BOMUtil

  type env = U.subst
  val trVar = U.subst : env -> B.var -> B.var
  (* curry this for consistency *)
  fun trVars (env : env) (vs : B.var list) = U.subst' (env, vs)

end
