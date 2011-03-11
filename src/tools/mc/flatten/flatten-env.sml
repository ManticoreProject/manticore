(* flatten-env.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Environment mapping tycons/dcons with non-nullary constructors to flat counterparts.
 *
 * for example: datatype a = A of (int * int) parray
 *                maps to
 *              datatype a' = A' of {int;lf} * {int;lf}
 *)

structure FlattenEnv : sig

  type env
  val  mkEnv : unit -> env

  val insertTyc  : env * Types.tycon * Types.tycon -> unit
  val insertDCon : env * Types.dcon * Types.dcon -> unit
  val insertVar  : env * AST.var * AST.var -> env
  val insertFlOp : env * AST.fl_op -> unit

  val findTyc   : env * Types.tycon -> Types.tycon option
  val findDCon  : env * Types.dcon -> Types.dcon option
  val lookupVar : env * AST.var -> AST.var

  val flOpSet   : env -> FlattenOp.Set.set

end = struct

  structure A = AST
  structure T = Types 
  structure B = Basis
  structure U = TypeUtil

  structure TTbl = TyCon.Tbl
  structure DTbl = DataCon.Tbl
  structure VMap = Var.Map
  structure FSet = FlattenOp.Set

  datatype env = Env of {
      tycEnv  : T.tycon TTbl.hash_table,
      dconEnv : T.dcon DTbl.hash_table,
      varEnv  : A.var VMap.map,
      flOps   : FSet.set ref
    }

(* selectors *)
  fun tycEnvOf  (Env {tycEnv, ...})  = tycEnv
  fun dconEnvOf (Env {dconEnv, ...}) = dconEnv
  fun varEnvOf  (Env {varEnv, ...})  = varEnv
  fun flOpsOf   (Env {flOps, ...})   = !flOps

(* functional update of varEnv *)
  fun withVarEnv v (Env {tycEnv, dconEnv, varEnv, flOps}) =
    Env {tycEnv=tycEnv, dconEnv=dconEnv, varEnv=v, flOps=flOps}

  fun setFlOps f (Env {flOps, ...}) = (flOps := f)

(* fresh env maker *)
  fun mkEnv () = let
    val t = TyCon.Tbl.mkTable (32, Fail "tycon table")
    val d = DataCon.Tbl.mkTable (32, Fail "dcon table")
    val v = Var.Map.empty
    val f = ref (FlattenOp.Set.empty)
    in
      Env {tycEnv = t, dconEnv = d, varEnv = v, flOps = f}
    end

  fun insertTyc (Env {tycEnv, ...}, tyc, tyc') = TTbl.insert tycEnv (tyc, tyc')
  
  fun insertDCon (Env {dconEnv, ...}, con, con') = DTbl.insert dconEnv (con, con')

  fun insertVar (e : env, x : A.var, y : A.var) : env = let
    val varEnv' = VMap.insert (varEnvOf e, x, y)
    in
      withVarEnv varEnv' e
    end

  fun insertFlOp (e : env, oper : AST.fl_op) : unit = let
    val flOps' = FSet.add (flOpsOf e, oper)
    in
      setFlOps flOps' e
    end

  fun findTyc (Env {tycEnv, ...}, tyc) = TTbl.find tycEnv tyc

  fun findDCon (Env {dconEnv, ...}, con) = DTbl.find dconEnv con

  fun lookupVar (Env {varEnv, ...}, x) = 
   (case VMap.find (varEnv, x)
     of SOME y => y
      | NONE => raise Fail ("lookupVar: " ^ Var.toString x ^ ")")
    (* end case *))

  fun flOpSet (Env {flOps, ...}) = !flOps

end
