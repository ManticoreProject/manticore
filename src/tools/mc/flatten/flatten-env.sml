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

  val findTyc   : env * Types.tycon -> Types.tycon option
  val findDCon  : env * Types.dcon -> Types.dcon option
  val lookupVar : env * AST.var -> AST.var

end = struct

  structure A = AST
  structure T = Types 
  structure B = Basis
  structure U = TypeUtil

  datatype env = Env of {
      tycEnv  : T.tycon TyCon.Tbl.hash_table,
      dconEnv : T.dcon DataCon.Tbl.hash_table,
      varEnv  : AST.var Var.Map.map
    }

  fun mkEnv () = let
    val t = TyCon.Tbl.mkTable (32, Fail "tycon table")
    val d = DataCon.Tbl.mkTable (32, Fail "dcon table")
    val v = Var.Map.empty
    in
      Env {tycEnv = t, dconEnv = d, varEnv = v}
    end

  fun insertTyc (Env {tycEnv, ...}, tyc, tyc') = 
    TyCon.Tbl.insert tycEnv (tyc, tyc')
  
  fun insertDCon (Env {dconEnv, ...}, con, con') = 
    DataCon.Tbl.insert dconEnv (con, con')

  fun insertVar (Env {dconEnv, tycEnv, varEnv}, x, y) = let
    val varEnv' = Var.Map.insert (varEnv, x, y)
    in
      Env {dconEnv=dconEnv, tycEnv=tycEnv, varEnv=varEnv'}
    end

  fun findTyc (Env {tycEnv, ...}, tyc) = 
    TyCon.Tbl.find tycEnv tyc

  fun findDCon (Env {dconEnv, ...}, con) = 
    DataCon.Tbl.find dconEnv con

  fun lookupVar (Env {varEnv, ...}, x) = 
   (case Var.Map.find (varEnv, x)
     of SOME y => y
      | NONE => raise Fail ("lookupVar: " ^ Var.toString x ^ ")")
    (* end case *))

end
