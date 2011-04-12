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

structure FlattenEnv (* : sig

  type env
  val  mkEnv : unit -> env

  val insertTyc  : env * Types.tycon * Types.tycon -> unit
  val insertDCon : env * Types.dcon * Types.dcon -> unit
  val insertVar  : env * AST.var * AST.var -> env
  val insertFlOp : env * AST.fl_op -> unit

  val findTyc   : env * Types.tycon -> Types.tycon option
  val findDCon  : env * Types.dcon -> Types.dcon option
  val findVar   : env * AST.var -> AST.var option

  val lookupTyc  : env * Types.tycon -> Types.tycon
  val lookupDCon : env * Types.dcon -> Types.dcon
  val lookupVar  : env * AST.var -> AST.var

  val flOpSet   : env -> FlattenOp.Set.set

end *) = struct

  structure A = AST
  structure T = Types 
  structure B = Basis

  structure TU = TypeUtil

  structure TTbl = TyCon.Tbl
  structure DTbl = DataCon.Tbl
  structure VMap = Var.Map
  structure FSet = FlattenOp.Set

  datatype env = Env of {
      mustFlatten : bool TTbl.hash_table,
      parrPrims   : (T.ty -> A.exp) option VMap.map,
      tycEnv      : T.tycon TTbl.hash_table,
      dconEnv     : T.dcon DTbl.hash_table,
      varEnv      : A.var VMap.map,
      flOps       : FSet.set ref
    }

(* selectors *)
  fun mustFlattenOf (Env {mustFlatten, ...}) = mustFlatten
  fun parrPrimsOf (Env {parrPrims, ...}) = parrPrims
  fun tycEnvOf  (Env {tycEnv, ...})  = tycEnv
  fun dconEnvOf (Env {dconEnv, ...}) = dconEnv
  fun varEnvOf  (Env {varEnv, ...})  = varEnv
  fun flOpsOf   (Env {flOps, ...})   = !flOps

(* functional update of varEnv *)
  fun withVarEnv v (Env {tycEnv, dconEnv, varEnv, flOps, mustFlatten, parrPrims}) =
    Env {tycEnv=tycEnv, dconEnv=dconEnv, varEnv=v, 
	 flOps=flOps, mustFlatten=mustFlatten, parrPrims=parrPrims}

(* imperative update of flOps *)
  fun setFlOps f (Env {flOps, ...}) = (flOps := f)

(* collectParrPrims : unit -> ... VMap.map *)
(* - collect the "primitive" operators out of the parray basis module. *)
(* These primitives require special handling in the flattening transformation. *)  
  fun collectParrPrims () = let
    fun getVar name = BasisEnv.getVarFromBasis ["PArray", name]
    val ps = [("toRope", NONE),
	      ("fromRope", NONE),
	      ("length", SOME PArrayOp.constructLength),
              ("sub", SOME PArrayOp.constructSub),
	      ("tab", SOME PArrayOp.constructTab),
	      ("map", SOME PArrayOp.constructMap),
	      ("reduce", SOME PArrayOp.constructReduce)]
    val ps' = List.map (fn (n, oper) => (getVar n, oper)) ps
    in
      List.foldl VMap.insert' VMap.empty ps'
    end

(* fresh env maker *)
  fun mkEnv () = let
    val t = TTbl.mkTable (32, Fail "tycon table")
    val d = DTbl.mkTable (32, Fail "dcon table")
    val v = List.foldl VMap.insert' VMap.empty [(B.eq, B.eq), (B.neq, B.neq)]
    val f = ref (FSet.empty)
    val m = TTbl.mkTable (32, Fail "must flatten table")
    val p = collectParrPrims ()
    in
      Env {tycEnv = t, dconEnv = d, varEnv = v, flOps = f, mustFlatten = m, parrPrims = p}
    end

(* spoof env for testing *)
    val spoofEnv =  let
      val t = TTbl.mkTable (32, Fail "tycon table")
      val d = DTbl.mkTable (32, Fail "dcon table")
      val v = List.foldl VMap.insert' VMap.empty [(B.eq, B.eq), (B.neq, B.neq)]
      val f = ref (FSet.empty)
      val m = TTbl.mkTable (32, Fail "must flatten table")
      val p = VMap.empty
      in
        Env {tycEnv = t, dconEnv = d, varEnv = v, flOps = f, mustFlatten = m, parrPrims = p}
      end

  fun markFlattenTyc (Env {mustFlatten, ...}, tyc, bool) = 
    TTbl.insert mustFlatten (tyc, bool)

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

  fun findMustFlatten (Env {mustFlatten, ...}, tyc) = TTbl.find mustFlatten tyc

  fun findTyc (Env {tycEnv, ...}, tyc) = TTbl.find tycEnv tyc

  fun findDCon (Env {dconEnv, ...}, con) = DTbl.find dconEnv con

  fun findParrPrim (Env {parrPrims, ...}, x) = VMap.find (parrPrims, x)

  fun isParrPrim (Env {parrPrims, ...}, x) = isSome (VMap.find (parrPrims, x))

  fun lookupTyc (Env {tycEnv, ...}, tyc) = (case TTbl.find tycEnv tyc
    of NONE => raise Fail ("lookupTyc " ^ TyCon.toString tyc)
     | SOME c => c
    (* end case *))

  fun lookupDCon (Env {dconEnv, ...}, dcon) = (case DTbl.find dconEnv dcon
    of NONE => raise Fail ("lookupDCon " ^ DataCon.toString dcon)
     | SOME d => d
    (* end case *))

  fun lookupVar (Env {varEnv, ...}, x) = 
   (case VMap.find (varEnv, x)
     of SOME y => y
      | NONE => raise Fail ("lookupVar: " ^ Var.toString x ^ ")")
    (* end case *))

  fun findVar (Env {varEnv, ...}, x) = VMap.find (varEnv, x)

  fun flOpSet (Env {flOps, ...}) = !flOps

(* +debug *)
  fun printVarEnv e = let
    fun pr (x, y) = TextIO.print (String.concat [Var.toString x,
						 " --> ",
						 Var.toString y,
						 "\n"])
    in
      List.app pr (VMap.listItemsi e)
    end

  fun PVE (Env {varEnv, ...}) = printVarEnv varEnv
(* -debug *)

end
