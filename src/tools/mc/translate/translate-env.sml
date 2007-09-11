(* translate-env.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TranslateEnv : sig

    type env

    val mkEnv : unit -> env

    val insertTyc	: (env * Types.tycon * BOMTy.ty) -> unit
    val insertDCon	: (env * Types.dcon * BOMTy.data_con) -> unit
    val insertFun	: (env * AST.var * BOM.lambda) -> env
    val insertVar	: (env * AST.var * BOM.var) -> env
    val newHandler	: env -> (BOM.var * env)

    datatype var_bind
      = Lambda of BOM.lambda	(* used for primops and high-level ops *)
      | Var of BOM.var

    val findTyc		: (env * Types.tycon) -> BOMTy.ty option
    val findDCon	: (env * Types.dcon) -> BOMTy.data_con option
    val lookupVar	: (env * AST.var) -> var_bind
    val handlerOf	: env -> BOM.var

  end = struct

    structure TTbl = TyCon.Tbl
    structure DTbl = DataCon.Tbl
    structure VMap = Var.Map

    datatype var_bind
      = Lambda of BOM.lambda	(* used for primops and high-level ops *)
      | Var of BOM.var

    datatype env = E of {
	tycEnv : BOM.ty TTbl.hash_table,
	dconEnv : BOMTy.data_con DTbl.hash_table,
	varEnv : var_bind VMap.map,	(* map from AST variables to BOM variables *)
	exh : BOM.var			(* current exception handler *)
      }

    fun mkEnv () = E{
	    tycEnv = TTbl.mkTable (32, Fail "tyc table"),
	    dconEnv = DTbl.mkTable (64, Fail "dcon table"),
	    varEnv = VMap.empty,
	    exh = BOM.Var.new ("*bogus*", BOMTy.T_Any)
	  }

    fun insertTyc (E{tycEnv, ...}, tyc, bty) = TTbl.insert tycEnv (tyc, bty)

    fun insertDCon (E{dconEnv, ...}, dc, bdc) = DTbl.insert dconEnv (dc, bdc)

    fun insertFun (E{tycEnv, dconEnv, varEnv, exh}, x, lambda) = E{
	    tycEnv = tycEnv,
	    dconEnv = dconEnv,
	    varEnv = VMap.insert(varEnv, x, Lambda lambda),
	    exh = exh
	  }

    fun insertVar (E{tycEnv, dconEnv, varEnv, exh}, x, x') = E{
	    tycEnv = tycEnv,
	    dconEnv = dconEnv,
	    varEnv = VMap.insert(varEnv, x, Var x'),
	    exh = exh
	  }

    fun newHandler (E{tycEnv, dconEnv, varEnv, ...}) = let
	  val exh = BOM.Var.new("_exh", BOMTy.exhTy)
	  val env = E{
		  tycEnv = tycEnv,
		  dconEnv = dconEnv,
		  varEnv = varEnv,
		  exh = exh
		}
	  in
	    (exh, env)
	  end

    fun findTyc (E{tycEnv, ...}, tyc) = TTbl.find tycEnv tyc

    fun findDCon (E{dconEnv, ...}, dc) = DTbl.find dconEnv dc

    fun lookupVar (E{varEnv, ...}, x) = (case VMap.find(varEnv, x)
	   of SOME x' => x'
	    | NONE => raise Fail(concat["lookupVar(_, ", Var.toString x, ")"])
	  (* end case *))

  (* handlerOf : env -> B.var *)
    fun handlerOf (E{exh, ...}) = exh

  end
