(* translate-env.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TranslateEnv : sig

    type env

    val mkEnv : unit -> env

  (* data-constructor bindings *)
    datatype con_bind
      = Const of word * BOMTy.ty	(* nullary data constructors *)
      | ExnConst of BOMTy.data_con	(* nullary exception constructors *)
      | DCon of (BOMTy.data_con * FlattenRep.rep_tree)

  (* variable bindings *)
    datatype var_bind
      = Lambda			(* bound to a lambda generator.  The type argument is
				 * the result type.  Note that the generator creates a
				 * fresh copy of the lambda term.
				 *)
	  of (BOMTy.ty -> BOM.lambda)
      | Var of BOM.var
      | EqOp			(* either "=" or "<>" *)

    val insertTyc	: (env * Types.tycon * BOMTy.ty) -> unit
    val insertCon	: (env * Types.dcon * con_bind) -> unit
    val insertFun	: (env * AST.var * (BOMTy.ty -> BOM.lambda)) -> env
    val insertVar	: (env * AST.var * BOM.var) -> env
    val newHandler	: env -> (BOM.var * env)

    val findTyc		: (env * Types.tycon) -> BOMTy.ty option
    val findDCon	: (env * Types.dcon) -> con_bind option
    val lookupVar	: (env * AST.var) -> var_bind
    val lookupVarArity  : (env * AST.var) -> var_bind
    val handlerOf	: env -> BOM.var

    type var = ProgramParseTree.PML2.BOMParseTree.var_use
    type con = ProgramParseTree.PML2.BOMParseTree.dcon
    type ty_def = ProgramParseTree.PML2.BOMParseTree.ty_def

  (* support for inline BOM code *)
    val insertBOMTyDef	: (ty_def * BOMTy.ty) -> unit
    val insertBOMVar	: (var * BOM.var) -> unit
    val insertBOMCon    : (con * BOMTy.data_con) -> unit

    val findBOMTy	: ty_def -> BOMTy.ty option
    val findBOMVar	: var -> BOM.var option
    val findBOMCon	: con -> BOMTy.data_con option

  (* output an environment *)
    val dump : (TextIO.outstream * env) -> unit

  end = struct

    structure TTbl = TyCon.Tbl
    structure DTbl = DataCon.Tbl
    structure VMap = Var.Map

    datatype con_bind
      = Const of word * BOMTy.ty
      | ExnConst of BOMTy.data_con
      | DCon of (BOMTy.data_con * FlattenRep.rep_tree)

    datatype var_bind
      = Lambda of (BOMTy.ty -> BOM.lambda) (* used for primops and high-level ops *)
      | Var of BOM.var
      | EqOp			(* either "=" or "<>" *)

    datatype env = E of {
	tycEnv : BOM.ty TTbl.hash_table,
	dconEnv : con_bind DTbl.hash_table,
	varEnv : var_bind VMap.map,	(* map from AST variables to BOM variables *)
	exh : BOM.var			(* current exception handler *)
      }

    fun mkEnv () = let
	  val vEnv = List.foldl VMap.insert' VMap.empty [
		  (Basis.eq,	EqOp),
		  (Basis.neq,	EqOp)
		]
	  in
	    E{
		tycEnv = TTbl.mkTable (32, Fail "tyc table"),
		dconEnv = DTbl.mkTable (64, Fail "dcon table"),
		varEnv = vEnv,
		exh = BOM.Var.new ("*bogus*", BOMTy.exhTy)
	      }
	  end

    fun insertTyc (E{tycEnv, ...}, tyc, bty) = TTbl.insert tycEnv (tyc, bty)

    fun insertCon (E{dconEnv, ...}, dc, bind) = DTbl.insert dconEnv (dc, bind)

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

    fun lookupVarArity (E{varEnv, ...}, x) = (case VMap.find(varEnv, x)
           of SOME x' => x'
            | NONE => raise Fail(concat["lookupVarArity(_, ", Var.toString x, ")"])
          (* end case *))

  (* handlerOf : env -> B.var *)
    fun handlerOf (E{exh, ...}) = exh

    type var = ProgramParseTree.PML2.BOMParseTree.var_use
    type con = ProgramParseTree.PML2.BOMParseTree.dcon
    type ty_def = ProgramParseTree.PML2.BOMParseTree.ty_def

  (* support for inline BOM code *)
    local
	val {
	   getFn=getVar : ProgramParseTree.Var.var -> BOM.var option, 
	   setFn=setVar : (ProgramParseTree.Var.var * BOM.var option) -> unit, ...
	} =
	    ProgramParseTree.Var.newProp(fn _ => NONE)

	val {
	   getFn=getCon : ProgramParseTree.Var.var -> BOMTy.data_con option, 
	   setFn=setCon : (ProgramParseTree.Var.var * BOMTy.data_con option) -> unit, ...
	} =
	    ProgramParseTree.Var.newProp(fn _ => NONE)

	val {
	   getFn=getTy : ProgramParseTree.Var.var -> BOMTy.ty option, 
	   setFn=setTy : (ProgramParseTree.Var.var * BOMTy.ty option) -> unit, ...
	} =
	    ProgramParseTree.Var.newProp(fn _ => NONE)
    in
    fun insertBOMTyDef (name, ty) = setTy(name, SOME ty)

    fun insertBOMVar (name, x) = setVar(name, SOME x)

    fun insertBOMCon (name, dc) = setCon(name, SOME dc)

    val findBOMTy = getTy

    val findBOMVar = getVar

    val findBOMCon = getCon
    end

  (* output an environment *)
    fun dump (outStrm, E{tycEnv, dconEnv, varEnv, ...}) = let
	  val w2s = Word.fmt StringCvt.DEC
	  fun pr x = TextIO.output(outStrm, x)
	  fun prl xs = pr(String.concat xs)
	  fun prTyc (tyc, ty) = prl [
		  "    ", TyCon.toString tyc, "  :->  ", BOMTyUtil.toString ty,
		  " :: ", BOMTyUtil.kindToString(BOMTyUtil.kindOf ty), "\n"
		]
	  fun prDcon (dc, Const(n, ty)) = prl [
		  "    ", DataCon.nameOf dc, "  :->  ", w2s n, " : ",
		  BOMTyUtil.toString ty, "\n"
		]
	    | prDcon (dc, ExnConst(BOMTy.DCon{name, ...})) = prl [
		  "    ", DataCon.nameOf dc, "  :->  ", name, "; <exn>\n"
		]
	    | prDcon (dc, DCon(BOMTy.DCon{name, rep, argTy, ...}, repTr)) = let
		val argTy = (case argTy
		       of [ty] => [BOMTyUtil.toString ty]
			| ty::tys => "(" :: BOMTyUtil.toString ty
			    :: (List.foldr (fn (ty, r) => ", " :: BOMTyUtil.toString ty :: r)
				[")"] tys)
		      (* end case *))
		val rep = (case rep
		       of BOMTy.Transparent => "<transparent>"
			| BOMTy.Tuple => "<tuple>"
			| BOMTy.TaggedTuple tag => concat["<tagged-tuple(", w2s tag, ")>"]
			| BOMTy.ExnRep => "<exn>"
		      (* end case *))
		in
		  prl [
		      "    ", DataCon.nameOf dc, " of ", FlattenRep.fmt {long=true} repTr,
		      "  :->  ",
		      name, " of ", String.concat argTy, "; ", rep, "\n"
		    ]
		end
	  fun prVar (x, bind) = let
		val bind = (case bind
		       of Var x' => concat[
			      BOM.Var.toString x', " : ", BOMTyUtil.toString(BOM.Var.typeOf x')
			    ]
			| Lambda _ => "<lambda>"
			| EqOp => "<eq>"
		      (* end case *))
		in
		  prl [
		      "    ", Var.toString x, " : ", TypeUtil.schemeToString(Var.typeOf x),
		      "  :->  ", bind, "\n"
		    ]
		end
	  in
	    pr "***** Translation environment dump *****\n";
	    pr "  *** Type constructors:\n";
	    TTbl.appi prTyc tycEnv;
	    pr "  *** Data constructors:\n";
	    DTbl.appi prDcon dconEnv;
	    pr "  *** Variables:\n";
	    VMap.appi prVar varEnv;
	    pr "*****\n"
	  end

  end
