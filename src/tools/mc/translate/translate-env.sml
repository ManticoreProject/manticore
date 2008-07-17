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
    type hlop = ProgramParseTree.PML2.BOMParseTree.hlop_bind
    type c_id = ProgramParseTree.PML2.BOMParseTree.c_id
    type import_env = BOM.var CFunctions.c_fun AtomTable.hash_table

  (* support for inline BOM code *)
    datatype bty_def
      = BTY_NONE
      | BTY_TYS of AST.ty_scheme
      | BTY_TYC of Types.tycon
      | BTY_TY of BOMTy.ty
    type hlop_def = {
	name : BOM.hlop,			(* the HLOp's identifier *)
	inline : bool,				(* should the HLOp be inlined? *)
	def : BOM.lambda,			(* the HLOps definition *)
	pmlImports : (BOM.var * BOM.var) list,  (* imports from PML *)
	externs : (BOM.var * int) list		(* list of external variables (i.e., C functions) *)
						(* that def references paired with a count of the *)
						(* number of references *)
      }
    val getImportEnv    : env -> import_env
    val insertBOMTyDef	: (ty_def * BOMTy.ty) -> unit
    val insertBOMVar	: (var * BOM.var) -> unit
    val insertBOMHLOp   : (hlop * HLOp.hlop) -> unit
    val insertBOMHLOpDef : (hlop * hlop_def) -> unit
    val insertBOMCFun   : (import_env * c_id * BOM.var CFunctions.c_fun) -> unit

    val findBOMTy	: ty_def -> bty_def
    val findBOMVar	: var -> BOM.var option
    val findBOMHLOp     : hlop -> HLOp.hlop option
    val findBOMHLOpDef  : hlop -> hlop_def option
    val findBOMCFun     : c_id -> BOM.var CFunctions.c_fun option
    val findBOMPMLVar   : var -> BOM.var option

  (* output an environment *)
    val dump : (TextIO.outstream * env) -> unit

  end = struct

    structure TTbl = TyCon.Tbl
    structure DTbl = DataCon.Tbl
    structure VMap = Var.Map
    structure ATbl = AtomTable

    datatype con_bind
      = Const of word * BOMTy.ty
      | ExnConst of BOMTy.data_con
      | DCon of (BOMTy.data_con * FlattenRep.rep_tree)

    datatype var_bind
      = Lambda of (BOMTy.ty -> BOM.lambda) (* used for primops and high-level ops *)
      | Var of BOM.var
      | EqOp			(* either "=" or "<>" *)

    type import_env = BOM.var CFunctions.c_fun AtomTable.hash_table

    datatype env = E of {
	tycEnv : BOM.ty TTbl.hash_table,
	dconEnv : con_bind DTbl.hash_table,
	varEnv : var_bind VMap.map,	(* map from AST variables to BOM variables *)
	importEnv : import_env,         (* an environment to keep track of any imports required by the module *)
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
		importEnv = ATbl.mkTable (32, Fail "importEnv"),
		exh = BOM.Var.new ("*bogus*", BOMTy.exhTy)
	      }
	  end

    fun insertTyc (E{tycEnv, ...}, tyc, bty) = TTbl.insert tycEnv (tyc, bty)

    fun insertCon (E{dconEnv, ...}, dc, bind) = DTbl.insert dconEnv (dc, bind)

    fun insertFun (E{tycEnv, dconEnv, varEnv, importEnv, exh}, x, lambda) = E{
	    tycEnv = tycEnv,
	    dconEnv = dconEnv,
	    varEnv = VMap.insert(varEnv, x, Lambda lambda),
	    importEnv = importEnv,
	    exh = exh
	  }

    fun newHandler (E{tycEnv, dconEnv, varEnv, importEnv, ...}) = let
	  val exh = BOM.Var.new("_exh", BOMTy.exhTy)
	  val env = E{
		  tycEnv = tycEnv,
		  dconEnv = dconEnv,
		  varEnv = varEnv,
		  importEnv = importEnv,	
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
    type hlop = ProgramParseTree.PML2.BOMParseTree.hlop_bind
    type c_id = ProgramParseTree.PML2.BOMParseTree.c_id
    datatype bty_def
      = BTY_NONE
      | BTY_TYS of AST.ty_scheme
      | BTY_TYC of Types.tycon
      | BTY_TY of BOMTy.ty
    type hlop_def = {
	name : BOM.hlop,			(* the HLOp's identifier *)
	inline : bool,				(* should the HLOp be inlined? *)
	def : BOM.lambda,			(* the HLOps definition *)
	pmlImports : (BOM.var * BOM.var) list,  (* imports from PML *)
	externs : (BOM.var * int) list		(* list of external variables (i.e., C functions) *)
						(* that def references paired with a count of the *)
						(* number of references *)
      }

    fun getImportEnv (E{importEnv, ...}) = importEnv

  (* support for inline BOM code *)
    local
	structure PTVar = ProgramParseTree.Var
      (* variables *)
	val {
	   getFn=getVar : PTVar.var -> BOM.var option, 
	   setFn=setVar : (PTVar.var * BOM.var option) -> unit, ...
	} =
	    ProgramParseTree.Var.newProp(fn _ => NONE)
      (* data constructors *)
	val {
	   getFn=getCon : PTVar.var -> con_bind option, 
	   setFn=setCon : (PTVar.var * con_bind option) -> unit, ...
	} =
	    ProgramParseTree.Var.newProp(fn _ => NONE)
      (* types *)
	val {
	   getFn=getTy : PTVar.var -> BOMTy.ty option, 
	   setFn=setTy : (PTVar.var * BOMTy.ty option) -> unit, ...
	} =
	    ProgramParseTree.Var.newProp(fn _ => NONE)
      (* HLOps *)
	val {
	   getFn=getHLOp : PTVar.var -> HLOp.hlop option,
	   setFn=setHLOp : (PTVar.var * HLOp.hlop option) -> unit, ...
	} =
	    ProgramParseTree.Var.newProp(fn _ => NONE)
      (* HLOp definitions *)
	val {
	   getFn=getHLOpDef : PTVar.var -> hlop_def option,
	   setFn=setHLOpDef : (PTVar.var * hlop_def option) -> unit, ...
	} =
	    ProgramParseTree.Var.newProp(fn _ => NONE)
      (* C functions *)
	val {
	   getFn=getCFun : PTVar.var -> BOM.var CFunctions.c_fun option,
	   setFn=setCFun : (PTVar.var * BOM.var CFunctions.c_fun option) -> unit, ...
	} =
	    ProgramParseTree.Var.newProp(fn _ => NONE)
      (* imported PML variables *)
	val {
	   getFn=getPMLVar : AST.var -> BOM.var option,
	   setFn=setPMLVar : (AST.var * BOM.var option) -> unit, ...
	} =
	    Var.newProp(fn _ => NONE)
    in
    fun insertBOMTyDef (name, ty) = setTy(name, SOME ty)
    fun insertBOMVar (name, x) = setVar(name, SOME x)
(*    fun insertBOMCon (name, con) = setCon(name, SOME con)*)
    fun insertBOMHLOp (name, hlop) = setHLOp(name, SOME hlop)
    fun insertBOMHLOpDef (name, hlop) = setHLOpDef(name, SOME hlop)
    fun insertBOMCFun (importEnv, name, cfun) = (
	    setCFun(name, SOME cfun);
	    ATbl.insert importEnv (Atom.atom (PTVar.nameOf name), cfun)
        )
    fun insertPMLVar (av, bv) = setPMLVar (av, SOME bv)  (* imported PML variables *)
    fun findBOMTy v = (
	   case getTy v
	    of NONE => (
	       (* find the type definition in PML code *)
	         case ModuleEnv.getTyDef v
		  of NONE => BTY_NONE
		   | SOME (ModuleEnv.TyDef tys) => BTY_TYS tys
		   | SOME (ModuleEnv.TyCon tyc) => BTY_TYC tyc
		 (* end case *))
              (* found the type definition in inline BOM code*)
	     | SOME ty => BTY_TY ty
           (* end case *))
    val findBOMVar = getVar
    val findBOMHLOp = getHLOp
    val findBOMHLOpDef = getHLOpDef
    val findBOMCFun = getCFun
  (* Importing PML variables is a two-step process: PML parse-tree variable -> AST variable -> BOM variable.
   *)
    fun findBOMPMLVar v = (case ModuleEnv.getValBind v
            of SOME (ModuleEnv.Var astVar) => getPMLVar astVar
	     | _ => NONE
            (* end case *))
    end

    fun insertVar (E{tycEnv, dconEnv, varEnv, importEnv, exh}, x, x') = (
	 insertPMLVar(x, x');  (* bind PML variables so that inline BOM code can reference them *)
	 E{
	    tycEnv = tycEnv,
	    dconEnv = dconEnv,
	    varEnv = VMap.insert(varEnv, x, Var x'),
	    importEnv = importEnv,
	    exh = exh
	  })

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
