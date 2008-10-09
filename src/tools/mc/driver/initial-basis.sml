(* initial-basis.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure InitialBasis : sig

  (* the primitive environment defines the name to binding-variable mapping for
   * the primitive PML and BOM types.  It is used to process the "initial-basis.pml"
   * file.
   *)
    val primBindingEnv : BindingEnv.env
    val primEnv : ModuleEnv.env

  (* given the environments produced from the "initial-basis.pml" file,
   * extend them with the operators.  We also return the initial translation
   * environment that binds the operator AST variables to the HLOps defined
   * in "initial-basis.pml".
   *)
    val extendInitialEnv : (BindingEnv.env * ModuleEnv.env)
	  -> (BindingEnv.env * ModuleEnv.env * (unit -> TranslateEnv.env))

  end = struct

    structure PPT = ProgramParseTree
    structure BEnv = BindingEnv
    structure MEnv = ModuleEnv
    structure TEnv = TranslateEnv
    structure N = BasisNames
    structure BTy = BOMTy
    structure BV = BOM.Var

  (* create a new bound variable *)
    fun newVar name = ProgramParseTree.Var.new(Atom.toString name, ())

  (* primitive PML types *)
    val boolTyc = TyCon.newDataTyc (N.bool, [])
    val boolTrue = DataCon.new boolTyc (N.boolTrue, NONE)
    val boolFalse = DataCon.new boolTyc (N.boolFalse, NONE)

    local
	val tv = TyVar.new(Atom.atom "'a")
	val tv' = AST.VarTy tv
    in
    val listTyc = TyCon.newDataTyc (N.list, [tv])
    val listNil = DataCon.new listTyc (N.listNil, NONE)
    val listCons = DataCon.new listTyc
	  (N.listCons, SOME(AST.TupleTy[tv', AST.ConTy([tv'], listTyc)]))
    end (* local *)

    val exnTyc = Exn.exnTyc

    val intTyc = TyCon.newAbsTyc (N.int, 0, true)
    val longTyc = TyCon.newAbsTyc (N.long, 0, true)
    val integerTyc = TyCon.newAbsTyc (N.integer, 0, true)
    val floatTyc = TyCon.newAbsTyc (N.float, 0, true)
    val doubleTyc = TyCon.newAbsTyc (N.double, 0, true)
    val charTyc = TyCon.newAbsTyc (N.char, 0, true)
    val runeTyc = TyCon.newAbsTyc (N.rune, 0, true)
    val stringTyc = TyCon.newAbsTyc (N.string, 0, true)
    val parrayTyc = TyCon.newAbsTyc (N.parray, 1, false)
    val chanTyc = TyCon.newAbsTyc (N.chan, 1, true)
    val ivarTyc = TyCon.newAbsTyc (N.ivar, 1, true)
    val mvarTyc = TyCon.newAbsTyc (N.mvar, 1, true)
    val eventTyc = TyCon.newAbsTyc (N.event, 1, false)
    val threadIdTyc = TyCon.newAbsTyc (N.thread_id, 0, true)

  (* predefined types *)
    val unitTy = AST.TupleTy[]
    val boolTy = AST.ConTy([], boolTyc)
    val exnTy = AST.ConTy([], exnTyc)
    val intTy = AST.ConTy([], intTyc)
    val longTy = AST.ConTy([], longTyc)
    val integerTy = AST.ConTy([], integerTyc)
    val floatTy = AST.ConTy([], floatTyc)
    val doubleTy = AST.ConTy([], doubleTyc)
    val charTy = AST.ConTy([], charTyc)
    val runeTy = AST.ConTy([], runeTyc)
    val stringTy = AST.ConTy([], stringTyc)
    fun listTy ty = AST.ConTy([ty], listTyc)
    fun parrayTy ty = AST.ConTy([ty], parrayTyc)

  (* exceptions *)
    val exnBind = Exn.new (N.exnBind, NONE)
    val exnDiv = Exn.new (N.exnDiv, NONE)
    val exnFail = Exn.new (N.exnFail, SOME stringTy)
    val exnMatch = Exn.new (N.exnMatch, NONE)

  (* construct the primitive environments *)
    val (primBindingEnv, primEnv) = let
	  val bEnv = BEnv.empty (Atom.atom "Prim", NONE)
	  val mEnv = MEnv.empty
	(* insert the primitive type constructors (and associated data constructors) *)
	  fun insTyc (tyc, (bEnv, mEnv)) = let
		val Types.Tyc{name, def, ...} = tyc
		val id = newVar name
		in
		  case def
		   of Types.AbsTyc => (
			  BEnv.insertTycBind(bEnv, name, id),
			  MEnv.insertTy(mEnv, id, MEnv.TyCon tyc)
			)
		    | Types.DataTyc{cons, ...} => let
			fun insDC (dc as Types.DCon{name, ...}, (bEnv, mEnv)) = let
			      val dcId = newVar name
			      in (
				BEnv.insertDataCon(bEnv, name, dcId, id),
				MEnv.insertVar (mEnv, dcId, MEnv.Con dc)
			      ) end
			val (bEnv, mEnv) = List.foldl insDC (bEnv, mEnv) (!cons)
			in (
			  BEnv.insertTycBind (bEnv, name, id),
			  MEnv.insertTy (mEnv, id, MEnv.TyCon tyc)
			) end
		  (* end case *)
		end
	  val (bEnv, mEnv) = List.foldl insTyc (bEnv, mEnv) [
		  boolTyc,
		  listTyc,
		  exnTyc,
		  intTyc,
		  longTyc,
		  integerTyc,
		  floatTyc,
		  doubleTyc,
		  charTyc,
		  runeTyc,
		  stringTyc,
		  parrayTyc,
		  chanTyc,
		  ivarTyc,
		  mvarTyc,
		  eventTyc,
		  threadIdTyc
		]
	(* insert the primitive exceptions *)
	  in
	    (bEnv, mEnv)
	  end

    val --> = AST.FunTy
    fun ** (t1, t2) = AST.TupleTy[t1, t2]
    infix 9 **
    infixr 8 -->

  (* forall : (AST.ty -> AST.ty) -> AST.ty_scheme *)
    fun forall mkTy = let
        val tv = TyVar.new(Atom.atom "'a")
        in
          AST.TyScheme([tv], mkTy(AST.VarTy tv))
        end

  (* monoVar : string * A.ty -> A.var *)
    fun monoVar (name, ty) = Var.new(name, ty)

  (* polyVar : string * (A.ty -> A.ty) -> A.var *) 
    fun polyVar (name, mkTy) = Var.newPoly(name, forall mkTy)

  (**** operator symbols ****
   *
   * We use the following naming convention for operators that allows automatic
   * mapping to the HLOp names in "initial-basis.pml".  For example, the addition
   * operator for integers is named "Int.add", which is mapped to "@int-add".
   *) 
    val list_append = Var.newPoly("list-append",
	  forall(fn tv => let
	    val ty = listTy tv
	    in
	      ty ** ty --> ty
	    end))
    val string_concat = monoVar("string-concat", stringTy ** stringTy --> stringTy)
    val parray_sub = polyVar("parray-sub", fn tv => (parrayTy tv) ** intTy --> tv)

    local
      fun name a = "int-" ^ a
    in
    val int_div =       monoVar(name "div", intTy ** intTy --> intTy)
    val int_gt =        monoVar(name "gt", intTy ** intTy --> boolTy)
    val int_gte =       monoVar(name "gte", intTy ** intTy --> boolTy)
    val int_lt =        monoVar(name "lt", intTy ** intTy --> boolTy)
    val int_lte =       monoVar(name "lte", intTy ** intTy --> boolTy)
    val int_minus =     monoVar(name "sub", intTy ** intTy --> intTy)
    val int_mod =       monoVar(name "mod", intTy ** intTy --> intTy)
    val int_neg =       monoVar(name "neg", intTy --> intTy)
    val int_plus =      monoVar(name "add", intTy ** intTy --> intTy)
    val int_times =     monoVar(name "mul", intTy ** intTy --> intTy)
    end

    local
      fun name a = "long-" ^ a
    in
    val long_div =      monoVar(name "div", longTy ** longTy --> longTy)
    val long_gt =       monoVar(name "gt", longTy ** longTy --> boolTy)
    val long_gte =      monoVar(name "gte", longTy ** longTy --> boolTy)
    val long_lt =       monoVar(name "lt", longTy ** longTy --> boolTy)
    val long_lte =      monoVar(name "lte", longTy ** longTy --> boolTy)
    val long_minus =    monoVar(name "sub", longTy ** longTy --> longTy)
    val long_mod =      monoVar(name "mod", longTy ** longTy --> longTy)
    val long_neg =      monoVar(name "neg", longTy --> longTy)
    val long_plus =     monoVar(name "add", longTy ** longTy --> longTy)
    val long_times =    monoVar(name "mul", longTy ** longTy --> longTy)
    end

    local
      fun name a = "integer-" ^ a
    in
    val integer_div =   monoVar(name "div", integerTy ** integerTy --> integerTy)
    val integer_gt =    monoVar(name "gt", integerTy ** integerTy --> boolTy)
    val integer_gte =   monoVar(name "gte", integerTy ** integerTy --> boolTy)
    val integer_lt =    monoVar(name "lt", integerTy ** integerTy --> boolTy)
    val integer_lte =   monoVar(name "lte", integerTy ** integerTy --> boolTy)
    val integer_minus = monoVar(name "sub", integerTy ** integerTy --> integerTy)
    val integer_mod =   monoVar(name "mod", integerTy ** integerTy --> integerTy)
    val integer_neg =   monoVar(name "neg", integerTy --> integerTy)
    val integer_plus =  monoVar(name "add", integerTy ** integerTy --> integerTy)
    val integer_times = monoVar(name "mul", integerTy ** integerTy --> integerTy)
    end

    local
      fun name a = "float-" ^ a
    in
    val float_fdiv =    monoVar(name "div", floatTy ** floatTy --> floatTy)
    val float_gt =      monoVar(name "gt", floatTy ** floatTy --> boolTy)
    val float_gte =     monoVar(name "gte", floatTy ** floatTy --> boolTy)
    val float_lt =      monoVar(name "lt", floatTy ** floatTy --> boolTy)
    val float_lte =     monoVar(name "lte", floatTy ** floatTy --> boolTy)
    val float_minus =   monoVar(name "sub", floatTy ** floatTy --> floatTy)
    val float_neg =	monoVar(name "neg", floatTy --> floatTy)
    val float_plus =    monoVar(name "add", floatTy ** floatTy --> floatTy)
    val float_times =   monoVar(name "mul", floatTy ** floatTy --> floatTy)
    end

    local
      fun name a = "double-" ^ a
    in
    val double_fdiv =   monoVar(name "div", doubleTy ** doubleTy --> doubleTy)
    val double_gt =     monoVar(name "gt", doubleTy ** doubleTy --> boolTy)
    val double_gte =    monoVar(name "gte", doubleTy ** doubleTy --> boolTy)
    val double_lt =     monoVar(name "lt", doubleTy ** doubleTy --> boolTy)
    val double_lte =    monoVar(name "lte", doubleTy ** doubleTy --> boolTy)
    val double_minus =  monoVar(name "sub", doubleTy ** doubleTy --> doubleTy)
    val double_neg =    monoVar(name "neg", doubleTy --> doubleTy)
    val double_plus =   monoVar(name "add", doubleTy ** doubleTy --> doubleTy)
    val double_times =  monoVar(name "mul", doubleTy ** doubleTy --> doubleTy)
    end

    local
      fun name a = "char-" ^ a
    in
    val char_gt =       monoVar(name "gt", charTy ** charTy --> boolTy)
    val char_gte =      monoVar(name "gte", charTy ** charTy --> boolTy)
    val char_lt =       monoVar(name "lt", charTy ** charTy --> boolTy)
    val char_lte =      monoVar(name "lte", charTy ** charTy --> boolTy)
    end

    local
      fun name a = "rune-" ^ a
    in
    val rune_gt =       monoVar(name "gt", runeTy ** runeTy --> boolTy)
    val rune_gte =      monoVar(name "gte", runeTy ** runeTy --> boolTy)
    val rune_lt =       monoVar(name "lt", runeTy ** runeTy --> boolTy)
    val rune_lte =      monoVar(name "lte", runeTy ** runeTy --> boolTy)
    end

    local
      fun name a = "string-" ^ a
    in
    val string_gt =     monoVar(name "gt", stringTy ** stringTy --> boolTy)
    val string_gte =    monoVar(name "gte", stringTy ** stringTy --> boolTy)
    val string_lt =     monoVar(name "lt", stringTy ** stringTy --> boolTy)
    val string_lte =    monoVar(name "lte", stringTy ** stringTy --> boolTy)
    end

    local
      fun eqTyScheme () = let
	    val tv = TyVar.newClass (Atom.atom "'a", Types.Eq)
	    val tv' = Types.VarTy tv
	    in
	      Types.TyScheme([tv], tv' ** tv' --> boolTy)
	    end
    in
    val eq = Var.newPoly(Atom.toString N.eq, eqTyScheme())
    val neq = Var.newPoly(Atom.toString N.neq, eqTyScheme())
    end


  (* overloaded operators *)
    nonfix div mod
    local
    (* create a type scheme that binds a kinded type variable *)
      fun tyScheme (cls, mk) = let
	    val tv = TyVar.newClass (Atom.atom "'a", cls)
	    in
	      Types.TyScheme([tv], mk(Types.VarTy tv))
	    end
    in
    val lte = (tyScheme(Types.Order, fn tv => (tv ** tv --> boolTy)),
	       [int_lte, long_lte, integer_lte, float_lte, double_lte, char_lte, rune_lte, string_lte])
    val lt = (tyScheme(Types.Order, fn tv => (tv ** tv --> boolTy)),
	       [int_lt, long_lt, integer_lt, float_lt, double_lt, char_lt, rune_lt, string_lt])
    val gte = (tyScheme(Types.Order, fn tv => (tv ** tv --> boolTy)),
	       [int_gte, long_gte, integer_gte, float_gte, double_gte, char_gte, rune_gte, string_gte])
    val gt = (tyScheme(Types.Order, fn tv => (tv ** tv --> boolTy)),
	       [int_gt, long_gt, integer_gt, float_gt, double_gt, char_gt, rune_gt, string_gt])

    val plus = (tyScheme(Types.Num, fn tv => (tv ** tv --> tv)),
	       [int_plus, long_plus, integer_plus, float_plus, double_plus])
    val minus = (tyScheme(Types.Num, fn tv => (tv ** tv --> tv)),
	       [int_minus, long_minus, integer_minus, float_minus, double_minus])
    val times = (tyScheme(Types.Num, fn tv => (tv ** tv --> tv)),
	       [int_times, long_times, integer_times, float_times, double_times])

    val fdiv = (tyScheme(Types.Float, fn tv => (tv ** tv --> tv)),
		[float_fdiv, double_fdiv])

    val div = (tyScheme(Types.Int, fn tv => (tv ** tv --> tv)),
	       [int_div, long_div, integer_div])
    val mod = (tyScheme(Types.Int, fn tv => (tv ** tv --> tv)),
	       [int_mod, long_mod, integer_mod])

    val neg = (tyScheme(Types.Num, fn tv => (tv --> tv)),
		[int_neg, long_neg, integer_neg, float_neg, double_neg])
    end (* local *)

  (* create wrapper code for a high-level operation. *)
    fun hlopFun (hlop as HLOp.HLOp{name, sign, ...}) = let
	  val {params, exh, results} = sign
	  val paramTys = let
		fun get (HLOp.PARAM t) = t
		  | get (HLOp.OPT t) = raise Fail "hlop.get: OPT"
		  | get (HLOp.VEC t) = raise Fail "hlop.get: VEC"
		in
		  List.map get params
		end
	(* mkVars : BTy.ty list -> BV.var list *)
	  fun mkVars baseName ts = let
	      (* build : string -> BTy.ty list * int -> BV.var list *)
		fun build ([], _) = []
		  | build (t::ts, n) = let
		      val x = baseName ^ Int.toString n
		      in
			  BV.new (x, t) :: build (ts, n+1)
		      end
		in
		  build (ts, 0)
		end
(* NOTE: once we have proper polymorphism in BOM, we shouldn't need this function *)
	  fun castResult (h, ty) = let
		  val resTy = (case results of [r] => r | _ => raise Fail "resTy")
		  in
		    if BOMTyUtil.equal (resTy, ty)
		      then h
		      else let
			val x = BV.new ("x", resTy)
			val y = BV.new ("y", ty)
			in
			  BOM.mkLet([x], h,
			  BOM.mkStmt([y], BOM.E_Cast(ty, x),
			    BOM.mkRet [y]))
			end
		  end
	  fun mkFB ty = let
		val params = mkVars "arg" paramTys
		val exhVars = mkVars "exh" exh
		val h = BOM.mkHLOp (hlop, params, exhVars)
		val fty = BTy.T_Fun(paramTys, exh, [ty])
		val f = BV.new (Atom.toString name, fty)
		in
		  BOM.FB{f=f, params=params, exh=exhVars, body=castResult(h, ty)}
		end
	  in
	    mkFB
	  end

  (* based on the given binding environment for "initial-basis.pml", create
   * the initial translation environment
   *)
    fun mkTransEnv bEnv () = let
	  fun bindOp (rator, tEnv) = let
		val name = Var.nameOf rator
		fun missing () = (
		      TextIO.output(TextIO.stdErr, concat[
			  "Warning: no HLOp for ", name, " found\n"
			]);
		      tEnv)
		in
		  case BEnv.findBOMHLOp(bEnv, Atom.atom name)
		   of SOME id => (case TEnv.findBOMHLOp id
			 of SOME hlop => TEnv.insertFun (tEnv, rator, hlopFun hlop)
			  | NONE => missing()
			(* end *))
		    | NONE => missing()
		end
	  in
	    List.foldl bindOp (TEnv.mkEnv()) [
		int_div,
		int_gt,
		int_gte,
		int_lt,
		int_lte,
		int_minus,
		int_mod,
		int_neg,
		int_plus,
		int_times,
		long_div,
		long_gt,
		long_gte,
		long_lt,
		long_lte,
		long_minus,
		long_mod,
		long_neg,
		long_plus,
		long_times,
		integer_div,
		integer_gt,
		integer_gte,
		integer_lt,
		integer_lte,
		integer_minus,
		integer_mod,
		integer_neg,
		integer_plus,
		integer_times,
		float_fdiv,
		float_gt,
		float_gte,
		float_lt,
		float_lte,
		float_minus,
		float_neg,
		float_plus,
		float_times,
		double_fdiv,
		double_gt,
		double_gte,
		double_lt,
		double_lte,
		double_minus,
		double_neg,
		double_plus,
		double_times,
		char_gt,
		char_gte,
		char_lt,
		char_lte,
		rune_gt,
		rune_gte,
		rune_lt,
		rune_lte,
		string_gt,
		string_gte,
		string_lt,
		string_lte,
		list_append,
		string_concat,
		parray_sub
	      ]
	  end

  (* given the environments produced from the "initial-basis.pml" file,
   * extend them with the operators
   *)
    fun extendInitialEnv (bEnv, mEnv) = let
	(* insert overloaded operators *)
	  fun insOverloadedOp ((name, (tyS, defs)), (bEnv, mEnv)) = let
		val id = newVar name
		in (
		  BEnv.insertVal(bEnv, name, BEnv.Var id),
		  MEnv.insertVar(mEnv, id, MEnv.Overload(tyS, defs))
		) end
	  val (bEnv, mEnv) = List.foldl insOverloadedOp (bEnv, mEnv) [
		  (N.lte, lte),
		  (N.lt, lt),
		  (N.gte, gte),
		  (N.gt, gt),
		  (N.plus, plus),
		  (N.minus, minus),
		  (N.times, times),
		  (N.fdiv, fdiv),
		  (N.div, div),
		  (N.mod, mod),
		  (N.uMinus, neg)
		]
	(* insert non-overloaded operators *)
	  fun insOp (x, (bEnv, mEnv)) = let
		val name = Atom.atom(Var.nameOf x)
		val id = newVar name
		in (
		  BEnv.insertVal(bEnv, name, BEnv.Var id),
		  MEnv.insertVar(mEnv, id, MEnv.Var x)
		) end
	  val (bEnv, mEnv) = List.foldl insOp (bEnv, mEnv) [
		  list_append,
		  string_concat,
		  parray_sub
		]
	  in
	    (bEnv, mEnv, mkTransEnv bEnv)
	  end

  (* primitive BOM types *)
    fun wrapTy rty = BOMTyUtil.wrap(BTy.T_Raw rty)
    val bomTypes = [
	    ("bool",	BTy.K_UNBOXED,	BOMBasis.boolTy),
	    ("int",	BTy.K_BOXED,	wrapTy BTy.T_Int),
	    ("long",	BTy.K_BOXED,	wrapTy BTy.T_Long),
	    ("float",	BTy.K_BOXED,	wrapTy BTy.T_Float),
	    ("double",	BTy.K_BOXED,	wrapTy BTy.T_Double),
	    ("string",	BTy.K_BOXED,	BOMBasis.stringTy),
	    ("exn",	BTy.K_BOXED,	BTy.exnTy)
	  ]

  end
