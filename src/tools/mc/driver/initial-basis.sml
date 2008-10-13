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
    val primTranslationEnv : TranslateEnv.env

  (* given the environments produced from the "initial-basis.pml" file,
   * extend them with the operators.  We also return the initial translation
   * environment that binds the operator AST variables to the HLOps defined
   * in "initial-basis.pml".
   *)
    val extendInitialEnv : (BindingEnv.env * ModuleEnv.env) -> {
	    bEnv : BindingEnv.env ,
	    mEnv : ModuleEnv.env,
	    glueAST : (AST.top_dec list * AST.exp) -> AST.exp
	  }

  end = struct

    structure PPT = ProgramParseTree
    structure BEnv = BindingEnv
    structure MEnv = ModuleEnv
    structure TEnv = TranslateEnv
    structure N = BasisNames
    structure BTy = BOMTy
    structure BV = BOM.Var

  (* get the definitions from Basis *)
    open Basis

  (* create a new bound variable *)
    fun newVar name = ProgramParseTree.Var.new(Atom.toString name, ())

  (* construct the primitive environments *)
    val (primBindingEnv, primEnv) = let
	  val bEnv = BEnv.empty (Atom.atom "Prim")
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
	(* add primitive types *)
	  fun insTy ((name, ty), (bEnv, mEnv)) = let
		val id = newVar name
		in (
		  BEnv.insertTy(bEnv, name, id),
		  MEnv.insertTy(mEnv, id, MEnv.TyDef ty)
		) end
	  val (bEnv, mEnv) = List.foldl insTy (bEnv, mEnv) [
		  (N.unit, Types.TyScheme([], unitTy))
		]
	(* add data-constructor aliases *)
	  val SOME consId = BEnv.findVar (bEnv, N.listCons)
	  val bEnv = BEnv.insertVal (bEnv, Atom.atom "CONS", consId)
	(* insert the primitive exceptions *)
	  val SOME exnTyId = BEnv.findTy(bEnv, N.exn)
	  fun insExn (ex as Types.DCon{name, ...}, (bEnv, mEnv)) = let
		val exId = newVar name
		in (
		  BEnv.insertDataCon(bEnv, name, exId, exnTyId),
		  MEnv.insertVar (mEnv, exId, MEnv.Con ex)
		) end
	  val (bEnv, mEnv) = List.foldl insExn (bEnv, mEnv) [
		  exnBind,
		  exnDiv,
		  exnFail,
		  exnMatch
		]
	  in
	    (bEnv, mEnv)
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
      val --> = AST.FunTy
      fun ** (t1, t2) = AST.TupleTy[t1, t2]
      infix 9 **
      infixr 8 -->
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

  (* seed the initial translation envirnment with a mapping from the primitive AST types to
   * their BOM equivalents.
   *)
(* NOTE: we might be able to get rid of this environment by using the mechanism that we
 * use to define the thread-ID type below.
 *)
    val primTranslationEnv = let
	    fun wrapTy rty = BOMTyUtil.wrap(BTy.T_Raw rty)
	    val env = TEnv.mkEnv()
	  (* insert a type constructor binding *)
	    fun insertTyc (tyc, k, bty) = let
		  val Types.Tyc{name, def, ...} = tyc
		  val SOME ptVar = BEnv.findTy(primBindingEnv, name)
	          in
		   (* bind the type for inline BOM (PT.var ---> BOM.ty) *)
		    TranslateEnv.insertBOMTyDef(ptVar, bty);
		    TranslateTypes.setTycKind (tyc, k);
		    case bty
		     of BTy.T_TyCon(BTy.DataTyc{kind, ...}) => kind := k
		      | _ => ()
		    (* end case *);
		    TEnv.insertTyc (env, tyc, bty)
                  end
          (* insert a data constructor binding *)
	    fun insertDataCon (name, dcon) = let
		  val SOME (BEnv.Con con) = BEnv.findVar(primBindingEnv, name)
		  val SOME (MEnv.Con con) = MEnv.findVar(primEnv, con)
	          in
		     TranslateEnv.insertCon(env, con, dcon)
		  end
	    in
	      List.app insertDataCon [
	          (N.boolTrue,   TranslateEnv.Const BOMBasis.boolTrue),
	          (N.boolFalse,  TranslateEnv.Const BOMBasis.boolFalse)
	      ];
	      List.app insertTyc [
	          (boolTyc,     BTy.K_UNBOXED,  BOMBasis.boolTy),
		  (intTyc,	BTy.K_BOXED,	wrapTy BTy.T_Int),
		  (longTyc,	BTy.K_BOXED,	wrapTy BTy.T_Long),
		  (floatTyc,	BTy.K_BOXED,	wrapTy BTy.T_Float),
		  (doubleTyc,	BTy.K_BOXED,	wrapTy BTy.T_Double),
(*		  (stringTyc,	BTy.K_BOXED,	BOMBasis.stringTy),*)
		  (exnTyc,	BTy.K_BOXED,	BTy.exnTy)
		];
	      env
	    end

  (* based on the given binding environment for "initial-basis.pml", create
   * the initial translation environment
   *)
    fun glueAST bEnv (initialAST, progAST) = let
	  fun bindVarToHLOp (x, e) = let
		val name = Var.nameOf x
		in
		  case BEnv.findBOMHLOp(bEnv, Atom.atom name)
		   of SOME id => AST.LetExp(
			AST.PrimVBind(x, ProgramParseTree.PML2.BOMParseTree.HLOpPrimVal id),
			e)
		    | NONE => (
			TextIO.output(TextIO.stdErr, concat[
			    "Warning: no HLOp for ", name, " found in binding environment\n"
			  ]);
			e)
		  (* end case *)
		end
	  val ast = List.foldr bindVarToHLOp progAST [
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
	  fun cat (AST.TD_Module _ :: r) = raise Fail "unexpected module in initial basis"
	    | cat (AST.TD_DCon _ :: r) = cat r
	    | cat (AST.TD_Binding b :: r) = AST.LetExp(b, cat r)
	    | cat [] = ast
	  in
	    cat initialAST
	  end

  (* given the environments produced from the "initial-basis.pml" file,
   * extend them with the operators and other primitive bindings.
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
		  (N.lte,	lte),
		  (N.lt,	lt),
		  (N.gte,	gte),
		  (N.gt,	gt),
		  (N.plus,	plus),
		  (N.minus,	minus),
		  (N.times,	times),
		  (N.fdiv,	fdiv),
		  (N.div,	div),
		  (N.mod,	mod),
		  (N.uMinus,	neg)
		]
	(* insert non-overloaded operators *)
	  fun insOp ((name, var), (bEnv, mEnv)) = let
		val id = newVar(Atom.atom(Var.nameOf var))
		in (
		  BEnv.insertVal(bEnv, name, BEnv.Var id),
		  MEnv.insertVar(mEnv, id, MEnv.Var var)
		) end
	  val (bEnv, mEnv) = List.foldl insOp (bEnv, mEnv) [
		  (N.append,	list_append),
		  (N.concat,	string_concat),
		  (N.psub,	parray_sub)
		]
	(* insert the equality operators *)
	  fun insEqOp ((name, var), (bEnv, mEnv)) = let
		val id = newVar(Atom.atom(Var.nameOf var))
		in (
		  BEnv.insertVal(bEnv, name, BEnv.Var id),
		  MEnv.insertVar(mEnv, id, MEnv.EqOp var)
		) end
	  val (bEnv, mEnv) = List.foldl insEqOp (bEnv, mEnv) [
		  (N.eq,	eq),
		  (N.neq,	neq)
		]
	(* add type bindings for abstract types *)
	  fun insTycBind (tyc, bomName) = let
		val pmlName = TyCon.nameOf tyc
		val SOME pmlId = BEnv.findTy(bEnv, pmlName)
		in
		  case BEnv.findBOMTy(bEnv, Atom.atom bomName)
		   of SOME id => let
			val bty = PPT.PML2.BOMParseTree.T_TyCon id
			in
			  MEnv.setRealizationOfTyc (tyc, MEnv.BOMTyDef bty);
			  MEnv.setPrimTyDef(pmlId, SOME bty)
			end
		    | NONE =>
			TextIO.output(TextIO.stdErr, concat[
			    "Warning: cannot find ", bomName,
			    " found in binding environment\n"
			  ])
		end
	  val () = List.app insTycBind [
		  (stringTyc, "ml_string"),
		  (threadIdTyc, "thread_id")
		]
	  in {
	    bEnv = bEnv,
	    mEnv = mEnv,
	    glueAST = glueAST bEnv
	  } end

  end
