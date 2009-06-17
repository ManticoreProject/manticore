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
			  BEnv.insertAbsTyc(bEnv, name, id),
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
			  BEnv.insertDataTyc(bEnv, name, id),
			  MEnv.insertTy (mEnv, id, MEnv.TyCon tyc)
			) end
		  (* end case *)
		end
	  val (bEnv, mEnv) = List.foldl insTyc (bEnv, mEnv) [
		  Basis.boolTyc,
		  Basis.listTyc,
		  Basis.exnTyc,
		  Basis.intTyc,
		  Basis.longTyc,
		  Basis.integerTyc,
		  Basis.floatTyc,
		  Basis.doubleTyc,
		  Basis.charTyc,
		  Basis.runeTyc,
		  Basis.stringTyc,
		  Basis.chanTyc,
		  Basis.ivarTyc,
		  Basis.mvarTyc,
		  Basis.eventTyc,
		  Basis.threadIdTyc
		]
	(* add primitive types *)
	  fun insTy ((name, ty), (bEnv, mEnv)) = let
		val id = newVar name
		in (
		  BEnv.insertTy(bEnv, name, id),
		  MEnv.insertTy(mEnv, id, MEnv.TyDef ty)
		) end
	  val (bEnv, mEnv) = List.foldl insTy (bEnv, mEnv) [
		  (N.unit, Types.TyScheme([], Basis.unitTy))
		]
	(* add data-constructor aliases *)
	  val SOME consId = BEnv.findVal (bEnv, N.listCons)
	  val bEnv = BEnv.insertVal (bEnv, Atom.atom "CONS", consId)
	(* insert the primitive exceptions *)
	  val SOME(BEnv.DataTyc exnTyId) = BEnv.findTy(bEnv, N.exn)
	  fun insExn (ex as Types.DCon{name, ...}, (bEnv, mEnv)) = let
		val exId = newVar name
		in (
		  BEnv.insertDataCon(bEnv, name, exId, exnTyId),
		  MEnv.insertVar (mEnv, exId, MEnv.Con ex)
		) end
	  val (bEnv, mEnv) = List.foldl insExn (bEnv, mEnv) [
		  Basis.exnBind,
		  Basis.exnDiv,
		  Basis.exnFail,
		  Basis.exnMatch
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
    val lte = (
	    tyScheme(Types.Order, fn tv => (tv ** tv --> Basis.boolTy)),
	    [
	      Basis.int_lte,
	      Basis.long_lte,
	      Basis.integer_lte,
	      Basis.float_lte,
	      Basis.double_lte,
	      Basis.char_lte,
	      Basis.rune_lte,
	      Basis.string_lte
	    ])
    val lt = (
	    tyScheme(Types.Order, fn tv => (tv ** tv --> Basis.boolTy)),
	    [
	      Basis.int_lt,
	      Basis.long_lt,
	      Basis.integer_lt,
	      Basis.float_lt,
	      Basis.double_lt,
	      Basis.char_lt,
	      Basis.rune_lt,
	      Basis.string_lt
	    ])
    val gte = (
	    tyScheme(Types.Order, fn tv => (tv ** tv --> Basis.boolTy)),
	    [
	      Basis.int_gte,
	      Basis.long_gte,
	      Basis.integer_gte,
	      Basis.float_gte,
	      Basis.double_gte,
	      Basis.char_gte,
	      Basis.rune_gte,
	      Basis.string_gte
	    ])
    val gt = (
	    tyScheme(Types.Order, fn tv => (tv ** tv --> Basis.boolTy)),
	    [
	      Basis.int_gt,
	      Basis.long_gt,
	      Basis.integer_gt,
	      Basis.float_gt,
	      Basis.double_gt,
	      Basis.char_gt,
	      Basis.rune_gt,
	      Basis.string_gt
	    ])

    val plus = (
	    tyScheme(Types.Num, fn tv => (tv ** tv --> tv)),
	    [
	      Basis.int_plus,
	      Basis.long_plus,
	      Basis.integer_plus,
	      Basis.float_plus,
	      Basis.double_plus
	    ])
    val minus = (
	    tyScheme(Types.Num, fn tv => (tv ** tv --> tv)),
	    [
	      Basis.int_minus,
	      Basis.long_minus,
	      Basis.integer_minus,
	      Basis.float_minus,
	      Basis.double_minus
	    ])
    val times = (
	    tyScheme(Types.Num, fn tv => (tv ** tv --> tv)),
	    [
	      Basis.int_times,
	      Basis.long_times,
	      Basis.integer_times,
	      Basis.float_times,
	      Basis.double_times
	    ])

    val fdiv = (
	    tyScheme(Types.Float, fn tv => (tv ** tv --> tv)),
	    [
	      Basis.float_fdiv,
	      Basis.double_fdiv
	    ])

    val div = (
	    tyScheme(Types.Int, fn tv => (tv ** tv --> tv)),
	    [
	      Basis.int_div,
	      Basis.long_div,
	      Basis.integer_div
	    ])
    val mod = (
	    tyScheme(Types.Int, fn tv => (tv ** tv --> tv)),
	    [
	      Basis.int_mod,
	      Basis.long_mod,
	      Basis.integer_mod
	    ])

    val neg = (
	    tyScheme(Types.Num, fn tv => (tv --> tv)),
	    [
	      Basis.int_neg,
	      Basis.long_neg,
	      Basis.integer_neg,
	      Basis.float_neg,
	      Basis.double_neg
	    ])
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
		    TranslateEnv.insertBOMTyDef(BEnv.tyId ptVar, bty);
		    TranslateTypes.setTycKind (tyc, k);
		    case bty
		     of BTy.T_TyCon(BTy.DataTyc{kind, ...}) => kind := k
		      | _ => ()
		    (* end case *);
		    TEnv.insertTyc (env, tyc, bty)
                  end
          (* insert a data constructor binding *)
	    fun insertDataCon (name, dcon) = let
		  val SOME (BEnv.Con con) = BEnv.findVal(primBindingEnv, name)
		  val SOME (MEnv.Con con) = MEnv.findVar(primEnv, con)
	          in
		     TranslateEnv.insertCon(env, con, dcon)
		  end
	    in
(* FIXME: boolTyc does not map to BTy.boolTy!!! *)
	      List.app insertDataCon [
	          (N.boolTrue,   TranslateEnv.Lit(Literal.trueLit, BTy.boolTy)),
	          (N.boolFalse,  TranslateEnv.Lit(Literal.falseLit, BTy.boolTy))
	      ];
	      List.app insertTyc [
(* FIXME: boolTyc does not map to BTy.boolTy!!! *)
	          (Basis.boolTyc,	BTy.K_UNBOXED,  BTy.boolTy),
		  (Basis.intTyc,	BTy.K_BOXED,	wrapTy BTy.T_Int),
		  (Basis.longTyc,	BTy.K_BOXED,	wrapTy BTy.T_Long),
		  (Basis.floatTyc,	BTy.K_BOXED,	wrapTy BTy.T_Float),
		  (Basis.doubleTyc,	BTy.K_BOXED,	wrapTy BTy.T_Double),
(*		  (Basis.stringTyc,	BTy.K_BOXED,	BOMBasis.stringTy),*)
		  (Basis.exnTyc,	BTy.K_BOXED,	BTy.exnTy)
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
		  Basis.int_div,
		  Basis.int_gt,
		  Basis.int_gte,
		  Basis.int_lt,
		  Basis.int_lte,
		  Basis.int_minus,
		  Basis.int_mod,
		  Basis.int_neg,
		  Basis.int_plus,
		  Basis.int_times,
		  Basis.long_div,
		  Basis.long_gt,
		  Basis.long_gte,
		  Basis.long_lt,
		  Basis.long_lte,
		  Basis.long_minus,
		  Basis.long_mod,
		  Basis.long_neg,
		  Basis.long_plus,
		  Basis.long_times,
		  Basis.integer_div,
		  Basis.integer_gt,
		  Basis.integer_gte,
		  Basis.integer_lt,
		  Basis.integer_lte,
		  Basis.integer_minus,
		  Basis.integer_mod,
		  Basis.integer_neg,
		  Basis.integer_plus,
		  Basis.integer_times,
		  Basis.float_fdiv,
		  Basis.float_gt,
		  Basis.float_gte,
		  Basis.float_lt,
		  Basis.float_lte,
		  Basis.float_minus,
		  Basis.float_neg,
		  Basis.float_plus,
		  Basis.float_times,
		  Basis.double_fdiv,
		  Basis.double_gt,
		  Basis.double_gte,
		  Basis.double_lt,
		  Basis.double_lte,
		  Basis.double_minus,
		  Basis.double_neg,
		  Basis.double_plus,
		  Basis.double_times,
		  Basis.char_gt,
		  Basis.char_gte,
		  Basis.char_lt,
		  Basis.char_lte,
		  Basis.rune_gt,
		  Basis.rune_gte,
		  Basis.rune_lt,
		  Basis.rune_lte,
		  Basis.string_gt,
		  Basis.string_gte,
		  Basis.string_lt,
		  Basis.string_lte,
		  Basis.list_append,
		  Basis.string_concat
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
		  (N.append,	Basis.list_append),
		  (N.concat,	Basis.string_concat)
		]
	(* insert the equality operators *)
	  fun insEqOp ((name, var), (bEnv, mEnv)) = let
		val id = newVar(Atom.atom(Var.nameOf var))
		in (
		  BEnv.insertVal(bEnv, name, BEnv.Var id),
		  MEnv.insertVar(mEnv, id, MEnv.EqOp var)
		) end
	  val (bEnv, mEnv) = List.foldl insEqOp (bEnv, mEnv) [
		  (N.eq,	Basis.eq),
		  (N.neq,	Basis.neq)
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
			  MEnv.setPrimTyDef(BEnv.tyId pmlId, SOME bty)
			end
		    | NONE =>
			TextIO.output(TextIO.stdErr, concat[
			    "Warning: cannot find ", bomName,
			    " found in binding environment\n"
			  ])
		end
	  val () = List.app insTycBind [
		  (Basis.stringTyc, "ml_string"),
		  (Basis.threadIdTyc, "thread_id")
		]
	  in {
	    bEnv = bEnv,
	    mEnv = mEnv,
	    glueAST = glueAST bEnv
	  } end

  end
