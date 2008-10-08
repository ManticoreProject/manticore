(* std-env.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Mapping from AST types and variables to their BOL representations.
 *
 * FIXME: we need to cleanup the way this stuff is handled.  Right now, things are being defined
 * all over the place (some in the compiler and some in prototypes.hlop).
 *)

structure StdEnv : sig

    val env : unit -> TranslateEnv.env

  end = struct

    structure B = Basis
    structure RB = RuntimeBasis
    structure P = Prim
    structure H = HLOpEnv
    structure TTbl = TyCon.Tbl
    structure BTy = BOMTy
    structure BV = BOM.Var
    structure RB = RuntimeBasis
    structure E = TranslateEnv
    structure R = Ropes
    structure U = UnseenBasis 

  (***** Predefined types *****)

    (* wrapTy : RawTypes.raw_ty -> BTy.ty *)
    fun wrapTy rty = BOMTyUtil.wrap(BTy.T_Raw rty)

    val types = [
	    (B.boolTyc,		BTy.K_UNBOXED,	BOMBasis.boolTy),
	    (B.intTyc,		BTy.K_BOXED,	wrapTy BTy.T_Int),
	    (B.longTyc,		BTy.K_BOXED,	wrapTy BTy.T_Long),
(*
	    (B.integerTyc,  ),
*)
	    (B.floatTyc,	BTy.K_BOXED,	wrapTy BTy.T_Float),
	    (B.doubleTyc,	BTy.K_BOXED,	wrapTy BTy.T_Double),
(*
	    (B.charTyc, ),
	    (B.runeTyc, ),
*)
	    (B.stringTyc,	BTy.K_BOXED,	BOMBasis.stringTy),
	    (B.listTyc,		BTy.K_UNIFORM,	BOMBasis.listTy),
	    (B.optionTyc,	BTy.K_UNIFORM,	BOMBasis.optionTy),
	    (B.exnTyc,		BTy.K_BOXED,	BTy.exnTy),
	    (B.threadIdTyc,	BOMTyUtil.kindOf(BTy.tidTy), BTy.tidTy)
	  ]


  (***** Predefined data constructors *****)

  (* a data-constructor binding for constructors with unflattened arguments *)
    fun mkDCon dc = let
	  val rep = (case BOMTyCon.dconArgTy dc
		 of [ty] => FlattenRep.ATOM ty
		  | tys => FlattenRep.TUPLE(tys, List.map FlattenRep.ATOM tys)
		(* end case *))
	  in
	    E.DCon(dc, rep)
	  end

    val dcons = [
	    (B.boolFalse,	E.Const BOMBasis.boolFalse),
	    (B.boolTrue,	E.Const BOMBasis.boolTrue),
	    (B.listNil,		E.Const BOMBasis.listNil),
	    (B.listCons,	mkDCon BOMBasis.listCons),
	    (B.optionNONE,	E.Const BOMBasis.optionNONE),
	    (B.optionSOME,	mkDCon BOMBasis.optionSOME)
	  ]

    (* mkCast : BOM.exp * BOMTy.ty * BOMTy.ty -> BOM.exp *)
      fun mkCast (e, origTy, newTy) =
	  (* FIXME *)
	  if BOMTyUtil.equal (origTy, newTy)
                 (* this really should be match, but that wasn't working *)
	  then e (* cast is unnecessary *)
	  else 
	      let val x = BV.new ("x", origTy)
		  val c = BV.new ("c", newTy)
	      in
		  BOM.mkLet ([x], e,
                    BOM.mkStmt ([c], BOM.E_Cast (newTy, x),
                      BOM.mkRet [c]))
	      end

    (* create wrapper code for a high-level operation.
     *
     *		hlop (h, hasPolyResult) resTy
     *
     * returns a BOM lambda that is the wrapper for the operator h.  The hasPolyResult
     * argument is a boolean flag that is true when the result type of the high-level
     * operation is polymorphic.  In this case, the resTy argument specifies the result
     * type as determined by the context of use.  Otherwise, resTy is ignored.
     *)
      fun hlop (hlop as HLOp.HLOp{name, sign, ...}, hasPolyResult) = let
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
(* FIXME: perhaps it would be more robust to compare resTy with ty in mkFB and add the
 * cast when they are different?
 *)
	    val castResult = if hasPolyResult
		  then let
		    val resTy = (case results of [r] => r | _ => raise Fail "resTy")
		    in
		      fn (h, ty) => mkCast(h, resTy, ty)
		    end
		  else fn (h, ty) => h
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

  (***** Predefined operators *****)
    local 
    (* generate a variable for a primop; if the ty is raw, then we have to
     * unwrap the argument and so we generate two variables.
     *)

     (* unwrapArg : string * BTy.ty * (BV.var list * BOM.rhs) list 
                    -> ((BV.var list * BOM.rhs) list * BV.var * BOM.var * BTy.ty) *)
      fun unwrapArg (name, ty, stms) = let
	    val rawX = BV.new("_"^name, ty)
	    in
	      case ty
	       of BTy.T_Raw rty => let
		    val wrapTy = wrapTy rty
		    val wrapX = BV.new("_w"^name, wrapTy)
		    in
		      (([rawX], BOM.unwrap wrapX)::stms, rawX, wrapX, wrapTy)
		    end
		| _ => (stms, rawX, rawX, ty)
	      (* end case *)
	    end

      (* wrapRes : BV.V.ty 
                   -> ((BV.V.var list * BOM.rhs) list 
                        * BV.V.var * BV.V.var * BV.V.ty) *)
      fun wrapRes ty = let
	    val rawX = BV.new("_res", ty)
	    in
	      case ty
	       of BTy.T_Raw rty => let
		    val wrapTy = wrapTy rty
		    val wrapX = BV.new("_wres", wrapTy)
		    in
		      ([([wrapX], BOM.wrap rawX)], rawX, wrapX, wrapTy)
		    end
		| _ => ([], rawX, rawX, ty)
	      (* end case *)
	    end

    (* funTy : BV.var * BV.var -> BTy.ty *)
      fun funTy (arg, res) = BTy.T_Fun([BV.typeOf arg], [BTy.exhTy], [BV.typeOf res])

    (* create a lambda generator for a unary BOM primop. *)
      fun prim1 (rator, f, rawArgTy, rawResTy) _ = let
	    val (preStms, rawArg, wrapArg, wrapArgTy) = unwrapArg ("arg", rawArgTy, [])
	    val (postStms, rawRes, wrapRes, wrapResTy) = wrapRes rawResTy
	    val stms = preStms @ (([rawRes], BOM.E_Prim(rator rawArg)) :: postStms)
	    in
	      BOM.FB{
		  f = BV.new(f, BTy.T_Fun([wrapArgTy], [BTy.exhTy], [wrapResTy])),
		  params = [wrapArg],
		  exh = [BV.new("_exh", BTy.exhTy)],
		  body = BOM.mkStmts(stms, BOM.mkRet[wrapRes])
		}
	    end

    (* create a lambda generator for a binary BOM primop. *)
      fun prim2 (rator, f, rawATy, rawBTy, rawResTy) _ = let
	    val (preStms, rawB, wrapB, wrapBTy) = unwrapArg ("b", rawBTy, [])
	    val (preStms, rawA, wrapA, wrapATy) = unwrapArg ("a", rawATy, preStms)
	    val argTy = BTy.T_Tuple(false, [wrapATy, wrapBTy])
	    val arg = BV.new("_arg", argTy)
	    val preStms = ([wrapA], BOM.E_Select(0, arg))
		  :: ([wrapB], BOM.E_Select(1, arg))
		  :: preStms
	    val (postStms, rawRes, wrapRes, wrapResTy) = wrapRes rawResTy
	    val stms = preStms @ (([rawRes], BOM.E_Prim(rator(rawA, rawB))) :: postStms)
	    in
	      BOM.FB{
		  f = BV.new(f, BTy.T_Fun([argTy], [BTy.exhTy], [wrapResTy])),
		  params = [arg],
		  exh = [BV.new("_exh", BTy.exhTy)],
		  body = BOM.mkStmts(stms, BOM.mkRet[wrapRes])
		}
	    end

    (* type shorthands *)
      val i = BTy.T_Raw BTy.T_Int
      val l = BTy.T_Raw BTy.T_Long
      val f = BTy.T_Raw BTy.T_Float
      val d = BTy.T_Raw BTy.T_Double
      val b = BOMBasis.boolTy
    in 
    val operators = [
	    (B.listAppend,	hlop (H.listAppendOp, false)),	    
	    (B.stringConcat,	hlop (H.stringConcatOp, false)),
	    (B.int_lte,		prim2 (P.I32Lte, "lte", i, i, b)),
	    (B.long_lte,	prim2 (P.I64Lte, "lte", l, l, b)),
	    (B.float_lte,	prim2 (P.F32Lte, "lte", f, f, b)),
	    (B.double_lte,	prim2 (P.F64Lte, "lte", d, d, b)),
(* FIXME
	    (B.integer_lte,	hlop H.integerLteOp),
	    (B.char_lte,	prim2 (P., "lte", ?, ?, ?)?),
	    (B.rune_lte,	prim2 (P., "lte", ?, ?, ?)?),
	    (B.string_lte,	hlop (H.stringLteOp, false)),  
*)
	    (B.int_lt,		prim2 (P.I32Lt, "lt", i, i, b)),
	    (B.float_lt,	prim2 (P.F32Lt, "lt", f, f, b)),
	    (B.double_lt,	prim2 (P.F64Lt, "lt", d, d, b)),
	    (B.long_lt,		prim2 (P.I64Lt, "lt", l, l, b)),
(* FIXME
	    (B.integer_lt,	hlop H.integerLtOp),
	    (B.char_lt,		prim2 (P., "lt", ?, ?, ?)?),
	    (B.rune_lt,		prim2 (P., "lt", ?, ?, ?)?),
	    (B.string_lt,	hlop H.stringLtOp),
*)
  
	    (B.int_gte,		prim2 (P.I32Gte, "gte", i, i, b)),
	    (B.float_gte,	prim2 (P.F32Gte, "gte", f, f, b)),
	    (B.double_gte,	prim2 (P.F64Gte, "gte", d, d, b)),
	    (B.long_gte,	prim2 (P.I64Gte, "gte", l, l, b)),
(* FIXME
	    (B.integer_gte,	hlop H.integerGteOp),
	    (B.char_gte,	prim2 (P., "gte", ?, ?, ?)?),
	    (B.rune_gte,	prim2 (P., "gte", ?, ?, ?)?),
	    (B.string_gte,	hlop H.stringGteOp),
*)
  
	    (B.int_gt,		prim2 (P.I32Gt, "gt", i, i, b)),
	    (B.float_gt,	prim2 (P.F32Gt, "gt", f, f, b)),
	    (B.double_gt,	prim2 (P.F64Gt, "gt", d, d, b)),
	    (B.long_gt,		prim2 (P.I64Gt, "gt", l, l, b)),
(* FIXME
	    (B.integer_gt,	hlop H.integerGtOp),
	    (B.char_gt,		prim2 (P., "gt", ?, ?, ?)?),
	    (B.rune_gt,		prim2 (P., "gt", ?, ?, ?)?),
	    (B.string_gt,	hlop H.stringGtOp),
*)

	    (B.int_plus,	prim2 (P.I32Add, "plus", i, i, i)),
	    (B.float_plus,	prim2 (P.F32Add, "plus", f, f, f)),
	    (B.double_plus,	prim2 (P.F64Add, "plus", d, d, d)),
	    (B.long_plus,	prim2 (P.I64Add, "plus", l, l, l)),
(*
	    (B.integer_plus,	hlop H.integerAddOp),
*)

	    (B.int_minus,	prim2 (P.I32Sub, "minus", i, i, i)),
	    (B.float_minus,	prim2 (P.F32Sub, "minus", f, f, f)),
	    (B.double_minus,	prim2 (P.F64Sub, "minus", d, d, d)),
	    (B.long_minus,	prim2 (P.I64Sub, "minus", l, l, l)),
(*
	    (B.integer_minus,	hlop H.integerSubOp),
*)

	    (B.int_times,	prim2 (P.I32Mul, "times", i, i, i)),
	    (B.float_times,	prim2 (P.F32Mul, "times", f, f, f)),
	    (B.double_times,	prim2 (P.F64Mul, "times", d, d, d)),
	    (B.long_times,	prim2 (P.I64Mul, "times", l, l, l)),
(*
	    (B.integer_times,	hlop H.integerMulOp),
*)

(* FIXME: these should really be HLOps that check for divide by zero *)
	    (B.int_div,		prim2 (P.I32Div, "div", i, i, i)),
	    (B.long_div,	prim2 (P.I64Div, "div", l, l, l)),
(*
	    (B.integer_div,	hlop H.integerDivOp),
*)
	    (B.float_fdiv,	prim2 (P.F32Div, "fdiv", f, f, f)),
	    (B.double_fdiv,	prim2 (P.F64Div, "fdiv", d, d, d)),

(* FIXME: these should really be HLOps that check for divide by zero *)
	    (B.int_mod,		prim2 (P.I32Mod, "mod", i, i, i)),
	    (B.long_mod,	prim2 (P.I64Mod, "mod", l, l, l)),
(* FIXME
	    (B.integer_mod,	hlop H.integerModOp),
*)

	    (B.int_neg,		prim1 (P.I32Neg, "neg", i, i)),
	    (B.long_neg,	prim1 (P.I64Neg, "neg", l, l)),
(* FIXME
	    (B.integer_neg,	prim1 (P.I32Neg, "neg", i, i)),
*)
	    (B.sqrtf,	        prim1 (P.F32Sqrt, "sqrtf", f, f)),
	    (B.sqrtd,	        prim1 (P.F64Sqrt, "sqrtd", d, d)),

	    (B.absf,	        prim1 (P.F32Abs, "absf", f, f)),
	    (B.absd,	        prim1 (P.F64Abs, "absd", d, d)),

	    (B.float_neg,	prim1 (P.F32Neg, "neg", f, f)),
	    (B.double_neg,	prim1 (P.F64Neg, "neg", d, d))
	  ]

  (* predefined functions *)
    val predefs : (AST.var * (BOMTy.ty -> BOM.lambda)) list = [
	    (B.not,		prim1 (P.BNot, "not", b, b)),
	    (B.itof,		prim1 (P.I32ToF32, "itof", i, f)),
	    (B.itod,		prim1 (P.I32ToF64, "itod", i, d))
	  ]

    end (* local *) 

  (* create the initial environment *)
    val env0 = let
	  val env = E.mkEnv()
	(* insert a type constructor binding *)
	  fun insertTyc (tyc, k, bty) = (
		TranslateTypes.setTycKind (tyc, k);
		case bty
		 of BTy.T_TyCon(BTy.DataTyc{kind, ...}) => kind := k
		  | _ => ()
		(* end case *);
		E.insertTyc (env, tyc, bty))
        (* insert a lambda binding *)
	  fun insertFun ((x, lambda), env) = E.insertFun (env, x, lambda)
	(* insert primitive operator definitions *)
	  val env = List.foldl insertFun env operators
	(* insert high-level operator definitions *)
	  val env = List.foldl insertFun env predefs
	  in
	    List.app insertTyc types;
	    List.app (fn (dc, bdc) => E.insertCon (env, dc, bdc)) dcons;
	    env
	  end

  (* enrich env0 with HLOP signatures from prototypes.hlop *)
    fun env () = let
	  val hlops =  [
                  (B.app,               "list-app",		false),
		  (B.print,		"print",		false),
		  (B.stringConcat,	"string-concat2",	false),
                  (B.stringConcatWith,  "string-concat-with",	false),
		  (B.itos,		"itos",			false),
		  (B.fail,		"fail",			true),
		  (B.length,            "list-length",          false),
		  (B.rev,               "list-rev",		false),
		  (B.nth,               "list-nth",             true),
		  (B.powf,              "float-pow",		false),
		  (B.sinf,              "float-tan",		false),
		  (B.cosf,              "float-tan",		false),
		  (B.tanf,              "float-tan",		false),
		  (B.powd,              "double-pow",		false),
		  (B.sind,              "double-sin",		false),
		  (B.cosd,              "double-cos",		false),
		  (B.tand,              "double-tan",		false),
                  (B.compose,           "compose",		true),
		  (B.foldl,             "list-foldl",		true),
		  (B.foldr,             "list-foldr",		true),
		  (B.map,               "list-map",		false),
                  (B.filter,            "list-filter",          false),
		  (B.tab,               "list-tab",		false),
		  (B.gettimeofday,	"gettimeofday",		false),
		  (B.readint,	        "read-int",		false),
		  (B.readfloat,	        "read-float",		false),
		  (B.readdouble,        "read-double",		false),
		  (B.drand,	        "drand",		false),
		  (B.papp,              "rope-app",		false),
                  (B.plen,              "rope-length",		false),
                  (B.prev,              "rope-rev",             false),
                  (B.pdivide,           "rope-divide",          false),
                  (B.psubseq,           "rope-subseq",          false),
                  (B.pappend,           "rope-unbal-append",    false),
		  (U.mapP,              "rope-map",		false),
		  (B.reduceP,           "rope-reduce",		true),
		  (B.sumP,              "rope-sum",		false),
		  (B.dist,              "dist",                 false),
		  (U.tabD,              "tabD",			false),
		  (B.todo,              "todo",			false),

     	         (* ivars *)
		  (B.iVar,              "ivar-new",              true),
		  (B.iGet,              "ivar-get",              true),
		  (B.iPut,              "ivar-put",              false),

                 (* continuations *)
                  (RB.callcc,            "callcc",               false),
		  (RB.throwcc,           "throwcc",              false),

		  (RB.threadExit,        "exit",                 true),

                 (* lazy task creation *)
                  (RB.ltcPop,            "ltc-pop",              false),
                  (RB.ltcPush,           "ltc-push",             false),
		  (RB.ltcIPut,           "ltc-ivar-put",         false),
(* EXPERIMENTATION *)
                  (RB.promoteOnly,       "promote-only",             false),
		  (B.ltcWaitForAll,     "ltc-wait-for-all",     false),

		  (B.por,              "p-or-hook",                true),		  

	         (* arrays *)
		  (B.array,              "array",                true),
		  (B.aupdate,            "array-update",         false),
		  (B.asub,               "array-sub",            true),
		  (B.alength,            "array-length",         false)
		]  
	  fun ins ((x, n, polyResTy), env) = (case H.find (Atom.atom n)
		of NONE => raise Fail ("cannot find hlop " ^ n)
		 | SOME hop => E.insertFun (env, x, hlop (hop, polyResTy))
		(* end case *))
	  in
(*
	    HLOpDefLoader.loadPrototypes ();
	    List.foldl ins env0 hlops
*)
	    env0
	  end

  end
