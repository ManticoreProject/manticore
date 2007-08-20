(* std-env.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Mapping from AST types and variables to their BOL representations.
 *)

structure StdEnv : sig

    val findTyc : Types.tycon -> BOMTy.ty option
    val lookupDCon : Types.dcon -> BOMTy.data_con
    val lookupVar : AST.var -> BOM.lambda

  end = struct

    structure B = Basis
    structure P = Prim
    structure H = HLOpEnv
    structure TTbl = TyCon.Tbl
    structure BTy = BOMTy
    structure BV = BOM.Var
    structure F = Futures

    (* fail : string -> 'a *)
    fun fail msg = raise Fail msg

    (* todo : string -> 'a *)
    fun todo msg = fail ("todo: " ^ msg)

    fun lookupDCon _ = raise Fail "lookupDCon"

    (* wrapTy : RawTypes.raw_ty -> BTy.ty *)
    fun wrapTy rty = BTy.wrap(BTy.T_Raw rty)

    val types = [
	    (B.boolTyc,		BTy.boolTy),
	    (B.intTyc,		wrapTy BTy.T_Int),
	    (B.longTyc,		wrapTy BTy.T_Long),
(*
	    (B.integerTyc,  ),
*)
	    (B.floatTyc,	wrapTy BTy.T_Float),
	    (B.doubleTyc,	wrapTy BTy.T_Double),
(*
	    (B.charTyc, ),
	    (B.runeTyc, ),
	    (B.stringTyc, ),
*)
	    (B.threadIdTyc,	BTy.tidTy),
(*
	    (B.parrayTyc, ),
	    (B.chanTyc, ),
	    (B.ivarTyc, ),
	    (B.mvarTyc, ),
	    (B.eventTyc, ),
*)
	    (F.futureTyc,       BTy.futureTy)
	  ]

    val findTyc : Types.tycon -> BOMTy.ty option = let
	  val tbl = TTbl.mkTable(List.length types, Fail "tyc tbl")
	  in
	    List.app (TTbl.insert tbl) types;
	    TTbl.find tbl
	  end

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

      (* prim1 : (BV.V.var -> BOM.prim) * string  * BV.V.ty * BV.V.ty 
                  -> BOM.lambda *)
      fun prim1 (rator, f, rawArgTy, rawResTy) = let
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

      (* prim2 : (BV.V.var * BV.V.var -> BOM.prim) * string 
                 * BV.V.ty * BV.V.ty * BV.V.ty 
                 -> BOM.lambda *)
      fun prim2 (rator, f, rawATy, rawBTy, rawResTy) = let
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

      (* hlop : HLOp.hlop -> BOM.lambda *)
      fun hlop (hlop as HLOp.HLOp{name, sign, ...}) = let
	    val {params, exh, results} = sign
	    val paramTys = let
		  fun get (HLOp.PARAM t) = t
		    | get (HLOp.OPT t) = fail "hlop.get: OPT"
		    | get (HLOp.VEC t) = fail "hlop.get: VEC"
		  in
		    List.map get params
		  end
	      val fty = BTy.T_Fun (paramTys, exh, results)
	      val f = BV.new (Atom.toString name, fty)
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
	      val params = mkVars "arg" paramTys
	      val exh = mkVars "exh" exh
	      val body = BOM.mkHLOp (hlop, params, exh)
	      in
		BOM.FB{f=f, params=params, exh=exh, body=body}
	    end

    (* type shorthands *)
      val i = BTy.T_Raw BTy.T_Int
      val l = BTy.T_Raw BTy.T_Long
      val f = BTy.T_Raw BTy.T_Float
      val d = BTy.T_Raw BTy.T_Double
      val b = BTy.boolTy
    in 
    val operators = [

(* FIXME
	    (B.append,		hlop H.listAppendOp),
*)
	    (B.int_lte,		prim2 (P.I32Lte, "lte", i, i, b)),
	    (B.long_lte,	prim2 (P.I64Lte, "lte", l, l, b)),
	    (B.float_lte,	prim2 (P.F32Lte, "lte", f, f, b)),
	    (B.double_lte,	prim2 (P.F64Lte, "lte", d, d, b)),
(*
	    (B.integer_lte,	hlop H.integerLteOp),
	    (B.char_lte,	prim2 (P., "lte", ?, ?, ?)?),
	    (B.rune_lte,	prim2 (P., "lte", ?, ?, ?)?),
	    (B.string_lte,	hlop H.stringLteOp),
*)
  
	    (B.int_lt,		prim2 (P.I32Lt, "lt", i, i, b)),
	    (B.float_lt,	prim2 (P.F32Lt, "lt", f, f, b)),
	    (B.double_lt,	prim2 (P.F64Lt, "lt", d, d, b)),
	    (B.long_lt,		prim2 (P.I64Lt, "lt", l, l, b)),
(*
	    (B.integer_lt,	hlop H.integerLtOp),
	    (B.char_lt,		prim2 (P., "lt", ?, ?, ?)?),
	    (B.rune_lt,		prim2 (P., "lt", ?, ?, ?)?),
	    (B.string_lt,	hlop H.stringLtOp),
*)
  
	    (B.int_gte,		prim2 (P.I32Gte, "gte", i, i, b)),
	    (B.float_gte,	prim2 (P.F32Gte, "gte", f, f, b)),
	    (B.double_gte,	prim2 (P.F64Gte, "gte", d, d, b)),
	    (B.long_gte,	prim2 (P.I64Gte, "gte", l, l, b)),
(*
	    (B.integer_gte,	hlop H.integerGteOp),
	    (B.char_gte,	prim2 (P., "gte", ?, ?, ?)?),
	    (B.rune_gte,	prim2 (P., "gte", ?, ?, ?)?),
	    (B.string_gte,	hlop H.stringGteOp),
*)
  
	    (B.int_gt,		prim2 (P.I32Gt, "gt", i, i, b)),
	    (B.float_gt,	prim2 (P.F32Gt, "gt", f, f, b)),
	    (B.double_gt,	prim2 (P.F64Gt, "gt", d, d, b)),
	    (B.long_gt,		prim2 (P.I64Gt, "gt", l, l, b)),
(*
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
(*
	    (B.integer_mod,	hlop H.integerModOp),
*)

	    (B.int_neg,		prim1 (P.I32Neg, "neg", i, i)),
	    (B.long_neg,	prim1 (P.I64Neg, "neg", l, l)),
(*
	    (B.integer_neg,	prim1 (P.I32Neg, "neg", i, i)),
*)
	    (B.float_neg,	prim1 (P.F32Neg, "neg", f, f)),
	    (B.double_neg,	prim1 (P.F64Neg, "neg", d, d))
	  ]

  (* predefined functions *)
    val predefs = [
	    (B.not,		prim1 (P.BNot, "not", b, b)),
(*
	    (B.sqrtf,		hlop H.sqrtf),
	    (B.lnf,		hlop H.lnf),
	    (B.log2f,		hlop H.log2f),
	    (B.log10f,		hlop H.log10f),
	    (B.powf,		hlop H.powf),
	    (B.expf,		hlop H.expf),
	    (B.sinf,		hlop H.sinf),
	    (B.cosf,		hlop H.cosf),
	    (B.tanf,		hlop H.tanf),
*)
	    (B.itof,		prim1 (P.I32ToF32, "itof", i, f)),
(*
	    (B.sqrtd,		hlop H.sqrtd),
	    (B.lnd,		hlop H.lnd),
	    (B.log2d,		hlop H.log2d),
	    (B.log10d,		hlop H.log10d),
	    (B.powd,		hlop H.powd),
	    (B.expd,		hlop H.expd),
	    (B.sind,		hlop H.sind),
	    (B.cosd,		hlop H.cosd),
	    (B.tand,		hlop H.tand),
*)
	    (B.itod,		prim1 (P.I32ToF64, "itod", i, d)),
(*
	    (B.channel,		hlop H.channel),
	    (B.send,		hlop H.send),
	    (B.recv,		hlop H.recv),
	    (B.iVar,		hlop H.iVar),
	    (B.iGet,		hlop H.iGet),
	    (B.iPut,		hlop H.iPut),
	    (B.mVar,		hlop H.mVar),
	    (B.mGet,		hlop H.mGet),
	    (B.mTake,		hlop H.mTake),
	    (B.mPut,		hlop H.mPut),
	    (B.itos,		hlop H.itos),
	    (B.ltos,		hlop H.ltos),
	    (B.ftos,		hlop H.ftos),
	    (B.dtos,		hlop H.dtos),
	    (B.print,		hlop H.print),
	    (B.args,		hlop H.args),
	    (B.fail,		hlop H.fail),
*)

	    (F.future,          hlop H.futureOp),
	    (F.touch,           hlop H.touchOp),
	    (F.cancel,          hlop H.cancelOp)
	  ]
    end (* local *) 

    val lookupVar : AST.var -> BOM.lambda = let
	  val tbl = Var.Tbl.mkTable(List.length operators, Fail "var tbl")
	  in
	    List.app (Var.Tbl.insert tbl) operators;
	    List.app (Var.Tbl.insert tbl) predefs;
	    fn x => (case Var.Tbl.find tbl x
	       of SOME lambda => BOMUtil.copyLambda lambda
		| NONE => raise Fail("unbound variable " ^ Var.toString x)
	      (* end case *))
	  end

  end
