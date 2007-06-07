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
    val lookupVar : AST.var -> AST.lambda

  end = struct

    structure B = Basis
    structure P = Prim
    structure H = HLOpEnv
    structure TTbl = TyCon.Tbl
    structure BTy = BOMTy

    fun lookupDCon _ = raise Fail "lookupDCon"

    val types = [
	    (B.intTyc, BTy.T_Wrap BTy.T_Int),
	    (B.longTyc,  BTy.T_Wrap BTy.T_Int),
(*
	    (B.integerTyc,  ),
*)
	    (B.floatTyc,  BTy.T_Wrap BTy.T_Float),
	    (B.doubleTyc,  BTy.T_Wrap BTy.T_Double),
(*
	    (B.charTyc, ),
	    (B.runeTyc, ),
	    (B.stringTyc, ),
	    (B.parrayTyc, ),
*)
	    (B.threadIdTyc, BTy.tidTy)
	  ]

    val findTyc : Types.tycon -> BOMTy.ty option = let
	  val tbl = TTbl.mkTable(List.length types, Fail "tyc tbl")
	  in
	    List.app (TTbl.insert tbl) types;
	    TTbl.find tbl
	  end

    local
      fun unary (t1, t2) = BTy.T_Fun([t1], [], [t2])
      fun binary (t1, t2, t3) = BTy.T_Fun([t1, t2], [], [t3])
      val i = BTy.T_Raw BTy.T_Int
      val b = BTy.boolTy
      val i_i = unary(i, i)
      val ii_i = binary(i, i, i)
      val ii_b = binary (i, i, b)
    (* generate a variable for a primop; if the ty is raw, then we have to
     * unwrap the argument and so we generate two variables.
     *)
      fun unwrapArg (name, ty, stms) = let
	    val rawX = BV.new("_"^name, ty)
	    in
	      case ty
	       of BTy.T_Raw rty => let
		    val wrapTy = BTy.T_Wrap rty
		    val wrapX = BV.new("_w"^name, wrapTy)
		    in
		      (([rawX], B.E_Unwrap wrapX)::stms, rawX, wrapX, wrapTy)
		    end
		| _ => (stms, rawX, rawX, ty)
	      (* end case *)
	    end
      fun wrapRes ty = let
	    val rawX = BV.new("_res", ty)
	    in
	      case ty
	       of BTy.T_Raw rty => let
		    val wrapTy = BTy.T_Wrap rty
		    val wrapX = BV.new("_w"^name, wrapTy)
		    in
		      ([([wrapX], B.E_Wrap wrapX)], rawX, wrapX, wrapTy)
		    end
		| _ => ([], rawX, rawX, ty)
	      (* end case *)
	    end
      fun funTy (arg, res) = BTy.T_Fun([BV.typeOf arg], [BTy.exhTy], [BT.typeOf res])
      fun prim2 (rator, f, rawATy, rawBTy, rawResTy) = let
	    val (preStms, rawB, wrapB, wrapBTy) = unwrapArg ("b", rawBTy, preStms)
	    val (preStms, rawA, wrapA, wrapATy) = unwrapArg ("a", rawATy, [])
	    val preStms = ([wrapA], B.E_Select(0, arg))
		  :: ([wrapB], B.E_Select(1, arg))
		  :: preStms
	    val argTy = BTy.T_Tuple(false, wrapATy, wrapBTy)
	    val arg = BV.new("_arg", argTy)
	    val (postStms, rawRes, wrapRes, wrapResTy) = wrapRes rawResTy
	    val stms = ([res], B.E_Prim(mk(a, b)) :: stms
		  ]
	    val stms = preStms @ (([res], B.E_Prim(mk(a, b))) :: postStms)
	    in
	      B.FB{
		  f = BV.new(f, BTy.T_Fun([argTy], [BTy.exhTy], [wrapResTy]),
		  params=[arg],
		  exh=[BV.new("_exh", BTy.exhTy)],
		  body = B.mkStmts([
		      ([a], B.E_Select(0, arg)),
		      ([b], B.E_Select(1, arg)),
		      ([res], B.E_Prim(mk(a, b)))
		    ],
		    B.mkRet[res])
		}
	    end
    in
    val operators = [
	    (B.append, HLOp H.listAppendOp),
  
	    (B.int_lte, Prim2 P.I32Lte),
	    (B.float_lte, Prim2 P.F32Lte),
	    (B.double_lte, Prim2 P.F64Lte),
	    (B.long_lte, Prim2 P.I64Lte),
(*
	    (B.integer_lte, HLOp H.integerLteOp),
	    (B.char_lte, Prim2 P.?),
	    (B.rune_lte, Prim2 P.?),
	    (B.string_lte, HLOp H.stringLteOp),
*)
  
	    (B.int_lt, Prim2 P.I32Lt),
	    (B.float_lt, Prim2 P.F32Lt),
	    (B.double_lt, Prim2 P.F64Lt),
	    (B.long_lt, Prim2 P.I64Lt),
(*
	    (B.integer_lt, HLOp H.integerLtOp),
	    (B.char_lt, Prim2 P.?),
	    (B.rune_lt, Prim2 P.?),
	    (B.string_lt, HLOp H.stringLtOp),
*)
  
	    (B.int_gte, Prim2 P.I32Gte),
	    (B.float_gte, Prim2 P.F32Gte),
	    (B.double_gte, Prim2 P.F64Gte),
	    (B.long_gte, Prim2 P.I64Gte),
(*
	    (B.integer_gte, HLOp H.integerGteOp),
	    (B.char_gte, Prim2 P.?),
	    (B.rune_gte, Prim2 P.?),
	    (B.string_gte, HLOp H.stringGteOp),
*)
  
	    (B.int_gt, Prim2 P.I32Gt),
	    (B.float_gt, Prim2 P.F32Gt),
	    (B.double_gt, Prim2 P.F64Gt),
	    (B.long_gt, Prim2 P.I64Gt),
(*
	    (B.integer_gt, HLOp H.integerGtOp),
	    (B.char_gt, Prim2 P.?),
	    (B.rune_gt, Prim2 P.?),
	    (B.string_gt, HLOp H.stringGtOp),
*)

	    (B.int_plus, Prim2 P.I32Add),
	    (B.float_plus, Prim2 P.F32Add),
	    (B.double_plus, Prim2 P.F64Add),
	    (B.long_plus, Prim2 P.I64Add),
(*
	    (B.integer_plus, HLOp H.integerAddOp),
*)

	    (B.int_minus, Prim2 P.I32Sub),
	    (B.float_minus, Prim2 P.F32Sub),
	    (B.double_minus, Prim2 P.F64Sub),
	    (B.long_minus, Prim2 P.I64Sub),
(*
	    (B.integer_minus, HLOp H.integerSubOp),
*)

	    (B.int_times, Prim2 P.I32Mul),
	    (B.float_times, Prim2 P.F32Mul),
	    (B.double_times, Prim2 P.F64Mul),
	    (B.long_times, Prim2 P.I64Mul),
(*
	    (B.integer_times, HLOp H.integerMulOp),
*)

	    (B.int_div, Prim2 P.I32Div),
	    (B.long_div, Prim2 P.I64Div),
(*
	    (B.integer_div, HLOp H.integerDivOp),
*)

	    (B.int_mod, Prim2 P.I32Mod),
	    (B.long_mod, Prim2 P.I64Mod)
(*
	    (B.integer_mod, HLOp H.integerModOp)
*)
	  ]

    val lookupVar : AST.var -> builtin = let
	  val tbl = Var.Tbl.mkTable(List.length operators, Fail "var tbl")
	  in
	    List.app (Var.Tbl.insert tbl) operators;
	    fn x => (case Var.Tbl.find tbl x
	       of SOME b => b
		| NONE => raise Fail("unbound variable " ^ Var.toString x)
	      (* end case *))
	  end

  end
