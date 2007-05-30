(* std-env.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Mapping from AST types and variables to their BOL representations.
 *)

structure StdEnv : sig

    datatype builtin
      = Prim1 of BOM.var -> BOM.prim
      | Prim2 of BOM.var * BOM.var -> BOM.prim
      | HLOp of HLOp.hlop

    val lookupTy : Types.tycon -> BOMTy.ty
    val lookupDCon : Types.dcon -> BOMTy.data_con
    val lookupVar : AST.var -> builtin

  end = struct

    structure B = Basis
    structure H = HLOpEnv

    datatype builtin
      = Prim1 of BOM.var -> BOM.prim
      | Prim2 of BOM.var * BOM.var -> BOM.prim
      | HLOp of HLOp.hlop

    val operators = [
	  (B.append, HLOp H.listappendOp),

	  (B.int_lte, Prim2 P.I32Lte),
	  (B.float_lte, Prim2 P.F32Lte),
	  (B.double_lte, Prim2 P.F64Lte),
	  (B.long_lte, Prim2 P.I64Lte),
	  (B.integer_lte, HLOp H.integerLteOp),
	  (B.char_lte, Prim2 P.?),
	  (B.rune_lte, Prim2 P.?),
	  (B.string_lte, HLOp H.stringLteOp),

	  (B.int_lt, Prim2 P.I32Lt),
	  (B.float_lt, Prim2 P.F32Lt),
	  (B.double_lt, Prim2 P.F64Lt),
	  (B.long_lt, Prim2 P.I64Lt),
	  (B.integer_lt, HLOp H.integerLtOp),
	  (B.char_lt, Prim2 P.?),
	  (B.rune_lt, Prim2 P.?),
	  (B.string_lt, HLOp H.stringLtOp),

	  (B.int_gte, Prim2 P.I32Gte),
	  (B.float_gte, Prim2 P.F32Gte),
	  (B.double_gte, Prim2 P.F64Gte),
	  (B.long_gte, Prim2 P.I64Gte),
	  (B.integer_gte, HLOp H.integerGteOp),
	  (B.char_gte, Prim2 P.?),
	  (B.rune_gte, Prim2 P.?),
	  (B.string_gte, HLOp H.stringGteOp),

	  (B.int_gt, Prim2 P.I32Gt),
	  (B.float_gt, Prim2 P.F32Gt),
	  (B.double_gt, Prim2 P.F64Gt),
	  (B.long_gt, Prim2 P.I64Gt),
	  (B.integer_gt, HLOp H.integerGtOp),
	  (B.char_gt, Prim2 P.?),
	  (B.rune_gt, Prim2 P.?),
	  (B.string_gt, HLOp H.stringGtOp),

	  (B.int_plus, Prim2 P.I32Add),
	  (B.float_plus, Prim2 P.F32Add),
	  (B.double_plus, Prim2 P.F64Add),
	  (B.long_plus, Prim2 P.I64Add),
	  (B.integer_plus, HLOp H.integerAddOp),

	  (B.int_minus, Prim2 P.I32Sub),
	  (B.float_minus, Prim2 P.F32Sub),
	  (B.double_minus, Prim2 P.F64Sub),
	  (B.long_minus, Prim2 P.I64Sub),
	  (B.integer_minus, HLOp H.integerSubOp),

	  (B.int_times, Prim2 P.I32Mul),
	  (B.float_times, Prim2 P.F32Mul),
	  (B.double_times, Prim2 P.F64Mul),
	  (B.long_times, Prim2 P.I64Mul),
	  (B.integer_times, HLOp H.integerMulOp),

	  (B.int_div, Prim2 P.I32Div),
	  (B.long_div, Prim2 P.I64Div),
	  (B.integer_div, HLOp H.integerDivOp),

	  (B.int_mod, Prim2 P.I32Mod),
	  (B.long_mod, Prim2 P.I64Mod),
	  (B.integer_mod, HLOp H.integerModOp)
	]

  end
