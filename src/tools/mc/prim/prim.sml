(* prim.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This is a placeholder until we get primop generation working.  It includes
 * booleans, 32 and 64-bit integers, and 32 and 64-bit floating-point numbers.
 *)

structure Prim =
  struct

    datatype 'var prim
      = BNot of 'var
      | BEq of 'var * 'var
      | BNEq of 'var * 'var
      | I32Add of 'var * 'var
      | I32Sub of 'var * 'var
      | I32Mul of 'var * 'var
      | I32Div of 'var * 'var
      | I32Mod of 'var * 'var
      | I32Neg of 'var
      | I32Eq of 'var * 'var
      | I32NEq of 'var * 'var
      | I32Lt of 'var * 'var
      | I32Lte of 'var * 'var
      | I32Gt of 'var * 'var
      | I32Gte of 'var * 'var
      | I64Add of 'var * 'var
      | I64Sub of 'var * 'var
      | I64Mul of 'var * 'var
      | I64Div of 'var * 'var
      | I64Mod of 'var * 'var
      | I64Neg of 'var * 'var
      | I64Eq of 'var * 'var
      | I64NEq of 'var * 'var
      | I64Lt of 'var * 'var
      | I64Lte of 'var * 'var
      | I64Gt of 'var * 'var
      | I64Gte of 'var * 'var
      | F32Add of 'var * 'var
      | F32Sub of 'var * 'var
      | F32Mul of 'var * 'var
      | F32Div of 'var * 'var
      | F32Neg of 'var
      | F32Eq of 'var * 'var
      | F32NEq of 'var * 'var
      | F32Lt of 'var * 'var
      | F32Lte of 'var * 'var
      | F32Gt of 'var * 'var
      | F32Gte of 'var * 'var
      | F64Add of 'var * 'var
      | F64Sub of 'var * 'var
      | F64Mul of 'var * 'var
      | F64Div of 'var * 'var
      | F64Neg of 'var
      | F64Eq of 'var * 'var
      | F64NEq of 'var * 'var
      | F64Lt of 'var * 'var
      | F64Lte of 'var * 'var
      | F64Gt of 'var * 'var
      | F64Gte of 'var * 'var

    fun fmt v2s p = (case p
	   of BNot x => concat["BNot(", v2s x, ")"]
	    | BEq(x, y) => concat["BEq(", v2s x, ",", v2s y, ")"]
	    | BNEq(x, y) => concat["BNEq(", v2s x, ",", v2s y, ")"]
	    | I32Add(x, y) => concat["I32Add(", v2s x, ",", v2s y, ")"]
	    | I32Sub(x, y) => concat["I32Sub(", v2s x, ",", v2s y, ")"]
	    | I32Mul(x, y) => concat["I32Mul(", v2s x, ",", v2s y, ")"]
	    | I32Div(x, y) => concat["I32Div(", v2s x, ",", v2s y, ")"]
	    | I32Mod(x, y) => concat["I32Mod(", v2s x, ",", v2s y, ")"]
	    | I32Neg x => concat["I32Neg(", v2s x, ")"]
	    | I32Eq(x, y) => concat["I32Eq(", v2s x, ",", v2s y, ")"]
	    | I32NEq(x, y) => concat["I32NEq(", v2s x, ",", v2s y, ")"]
	    | I32Lt(x, y) => concat["I32Lt(", v2s x, ",", v2s y, ")"]
	    | I32Lte(x, y) => concat["I32Lte(", v2s x, ",", v2s y, ")"]
	    | I32Gt(x, y) => concat["I32Gt(", v2s x, ",", v2s y, ")"]
	    | I32Gte(x, y) => concat["I32Gte(", v2s x, ",", v2s y, ")"]
	    | I64Add(x, y) => concat["I64Add(", v2s x, ",", v2s y, ")"]
	    | I64Sub(x, y) => concat["I64Sub(", v2s x, ",", v2s y, ")"]
	    | I64Mul(x, y) => concat["I64Mul(", v2s x, ",", v2s y, ")"]
	    | I64Div(x, y) => concat["I64Div(", v2s x, ",", v2s y, ")"]
	    | I64Mod(x, y) => concat["I64Mod(", v2s x, ",", v2s y, ")"]
	    | I64Neg(x, y) => concat["I64Neg(", v2s x, ",", v2s y, ")"]
	    | I64Eq(x, y) => concat["I64Eq(", v2s x, ",", v2s y, ")"]
	    | I64NEq(x, y) => concat["I64NEq(", v2s x, ",", v2s y, ")"]
	    | I64Lt(x, y) => concat["I64Lt(", v2s x, ",", v2s y, ")"]
	    | I64Lte(x, y) => concat["I64Lte(", v2s x, ",", v2s y, ")"]
	    | I64Gt(x, y) => concat["I64Gt(", v2s x, ",", v2s y, ")"]
	    | I64Gte(x, y) => concat["I64Gte(", v2s x, ",", v2s y, ")"]
	    | F32Add(x, y) => concat["F32Add(", v2s x, ",", v2s y, ")"]
	    | F32Sub(x, y) => concat["F32Sub(", v2s x, ",", v2s y, ")"]
	    | F32Mul(x, y) => concat["F32Mul(", v2s x, ",", v2s y, ")"]
	    | F32Div(x, y) => concat["F32Div(", v2s x, ",", v2s y, ")"]
	    | F32Neg x => concat["F32Neg(", v2s x, ")"]
	    | F32Eq(x, y) => concat["F32Eq(", v2s x, ",", v2s y, ")"]
	    | F32NEq(x, y) => concat["F32NEq(", v2s x, ",", v2s y, ")"]
	    | F32Lt(x, y) => concat["F32Lt(", v2s x, ",", v2s y, ")"]
	    | F32Lte(x, y) => concat["F32Lte(", v2s x, ",", v2s y, ")"]
	    | F32Gt(x, y) => concat["F32Gt(", v2s x, ",", v2s y, ")"]
	    | F32Gte(x, y) => concat["F32Gte(", v2s x, ",", v2s y, ")"]
	    | F64Add(x, y) => concat["F64Add(", v2s x, ",", v2s y, ")"]
	    | F64Sub(x, y) => concat["F64Sub(", v2s x, ",", v2s y, ")"]
	    | F64Mul(x, y) => concat["F64Mul(", v2s x, ",", v2s y, ")"]
	    | F64Div(x, y) => concat["F64Div(", v2s x, ",", v2s y, ")"]
	    | F64Neg x => concat["F64Neg(", v2s x, ")"]
	    | F64Eq(x, y) => concat["F64Eq(", v2s x, ",", v2s y, ")"]
	    | F64NEq(x, y) => concat["F64NEq(", v2s x, ",", v2s y, ")"]
	    | F64Lt(x, y) => concat["F64Lt(", v2s x, ",", v2s y, ")"]
	    | F64Lte(x, y) => concat["F64Lte(", v2s x, ",", v2s y, ")"]
	    | F64Gt(x, y) => concat["F64Gt(", v2s x, ",", v2s y, ")"]
	    | F64Gte(x, y) => concat["F64Gte(", v2s x, ",", v2s y, ")"]
	  (* end case *))

  end
