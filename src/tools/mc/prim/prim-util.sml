(* prim-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This is a placeholder until we get primop generation working.  It includes
 * booleans, 32 and 64-bit integers, and 32 and 64-bit floating-point numbers.
 *)

structure PrimUtil : sig

    val fmt : ('var -> string) -> 'var Prim.prim -> string
    val varsOf : 'var Prim.prim -> 'var list
    val explode : 'var Prim.prim -> (('var list -> 'var Prim.prim) * 'var list)

  end = struct

    structure P = Prim

    fun fmt v2s p = (case p
	   of P.BNot x => concat["BNot(", v2s x, ")"]
	    | P.BEq(x, y) => concat["BEq(", v2s x, ",", v2s y, ")"]
	    | P.BNEq(x, y) => concat["BNEq(", v2s x, ",", v2s y, ")"]
	    | P.I32Add(x, y) => concat["I32Add(", v2s x, ",", v2s y, ")"]
	    | P.I32Sub(x, y) => concat["I32Sub(", v2s x, ",", v2s y, ")"]
	    | P.I32Mul(x, y) => concat["I32Mul(", v2s x, ",", v2s y, ")"]
	    | P.I32Div(x, y) => concat["I32Div(", v2s x, ",", v2s y, ")"]
	    | P.I32Mod(x, y) => concat["I32Mod(", v2s x, ",", v2s y, ")"]
	    | P.I32Neg x => concat["I32Neg(", v2s x, ")"]
	    | P.I32Eq(x, y) => concat["I32Eq(", v2s x, ",", v2s y, ")"]
	    | P.I32NEq(x, y) => concat["I32NEq(", v2s x, ",", v2s y, ")"]
	    | P.I32Lt(x, y) => concat["I32Lt(", v2s x, ",", v2s y, ")"]
	    | P.I32Lte(x, y) => concat["I32Lte(", v2s x, ",", v2s y, ")"]
	    | P.I32Gt(x, y) => concat["I32Gt(", v2s x, ",", v2s y, ")"]
	    | P.I32Gte(x, y) => concat["I32Gte(", v2s x, ",", v2s y, ")"]
	    | P.I64Add(x, y) => concat["I64Add(", v2s x, ",", v2s y, ")"]
	    | P.I64Sub(x, y) => concat["I64Sub(", v2s x, ",", v2s y, ")"]
	    | P.I64Mul(x, y) => concat["I64Mul(", v2s x, ",", v2s y, ")"]
	    | P.I64Div(x, y) => concat["I64Div(", v2s x, ",", v2s y, ")"]
	    | P.I64Mod(x, y) => concat["I64Mod(", v2s x, ",", v2s y, ")"]
	    | P.I64Neg x => concat["I64Neg(", v2s x, ")"]
	    | P.I64Eq(x, y) => concat["I64Eq(", v2s x, ",", v2s y, ")"]
	    | P.I64NEq(x, y) => concat["I64NEq(", v2s x, ",", v2s y, ")"]
	    | P.I64Lt(x, y) => concat["I64Lt(", v2s x, ",", v2s y, ")"]
	    | P.I64Lte(x, y) => concat["I64Lte(", v2s x, ",", v2s y, ")"]
	    | P.I64Gt(x, y) => concat["I64Gt(", v2s x, ",", v2s y, ")"]
	    | P.I64Gte(x, y) => concat["I64Gte(", v2s x, ",", v2s y, ")"]
	    | P.F32Add(x, y) => concat["F32Add(", v2s x, ",", v2s y, ")"]
	    | P.F32Sub(x, y) => concat["F32Sub(", v2s x, ",", v2s y, ")"]
	    | P.F32Mul(x, y) => concat["F32Mul(", v2s x, ",", v2s y, ")"]
	    | P.F32Div(x, y) => concat["F32Div(", v2s x, ",", v2s y, ")"]
	    | P.F32Neg x => concat["F32Neg(", v2s x, ")"]
	    | P.F32Eq(x, y) => concat["F32Eq(", v2s x, ",", v2s y, ")"]
	    | P.F32NEq(x, y) => concat["F32NEq(", v2s x, ",", v2s y, ")"]
	    | P.F32Lt(x, y) => concat["F32Lt(", v2s x, ",", v2s y, ")"]
	    | P.F32Lte(x, y) => concat["F32Lte(", v2s x, ",", v2s y, ")"]
	    | P.F32Gt(x, y) => concat["F32Gt(", v2s x, ",", v2s y, ")"]
	    | P.F32Gte(x, y) => concat["F32Gte(", v2s x, ",", v2s y, ")"]
	    | P.F64Add(x, y) => concat["F64Add(", v2s x, ",", v2s y, ")"]
	    | P.F64Sub(x, y) => concat["F64Sub(", v2s x, ",", v2s y, ")"]
	    | P.F64Mul(x, y) => concat["F64Mul(", v2s x, ",", v2s y, ")"]
	    | P.F64Div(x, y) => concat["F64Div(", v2s x, ",", v2s y, ")"]
	    | P.F64Neg x => concat["F64Neg(", v2s x, ")"]
	    | P.F64Eq(x, y) => concat["F64Eq(", v2s x, ",", v2s y, ")"]
	    | P.F64NEq(x, y) => concat["F64NEq(", v2s x, ",", v2s y, ")"]
	    | P.F64Lt(x, y) => concat["F64Lt(", v2s x, ",", v2s y, ")"]
	    | P.F64Lte(x, y) => concat["F64Lte(", v2s x, ",", v2s y, ")"]
	    | P.F64Gt(x, y) => concat["F64Gt(", v2s x, ",", v2s y, ")"]
	    | P.F64Gte(x, y) => concat["F64Gte(", v2s x, ",", v2s y, ")"]
	  (* end case *))

  (* return the list of variables referenced in a primitive operation *)
    fun varsOf (P.BNot a) = [a]
      | varsOf (P.BEq(a, b)) = [a, b]
      | varsOf (P.BNEq(a, b)) = [a, b]
      | varsOf (P.I32Add(a, b)) = [a, b]
      | varsOf (P.I32Sub(a, b)) = [a, b]
      | varsOf (P.I32Mul(a, b)) = [a, b]
      | varsOf (P.I32Div(a, b)) = [a, b]
      | varsOf (P.I32Mod(a, b)) = [a, b]
      | varsOf (P.I32Neg a) = [a]
      | varsOf (P.I32Eq(a, b)) = [a, b]
      | varsOf (P.I32NEq(a, b)) = [a, b]
      | varsOf (P.I32Lt(a, b)) = [a, b]
      | varsOf (P.I32Lte(a, b)) = [a, b]
      | varsOf (P.I32Gt(a, b)) = [a, b]
      | varsOf (P.I32Gte(a, b)) = [a, b]
      | varsOf (P.I64Add(a, b)) = [a, b]
      | varsOf (P.I64Sub(a, b)) = [a, b]
      | varsOf (P.I64Mul(a, b)) = [a, b]
      | varsOf (P.I64Div(a, b)) = [a, b]
      | varsOf (P.I64Mod(a, b)) = [a, b]
      | varsOf (P.I64Neg a) = [a]
      | varsOf (P.I64Eq(a, b)) = [a, b]
      | varsOf (P.I64NEq(a, b)) = [a, b]
      | varsOf (P.I64Lt(a, b)) = [a, b]
      | varsOf (P.I64Lte(a, b)) = [a, b]
      | varsOf (P.I64Gt(a, b)) = [a, b]
      | varsOf (P.I64Gte(a, b)) = [a, b]
      | varsOf (P.F32Add(a, b)) = [a, b]
      | varsOf (P.F32Sub(a, b)) = [a, b]
      | varsOf (P.F32Mul(a, b)) = [a, b]
      | varsOf (P.F32Div(a, b)) = [a, b]
      | varsOf (P.F32Neg a) = [a]
      | varsOf (P.F32Eq(a, b)) = [a, b]
      | varsOf (P.F32NEq(a, b)) = [a, b]
      | varsOf (P.F32Lt(a, b)) = [a, b]
      | varsOf (P.F32Lte(a, b)) = [a, b]
      | varsOf (P.F32Gt(a, b)) = [a, b]
      | varsOf (P.F32Gte(a, b)) = [a, b]
      | varsOf (P.F64Add(a, b)) = [a, b]
      | varsOf (P.F64Sub(a, b)) = [a, b]
      | varsOf (P.F64Mul(a, b)) = [a, b]
      | varsOf (P.F64Div(a, b)) = [a, b]
      | varsOf (P.F64Neg a) = [a]
      | varsOf (P.F64Eq(a, b)) = [a, b]
      | varsOf (P.F64NEq(a, b)) = [a, b]
      | varsOf (P.F64Lt(a, b)) = [a, b]
      | varsOf (P.F64Lte(a, b)) = [a, b]
      | varsOf (P.F64Gt(a, b)) = [a, b]
      | varsOf (P.F64Gte(a, b)) = [a, b]

    local
      fun p1 p [a] = p(a)
	| p1 p _ = raise Fail "unary primop needs one arg"
      fun p2 p [a, b] = p(a, b)
	| p2 p _ = raise Fail "binary primop needs two args"
    in
    fun explode (P.BNot a) = (p1 P.BNot, [a])
      | explode (P.BEq(a, b)) = (p2 P.BEq, [a, b])
      | explode (P.BNEq(a, b)) = (p2 P.BNEq, [a, b])
      | explode (P.I32Add(a, b)) = (p2 P.I32Add, [a, b])
      | explode (P.I32Sub(a, b)) = (p2 P.I32Sub, [a, b])
      | explode (P.I32Mul(a, b)) = (p2 P.I32Mul, [a, b])
      | explode (P.I32Div(a, b)) = (p2 P.I32Div, [a, b])
      | explode (P.I32Mod(a, b)) = (p2 P.I32Mod, [a, b])
      | explode (P.I32Neg a) = (p1 P.I32Neg, [a])
      | explode (P.I32Eq(a, b)) = (p2 P.I32Eq, [a, b])
      | explode (P.I32NEq(a, b)) = (p2 P.I32NEq, [a, b])
      | explode (P.I32Lt(a, b)) = (p2 P.I32Lt, [a, b])
      | explode (P.I32Lte(a, b)) = (p2 P.I32Lte, [a, b])
      | explode (P.I32Gt(a, b)) = (p2 P.I32Gt, [a, b])
      | explode (P.I32Gte(a, b)) = (p2 P.I32Gte, [a, b])
      | explode (P.I64Add(a, b)) = (p2 P.I64Add, [a, b])
      | explode (P.I64Sub(a, b)) = (p2 P.I64Sub, [a, b])
      | explode (P.I64Mul(a, b)) = (p2 P.I64Mul, [a, b])
      | explode (P.I64Div(a, b)) = (p2 P.I64Div, [a, b])
      | explode (P.I64Mod(a, b)) = (p2 P.I64Mod, [a, b])
      | explode (P.I64Neg a) = (p1 P.I64Neg, [a])
      | explode (P.I64Eq(a, b)) = (p2 P.I64Eq, [a, b])
      | explode (P.I64NEq(a, b)) = (p2 P.I64NEq, [a, b])
      | explode (P.I64Lt(a, b)) = (p2 P.I64Lt, [a, b])
      | explode (P.I64Lte(a, b)) = (p2 P.I64Lte, [a, b])
      | explode (P.I64Gt(a, b)) = (p2 P.I64Gt, [a, b])
      | explode (P.I64Gte(a, b)) = (p2 P.I64Gte, [a, b])
      | explode (P.F32Add(a, b)) = (p2 P.F32Add, [a, b])
      | explode (P.F32Sub(a, b)) = (p2 P.F32Sub, [a, b])
      | explode (P.F32Mul(a, b)) = (p2 P.F32Mul, [a, b])
      | explode (P.F32Div(a, b)) = (p2 P.F32Div, [a, b])
      | explode (P.F32Neg a) = (p1 P.F32Neg, [a])
      | explode (P.F32Eq(a, b)) = (p2 P.F32Eq, [a, b])
      | explode (P.F32NEq(a, b)) = (p2 P.F32NEq, [a, b])
      | explode (P.F32Lt(a, b)) = (p2 P.F32Lt, [a, b])
      | explode (P.F32Lte(a, b)) = (p2 P.F32Lte, [a, b])
      | explode (P.F32Gt(a, b)) = (p2 P.F32Gt, [a, b])
      | explode (P.F32Gte(a, b)) = (p2 P.F32Gte, [a, b])
      | explode (P.F64Add(a, b)) = (p2 P.F64Add, [a, b])
      | explode (P.F64Sub(a, b)) = (p2 P.F64Sub, [a, b])
      | explode (P.F64Mul(a, b)) = (p2 P.F64Mul, [a, b])
      | explode (P.F64Div(a, b)) = (p2 P.F64Div, [a, b])
      | explode (P.F64Neg a) = (p1 P.F64Neg, [a])
      | explode (P.F64Eq(a, b)) = (p2 P.F64Eq, [a, b])
      | explode (P.F64NEq(a, b)) = (p2 P.F64NEq, [a, b])
      | explode (P.F64Lt(a, b)) = (p2 P.F64Lt, [a, b])
      | explode (P.F64Lte(a, b)) = (p2 P.F64Lte, [a, b])
      | explode (P.F64Gt(a, b)) = (p2 P.F64Gt, [a, b])
      | explode (P.F64Gte(a, b)) = (p2 P.F64Gte, [a, b])
    end (* local *)

  end
