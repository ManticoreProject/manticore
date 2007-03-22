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
    val explode : 'var Prim.prim -> (('a list -> 'a Prim.prim) * 'var list)
    val map : ('a -> 'b) -> 'a Prim.prim -> 'b Prim.prim

  end = struct

    structure P = Prim

    fun nameOf (P.isBoxed _) = "isBoxed"
      | nameOf (P.isUnboxed _) = "isUnboxed"
      | nameOf (P.BNot _) = "BNot"
      | nameOf (P.BEq _) = "BEq"
      | nameOf (P.BNEq _) = "BNEq"
      | nameOf (P.I32Add _) = "I32Add"
      | nameOf (P.I32Sub _) = "I32Sub"
      | nameOf (P.I32Mul _) = "I32Mul"
      | nameOf (P.I32Div _) = "I32Div"
      | nameOf (P.I32Mod _) = "I32Mod"
      | nameOf (P.I32Neg _) = "I32Neg"
      | nameOf (P.I32Eq _) = "I32Eq"
      | nameOf (P.I32NEq _) = "I32NEq"
      | nameOf (P.I32Lt _) = "I32Lt"
      | nameOf (P.I32Lte _) = "I32Lte"
      | nameOf (P.I32Gt _) = "I32Gt"
      | nameOf (P.I32Gte _) = "I32Gte"
      | nameOf (P.I64Add _) = "I64Add"
      | nameOf (P.I64Sub _) = "I64Sub"
      | nameOf (P.I64Mul _) = "I64Mul"
      | nameOf (P.I64Div _) = "I64Div"
      | nameOf (P.I64Mod _) = "I64Mod"
      | nameOf (P.I64Neg _) = "I64Neg"
      | nameOf (P.I64Eq _) = "I64Eq"
      | nameOf (P.I64NEq _) = "I64NEq"
      | nameOf (P.I64Lt _) = "I64Lt"
      | nameOf (P.I64Lte _) = "I64Lte"
      | nameOf (P.I64Gt _) = "I64Gt"
      | nameOf (P.I64Gte _) = "I64Gte"
      | nameOf (P.F32Add _) = "F32Add"
      | nameOf (P.F32Sub _) = "F32Sub"
      | nameOf (P.F32Mul _) = "F32Mul"
      | nameOf (P.F32Div _) = "F32Div"
      | nameOf (P.F32Neg _) = "F32Neg"
      | nameOf (P.F32Eq _) = "F32Eq"
      | nameOf (P.F32NEq _) = "F32NEq"
      | nameOf (P.F32Lt _) = "F32Lt"
      | nameOf (P.F32Lte _) = "F32Lte"
      | nameOf (P.F32Gt _) = "F32Gt"
      | nameOf (P.F32Gte _) = "F32Gte"
      | nameOf (P.F64Add _) = "F64Add"
      | nameOf (P.F64Sub _) = "F64Sub"
      | nameOf (P.F64Mul _) = "F64Mul"
      | nameOf (P.F64Div _) = "F64Div"
      | nameOf (P.F64Neg _) = "F64Neg"
      | nameOf (P.F64Eq _) = "F64Eq"
      | nameOf (P.F64NEq _) = "F64NEq"
      | nameOf (P.F64Lt _) = "F64Lt"
      | nameOf (P.F64Lte _) = "F64Lte"
      | nameOf (P.F64Gt _) = "F64Gt"
      | nameOf (P.F64Gte _) = "F64Gte"

  (* return the list of variables referenced in a primitive operation *)
    fun varsOf (P.isBoxed a) = [a]
      | varsOf (P.isUnboxed a) = [a]
      | varsOf (P.BNot a) = [a]
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

    fun fmt v2s p = (case varsOf p
	   of [x] => concat[nameOf p, "(", v2s x, ")"]
	    | [x, y] => concat[nameOf p, "(", v2s x, ",", v2s y, ")"]
	  (* end case *))

    local
      fun p1 p [a] = p(a)
	| p1 p _ = raise Fail "unary primop needs one arg"
      fun p2 p [a, b] = p(a, b)
	| p2 p _ = raise Fail "binary primop needs two args"
    in
    fun explode (P.isBoxed a) = (p1 P.isBoxed, [a])
      | explode (P.isUnboxed a) = (p1 P.isUnboxed, [a])
      | explode (P.BNot a) = (p1 P.BNot, [a])
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

    fun map f p = let val (mk, args) = explode p in mk(List.map f args) end

  end
