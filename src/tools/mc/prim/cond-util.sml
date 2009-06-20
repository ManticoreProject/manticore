(* cond-util.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This is a placeholder until we get primop generation working.
 *)

structure CondUtil : sig

    val nameOf : 'var Prim.cond -> string
    val fmt : ('var -> string) -> 'var Prim.cond -> string
    val varsOf : 'var Prim.cond -> 'var list
    val explode : 'var Prim.cond -> (('a list -> 'a Prim.cond) * 'var list)
    val map : ('a -> 'b) -> 'a Prim.cond -> 'b Prim.cond
    val app : ('a -> unit) -> 'a Prim.cond -> unit
    val isPure : 'var Prim.cond -> bool

  end = struct

    structure P = Prim

    fun nameOf (P.isBoxed _) = "isBoxed"
      | nameOf (P.isUnboxed _) = "isUnboxed"
      | nameOf (P.Equal _) = "Equal"
      | nameOf (P.NotEqual _) = "NotEqual"
      | nameOf (P.EnumEq _) = "EnumEq"
      | nameOf (P.EnumNEq _) = "EnumNEq"
      | nameOf (P.I32Eq _) = "I32Eq"
      | nameOf (P.I32NEq _) = "I32NEq"
      | nameOf (P.I32Lt _) = "I32Lt"
      | nameOf (P.I32Lte _) = "I32Lte"
      | nameOf (P.I32Gt _) = "I32Gt"
      | nameOf (P.I32Gte _) = "I32Gte"
      | nameOf (P.U32Lt _) = "U32Lt"
      | nameOf (P.I64Eq _) = "I64Eq"
      | nameOf (P.I64NEq _) = "I64NEq"
      | nameOf (P.I64Lt _) = "I64Lt"
      | nameOf (P.I64Lte _) = "I64Lte"
      | nameOf (P.I64Gt _) = "I64Gt"
      | nameOf (P.I64Gte _) = "I64Gte"
      | nameOf (P.U64Lt _) = "U64Lt"
      | nameOf (P.F32Eq _) = "F32Eq"
      | nameOf (P.F32NEq _) = "F32NEq"
      | nameOf (P.F32Lt _) = "F32Lt"
      | nameOf (P.F32Lte _) = "F32Lte"
      | nameOf (P.F32Gt _) = "F32Gt"
      | nameOf (P.F32Gte _) = "F32Gte"
      | nameOf (P.F64Eq _) = "F64Eq"
      | nameOf (P.F64NEq _) = "F64NEq"
      | nameOf (P.F64Lt _) = "F64Lt"
      | nameOf (P.F64Lte _) = "F64Lte"
      | nameOf (P.F64Gt _) = "F64Gt"
      | nameOf (P.F64Gte _) = "F64Gte"
      | nameOf (P.AdrEq _) = "AdrEq"
      | nameOf (P.AdrNEq _) = "AdrNEq"
      | nameOf (P.BCAS _) = "BCAS"
      | nameOf (P.TAS _) = "TAS"

  (* return the list of variables referenced in a primitive operation *)
    fun varsOf (P.isBoxed a) = [a]
      | varsOf (P.isUnboxed a) = [a]
      | varsOf (P.Equal(a, b)) = [a, b]
      | varsOf (P.NotEqual(a, b)) = [a, b]
      | varsOf (P.EnumEq(a, b)) = [a, b]
      | varsOf (P.EnumNEq(a, b)) = [a, b]
      | varsOf (P.I32Eq(a, b)) = [a, b]
      | varsOf (P.I32NEq(a, b)) = [a, b]
      | varsOf (P.I32Lt(a, b)) = [a, b]
      | varsOf (P.I32Lte(a, b)) = [a, b]
      | varsOf (P.I32Gt(a, b)) = [a, b]
      | varsOf (P.I32Gte(a, b)) = [a, b]
      | varsOf (P.U32Lt(a, b)) = [a, b]
      | varsOf (P.I64Eq(a, b)) = [a, b]
      | varsOf (P.I64NEq(a, b)) = [a, b]
      | varsOf (P.I64Lt(a, b)) = [a, b]
      | varsOf (P.I64Lte(a, b)) = [a, b]
      | varsOf (P.I64Gt(a, b)) = [a, b]
      | varsOf (P.I64Gte(a, b)) = [a, b]
      | varsOf (P.U64Lt(a, b)) = [a, b]
      | varsOf (P.F32Eq(a, b)) = [a, b]
      | varsOf (P.F32NEq(a, b)) = [a, b]
      | varsOf (P.F32Lt(a, b)) = [a, b]
      | varsOf (P.F32Lte(a, b)) = [a, b]
      | varsOf (P.F32Gt(a, b)) = [a, b]
      | varsOf (P.F32Gte(a, b)) = [a, b]
      | varsOf (P.F64Eq(a, b)) = [a, b]
      | varsOf (P.F64NEq(a, b)) = [a, b]
      | varsOf (P.F64Lt(a, b)) = [a, b]
      | varsOf (P.F64Lte(a, b)) = [a, b]
      | varsOf (P.F64Gt(a, b)) = [a, b]
      | varsOf (P.F64Gte(a, b)) = [a, b]
      | varsOf (P.AdrEq(a, b)) = [a, b]
      | varsOf (P.AdrNEq(a, b)) = [a, b]
      | varsOf (P.BCAS(a, b, c)) = [a, b, c]
      | varsOf (P.TAS a) = [a]

    fun fmt v2s p = (case varsOf p
	   of [x] => concat[nameOf p, "(", v2s x, ")"]
	    | [x, y] => concat[nameOf p, "(", v2s x, ",", v2s y, ")"]
	    | [x, y, z] => concat[nameOf p, "(", v2s x, ",", v2s y, ",", v2s z, ")"]
	  (* end case *))

    local
      fun p1 p [a] = p(a)
	| p1 p _ = raise Fail "unary condition needs one arg"
      fun p2 p [a, b] = p(a, b)
	| p2 p _ = raise Fail "binary condition needs two args"
      fun p3 p [a, b, c] = p(a, b, c)
	| p3 p _ = raise Fail "ternary condition needs three args"
    in
    fun explode (P.isBoxed a) = (p1 P.isBoxed, [a])
      | explode (P.isUnboxed a) = (p1 P.isUnboxed, [a])
      | explode (P.Equal(a, b)) = (p2 P.Equal, [a, b])
      | explode (P.NotEqual(a, b)) = (p2 P.NotEqual, [a, b])
      | explode (P.EnumEq(a, b)) = (p2 P.EnumEq, [a, b])
      | explode (P.EnumNEq(a, b)) = (p2 P.EnumNEq, [a, b])
      | explode (P.I32Eq(a, b)) = (p2 P.I32Eq, [a, b])
      | explode (P.I32NEq(a, b)) = (p2 P.I32NEq, [a, b])
      | explode (P.I32Lt(a, b)) = (p2 P.I32Lt, [a, b])
      | explode (P.I32Lte(a, b)) = (p2 P.I32Lte, [a, b])
      | explode (P.I32Gt(a, b)) = (p2 P.I32Gt, [a, b])
      | explode (P.I32Gte(a, b)) = (p2 P.I32Gte, [a, b])
      | explode (P.U32Lt(a, b)) = (p2 P.U64Lt, [a, b])
      | explode (P.I64Eq(a, b)) = (p2 P.I64Eq, [a, b])
      | explode (P.I64NEq(a, b)) = (p2 P.I64NEq, [a, b])
      | explode (P.I64Lt(a, b)) = (p2 P.I64Lt, [a, b])
      | explode (P.I64Lte(a, b)) = (p2 P.I64Lte, [a, b])
      | explode (P.I64Gt(a, b)) = (p2 P.I64Gt, [a, b])
      | explode (P.I64Gte(a, b)) = (p2 P.I64Gte, [a, b])
      | explode (P.U64Lt(a, b)) = (p2 P.U64Lt, [a, b])
      | explode (P.F32Eq(a, b)) = (p2 P.F32Eq, [a, b])
      | explode (P.F32NEq(a, b)) = (p2 P.F32NEq, [a, b])
      | explode (P.F32Lt(a, b)) = (p2 P.F32Lt, [a, b])
      | explode (P.F32Lte(a, b)) = (p2 P.F32Lte, [a, b])
      | explode (P.F32Gt(a, b)) = (p2 P.F32Gt, [a, b])
      | explode (P.F32Gte(a, b)) = (p2 P.F32Gte, [a, b])
      | explode (P.F64Eq(a, b)) = (p2 P.F64Eq, [a, b])
      | explode (P.F64NEq(a, b)) = (p2 P.F64NEq, [a, b])
      | explode (P.F64Lt(a, b)) = (p2 P.F64Lt, [a, b])
      | explode (P.F64Lte(a, b)) = (p2 P.F64Lte, [a, b])
      | explode (P.F64Gt(a, b)) = (p2 P.F64Gt, [a, b])
      | explode (P.F64Gte(a, b)) = (p2 P.F64Gte, [a, b])
      | explode (P.AdrEq(a, b)) = (p2 P.AdrEq, [a, b])
      | explode (P.AdrNEq(a, b)) = (p2 P.AdrNEq, [a, b])
      | explode (P.BCAS(a, b, c)) = (p3 P.BCAS, [a, b, c])
      | explode (P.TAS a) = (p1 P.TAS, [a])
    end (* local *)

    fun map f p = let val (mk, args) = explode p in mk(List.map f args) end

    fun app f p = List.app f (varsOf p)

    fun isPure (P.BCAS _) = false
      | isPure (P.TAS _) = false
      | isPure _ = true

  end
