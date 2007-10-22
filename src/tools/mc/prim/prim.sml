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
      = isBoxed of 'var
      | isUnboxed of 'var
      | Equal of 'var * 'var	(* equality on T_any *)
      | NotEqual of 'var * 'var	(* equality on T_any *)
      | BNot of 'var
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
      | I64Neg of 'var
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
      | F32Sqrt of 'var
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
      | F64Sqrt of 'var
      | F64Eq of 'var * 'var
      | F64NEq of 'var * 'var
      | F64Lt of 'var * 'var
      | F64Lte of 'var * 'var
      | F64Gt of 'var * 'var
      | F64Gte of 'var * 'var
    (* conversions *)
      | I32ToI64X of 'var		(* int -> long conversion with sign extension *)
      | I32ToI64 of 'var		(* unsigned int -> long conversion *)
      | I32ToF32 of 'var		(* int -> float conversion *)
      | I32ToF64 of 'var		(* int -> double conversion *)
      | I64ToF32 of 'var		(* long -> float conversion *)
      | I64ToF64 of 'var		(* long -> double conversion *)
    (* atomic operations *)
      | I32FetchAndAdd of 'var * 'var
      | CAS of 'var * 'var * 'var	(* compare and swap; returns old value *)
      | BCAS of 'var * 'var * 'var	(* compare and swap; returns bool *)

  end
