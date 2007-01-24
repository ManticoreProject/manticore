(* cfg-ty.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for the first-order CPS representation.
 *)

structure CFGTy =
  struct

    datatype ty
      = T_Any			(* unknown type; uniform representation *)
      | T_Raw of raw_ty		(* raw machine type *)
      | T_Fun of ty list	(* function/continuation *)

    and raw_ty
      = T_Byte | T_Short | T_Int | T_Long
      | T_Float | T_Double

  end
