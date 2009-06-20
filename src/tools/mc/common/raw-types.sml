(* raw-ty.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Raw machine-level numeric types.
 *)

structure RawTypes =
  struct

    datatype raw_ty
      = T_Byte | T_Short | T_Int | T_Long
      | T_Float | T_Double
      | T_Vec128

    fun sizeOf T_Byte = 1
      | sizeOf T_Short = 2
      | sizeOf T_Int = 4
      | sizeOf T_Long = 8
      | sizeOf T_Float = 4
      | sizeOf T_Double = 8
      | sizeOf T_Vec128 = 16

    fun toString T_Byte = "byte"
      | toString T_Short = "short"
      | toString T_Int = "int"
      | toString T_Long = "long"
      | toString T_Float = "float"
      | toString T_Double = "double"
      | toString T_Vec128 = "vec128"

  end
