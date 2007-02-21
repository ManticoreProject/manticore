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

    fun equals (rty1, rty2) =
        case (rty1, rty2) of
            (T_Byte, T_Byte) => true
          | (T_Short, T_Short) => true
          | (T_Int, T_Int) => true
          | (T_Long, T_Long) => true
          | (T_Float, T_Float) => true
          | (T_Double, T_Double) => true
          | (T_Vec128, T_Vec128) => true
          | _ => false

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
