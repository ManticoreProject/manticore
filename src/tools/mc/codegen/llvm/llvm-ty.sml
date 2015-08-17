(* llvm-ty.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Provides a single definition of an LLVM type used in various places.
 * Use LLVMTy.t when talking about an LLVM type.
 *)

structure LLVMTy = struct

  structure HC = HashCons
  structure HCInt = HashConsGroundFn (
  struct
    type hash_key = int
    val sameKey = (op = : int * int -> bool)

    (* QUESTION(kavon): is this hashVal okay or do we need to hash the integer with
                 some sort of prime number multiplication? *)
    val hashVal = Word.fromInt 

  end)

  (* must use the ctor functions defined below *)
  type count = HCInt.obj
  datatype t_node = 
      T_Void
    | T_VProc
    | T_Deque
    | T_Label
    | T_Func of t list  (* first element is the return type *)
    | T_Int of count (* number of bits *)
    | T_Float
    | T_Double
    | T_Ptr of t
    | T_Vector of count * t
    | T_Array of count * t
    | T_Struct of t list  (* packed struct *)
    withtype t = t_node HC.obj  
  
end
