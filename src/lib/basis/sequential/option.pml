(* option.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * FIXME Option.option is different from option!
 *)

structure Option =
  struct

    exception Option

    datatype option = datatype option

(*
    fun valOf opt = (
	  case opt
	   of NONE => raise Option
	    | SOME x => x
          (* end case *))
*)

  end
