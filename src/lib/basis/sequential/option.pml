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

    datatype 'a option 
      = NONE 
      | SOME of 'a

    fun isSome opt = (case opt
	   of NONE => false
	    | SOME _ => true
	  (* end case *))

(*
    fun valOf opt = (case opt
	   of NONE => raise Option
	    | SOME x => x
          (* end case *))
*)

  end
