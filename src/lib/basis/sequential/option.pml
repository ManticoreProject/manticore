(* FIXME Option.option is different from option! *)

structure Option =
  struct

    exception Option

    datatype 'a option =
	     NONE
	   | SOME of 'a
(*
    fun valOf opt = (
	  case opt
	   of NONE => raise Option
	    | SOME x => x
          (* end case *))
*)

  end
