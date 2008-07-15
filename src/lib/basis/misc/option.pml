structure Option =
  struct

    exception Option

    datatype 'a opt =
	     ONONE
	   | OSOME of 'a

(*
    fun valOf opt = (
	  case opt
	   of NONE => raise Option
	    | SOME x => x
          (* end case *))
*)

  end
