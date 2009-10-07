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

    fun valOf opt = 
     (case opt
        of NONE => (raise Fail "Option")
	 | SOME x => x
       (* end case *))

    fun getOpt (optX, default) =
     (case optX
        of NONE => default
	 | SOME x => x
       (* end case *))

    fun filter pred opt =
     (case opt
        of NONE => NONE
	 | SOME x => if pred x then SOME x else NONE
       (* end case *))

    fun join optopt =
     (case optopt
        of NONE => NONE
	 | SOME opt => opt
       (* end case *))

    fun app f opt =
     (case opt
        of NONE => ()
	 | SOME x => f x
       (* end case *))

    fun map f opt =
     (case opt
        of NONE => NONE
         | SOME x => SOME (f x)
       (* end case *))

    fun mapPartial f opt =
     (case opt
        of NONE => NONE
	 | SOME x => f x
       (* end case *))

    fun compose (f, g) =
     (fn a =>
       (case g a
	  of NONE => NONE
	   | SOME v => SOME (f v)
         (* end case *))
     (* end fn *))

    fun composePartial (f, g) = 
     (fn a =>
       (case g a
	  of NONE => NONE
	   | SOME v => f v
         (* end case *))
     (* end fn *))

  end
