(* parray.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure PArray : sig

  val setTyc    : (unit -> Types.tycon) -> unit
  val parrayTyc : unit -> Types.tycon
  val parrayTy  : AST.ty -> AST.ty

end = struct

  val thunkCell : (unit -> Types.tycon) option ref = ref NONE
  val tycCell   : Types.tycon option ref = ref NONE

  fun setTyc getC = (thunkCell := SOME getC)

  fun parrayTyc () = 
   (case !tycCell
      of NONE => 
          (case !thunkCell
	     of NONE => raise Fail "compiler bug"
	      | SOME getC => let
                  val c = getC ()
                  val _ = (tycCell := SOME c)
                  in
		    c
		  end)
       | SOME c => c)

  fun parrayTy ty = AST.ConTy ([ty], parrayTyc ())

end
