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

(* FIXME make this a Memo.memo *)
  val cell : (unit -> Types.tycon) option ref = ref NONE

  fun setTyc getC = (cell := SOME getC)

  fun parrayTyc () = 
   (case !cell
      of NONE => raise Fail "compiler bug"
       | SOME getC => getC ())

  fun parrayTy ty = AST.ConTy ([ty], parrayTyc ())

end
