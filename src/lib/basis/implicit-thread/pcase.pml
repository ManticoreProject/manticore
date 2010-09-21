(* pcase.pml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Underlying support for parallel case expressions.
 * See JFP paper submission (in final/jfp-datapar) for details.
 *)

structure PCase = struct

  val dispatch : unit -> 'a = raise Fail "todo: dispatch"

  val pcaseWrapper : (('a -> 'b) * (exn -> 'c)) -> 'a  = raise Fail "todo: pcaseWrapper"

end
