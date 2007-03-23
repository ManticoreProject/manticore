(* amd64-frame.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

functor AMD64FrameFn (structure Spec : TARGET_SPEC) = struct 

  structure Frame = ManticoreFrameFn ( structure Spec = Spec )
  open Frame

end (* AMD64Frame *)
