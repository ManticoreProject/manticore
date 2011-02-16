(* ft-synth-ops.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * My flattening transformation includes type-indexed families of operators.
 * All these operators may be statically synthesized out a simple components.
 * This module performs the synthesis.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure FTSynthOps = struct

  structure F = FLAST
  structure R = RepresentationTypes
  structure T = FTTypes

  fun flatten (r : R.ty) : F.exp = raise Fail "todo"

end
