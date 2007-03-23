(* manticore-frame-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

functor ManticoreFrameFn (
          structure Spec : TARGET_SPEC
) : MANTICORE_FRAME = struct

  open ManticoreFrame

  val wordSz = IntInf.toInt Spec.ABI.wordSzB
  val spillAreaOffB = IntInf.toInt Spec.ABI.spillAreaOffB

  fun frameOffset (Word i) = ~(wordSz * i) + ~spillAreaOffB
    | frameOffset _ = raise Fail "frameOffset"

end (* ManticoreFrameFn *)
