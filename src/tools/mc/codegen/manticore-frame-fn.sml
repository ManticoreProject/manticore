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

  val wordSzB = IntInf.toInt Spec.ABI.wordSzB
  val spillAreaOffB = IntInf.toInt Spec.ABI.spillAreaOffB

  fun frameOffset (Word i | Float i) = 
      if i > IntInf.toInt Spec.ABI.spillAreaSzB div wordSzB
      then raise Fail "the number of spilled variables has exceeded the space in the spill area"
      else ~(wordSzB * i) + ~spillAreaOffB
    | frameOffset _ = raise Fail "frameOffset"

end (* ManticoreFrameFn *)
