(* manticore-frame-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

functor ManticoreFrameFn (
          structure Spec : TARGET_SPEC
(*          val wordSz : int
	  val floatSz : int
	  val floatAlign : int
	  val linkageSz : int *)) : MANTICORE_FRAME = struct

  open ManticoreFrame

  val wordSz = IntInf.toInt Spec.ABI.wordSzB
  val spillAreaOff = IntInf.toInt Spec.ABI.spillAreaOff

  fun frameOffset (Word i) = ~(wordSz * (i+spillAreaOff))
    | frameOffset _ = raise Fail "frameOffset"

end (* ManticoreFrameFn *)
