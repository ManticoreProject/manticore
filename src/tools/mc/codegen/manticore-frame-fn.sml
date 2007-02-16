(* manticore-frame-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

functor ManticoreFrameFn (
          val wordSz : int
	  val floatSz : int
	  val floatAlign : int
	  val linkageSz : int) : MANTICORE_FRAME = struct

  open ManticoreFrame

  fun frameOffset (Word i) = ~( wordSz * i )
    | frameOffset _ = raise Fail "frameOffset"

end (* ManticoreFrameFn *)
