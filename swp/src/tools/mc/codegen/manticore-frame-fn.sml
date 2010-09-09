(* manticore-frame-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor ManticoreFrameFn (

    structure Spec : TARGET_SPEC

  ) : MANTICORE_FRAME = struct

    open ManticoreFrame

    val wordSz = IntInf.toInt Spec.ABI.wordSzB
    val spillAreaOffB = IntInf.toInt Spec.ABI.spillAreaOffB
    val nSpillSlots = (IntInf.toInt Spec.ABI.spillAreaSzB) div wordSz

    fun spillOffset i = (
	  if i >= nSpillSlots
	    then raise Fail "spill area exceeded"
	    else ();
	  ~(wordSz * i + spillAreaOffB))

    fun frameOffset (Word i) = spillOffset i
      | frameOffset (Float i) = spillOffset i
      | frameOffset _ = raise Fail "frameOffset"

  end (* ManticoreFrameFn *)
