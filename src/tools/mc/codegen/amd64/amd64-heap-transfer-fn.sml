functor AMD64HeapTransferFn (
    structure Spec : TARGET_SPEC
    structure SpillLoc: SPILL_LOC
) = struct

  structure T = AMD64MLTree

  type stm = T.stm

  val wordSzB = Word.toInt Spec.wordSzB
  val memory = ManticoreRegion.memory
  val ty = wordSzB * 8
  fun regExp r = T.REG (ty, r)

  fun genGCCall () = 
      [T.JMP (T.LABEL RuntimeLabels.initGC, [RuntimeLabels.initGC])]
(*
      [T.CALL {funct=T.LABEL RuntimeLabels.initGC, targets=[],
	       defs=[], uses=[],
	       region=memory, pops=0}]
*)
end (* AMD64HeapTransferFn *)
