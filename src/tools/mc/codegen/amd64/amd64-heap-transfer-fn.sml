functor AMD64HeapTransferFn (
    structure Spec : TARGET_SPEC
    structure SpillLoc: SPILL_LOC
    structure AMD64MLTree : MLTREE
) = struct

  structure T = AMD64MLTree

  type stm = T.stm

  val wordSzB = IntInf.toInt Spec.ABI.wordSzB
  val memory = ManticoreRegion.memory
  val ty = wordSzB * 8
  fun regExp r = T.REG (ty, r)

  fun genGCCall () = [T.JMP (T.LABEL RuntimeLabels.initGC, [RuntimeLabels.initGC])]

end (* AMD64HeapTransferFn *)
