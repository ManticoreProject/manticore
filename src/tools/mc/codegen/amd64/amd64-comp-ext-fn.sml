(* amd64-comp-ext-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor AMD64CompExtFn (
    structure I : AMD64INSTR where T.Extension = AMD64Extension
    structure TS : MLTREE_STREAM where T = I.T
    structure CFG : CONTROL_FLOW_GRAPH where I = I and P = TS.S.P
  ) : MLTREE_EXTENSION_COMP =
  struct

    structure T = TS.T
    structure TS = TS
    structure I = I
    structure CFG = CFG
    structure C = I.C

    structure CompInstrExt = AMD64CompInstrExt (
      structure I = I
      structure TS = TS
      structure CFG = CFG)

    type reducer =
	  (I.instruction,C.cellset,I.operand,I.addressing_mode,CFG.cfg) TS.reducer

    val compileSext = CompInstrExt.compileSext

    fun compileRext _ = raise Fail "AMD64CompExtFn.compileRext"
    fun compileFext _ = raise Fail "AMD64CompExtFn.compileFext"
    fun compileCCext _ = raise Fail "AMD64CompExtFn.compileCCext"

  end
