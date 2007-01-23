(* amd64-mltree.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Specialize the MLRISC tree to the AMD64.
 *)

structure AMD64MLTree = MLTreeF (
   structure Constant = AMD64Constant
   structure Region = ManticoreRegion
   structure Extension = AMD64Extension)

structure AMD64Instr = AMD64Instr (AMD64MLTree)

structure AMD64Shuffle = AMD64Shuffle (AMD64Instr)

structure AMD64MLTreeEval = MLTreeEval (
   structure T = AMD64MLTree
   fun eqSext _ _ = raise Fail "eqSext unimplemented"
   fun eqRext _ _ = raise Fail "eqRext unimplemented"
   fun eqFext _ _ = raise Fail "eqFext unimplemented"
   fun eqCCext _ _ = raise Fail "eqCCext unimplemented")
			    
structure AMD64GasPseudoOps = AMD64GasPseudoOps (
   structure T = AMD64MLTree
   structure MLTreeEval=AMD64MLTreeEval)

structure AMD64Imports = struct
  type import_kind = unit
  fun add _ = ()
  fun output _ = ""
end

structure AMD64PseudoOps = ManticorePseudoOpsFn (
   structure P=AMD64GasPseudoOps
   structure T=AMD64MLTree)

structure AMD64MLTreeHash =  MLTreeHash (
   structure T = AMD64MLTree
   fun h _ _ = 0w0
   val hashRext = h val hashFext = h
   val hashCCext = h val hashSext = h)

structure AMD64MLTreeEval = MLTreeEval (
   structure T = AMD64MLTree
   fun eq _ _ = false
   val eqRext = eq val eqFext = eq
   val eqCCext = eq val eqSext = eq)

structure AMD64Props = AMD64Props (
		        structure Instr = AMD64Instr
                        structure MLTreeHash = AMD64MLTreeHash
			structure MLTreeEval = AMD64MLTreeEval)

structure AMD64Stream = InstructionStream (AMD64PseudoOps.PseudoOps)

structure AMD64AsmEmit = AMD64AsmEmitter (
   structure Instr = AMD64Instr
   structure S = AMD64Stream
   structure MLTreeEval = AMD64MLTreeEval
   structure Shuffle = AMD64Shuffle
   structure MemRegs = struct
     structure I = AMD64Instr
     fun memReg _ = raise Fail "mem reg"
   end
   val memRegBase = NONE)

structure AMD64CFG = ControlFlowGraph (
   structure I = AMD64AsmEmit.I
   structure GraphImpl = DirectedGraph
   structure InsnProps = AMD64Props
   structure Asm = AMD64AsmEmit)

structure AMD64MLTStream = MLTreeStream (
		      structure T = AMD64MLTree
		      structure S = AMD64Stream )

structure AMD64MTC = struct
  structure T = AMD64MLTree
  structure TS = AMD64MLTStream
  structure I = AMD64Instr
  structure CFG = AMD64CFG
  structure C = I.C
   type reducer =
     (I.instruction,C.cellset,I.operand,I.addressing_mode,CFG.cfg) TS.reducer
   fun unimplemented _ = MLRiscErrorMsg.impossible "UserMLTreeExtComp"
   val compileSext  = unimplemented
   val compileRext  = unimplemented
   val compileFext  = unimplemented
   val compileCCext = unimplemented
end (* AMD64MTC *)

structure AMD64MLTreeUtils : MLTREE_UTILS = struct 
  structure T = AMD64MLTree
  structure IX = AMD64InstrExt		 
  structure U = MLTreeUtils (
      structure T = T
      fun hashSext _ _ = 0w0
      fun hashRext _ _ = 0w0
      fun hashFext _ _ = 0w0
      fun hashCCext _ _ = 0w0
      fun eqSext _ _ = raise Fail "eqSext"
      fun eqRext _ _ = raise Fail "eqRext"
      fun eqFext _ _ = raise Fail "eqFext"
      fun eqCCext _ _ = raise Fail "eqCCext"
      fun showSext (prt : T.printer) ext = raise Fail "Todo"
      fun showRext _ _ = raise Fail "showRext"
      fun showFext _ _ = raise Fail "showFext"
      fun showCCext _ _ = raise Fail "showCCext")    
  open U  
end (* AMD64MLTreeUtils *)

structure AMD64MLTreeComp = AMD64 (
   structure AMD64Instr = AMD64Instr
   structure MLTreeUtils = AMD64MLTreeUtils
   structure MLTreeStream = AMD64MLTStream
   structure ExtensionComp = AMD64MTC
   fun cvti2f _ = raise Fail "Todo"
   val fast_floating_point = ref false
   val defaultIntTy = 64
   val defaultAddrTy = 64)


