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

    structure IX = AMD64Extension
    structure X = AMD64InstrExt
		       
    structure CompInstrExt = AMD64CompInstrExt (
      structure I = I
      structure TS = TS
      structure CFG = CFG)

    type reducer =
	  (I.instruction,C.cellset,I.operand,I.addressing_mode,CFG.cfg) TS.reducer

    type stm = (T.stm, T.rexp, T.fexp, T.ccexp) IX.sx
									
    fun compileSext reducer {stm: stm, an:T.an list} = let
	val TS.REDUCER{operand, emit, reduceFexp, instrStream, reduceOperand, ...} = reducer
	val TS.S.STREAM{emit=emitI, ...} = instrStream
    in
	case stm
	 of IX.EXT stm => CompInstrExt.compileSext reducer {stm=stm, an=an}
	  | IX.LOCK_ANDL (src, dst) =>
	    emit (I.INSTR(I.BINARY{binOp=I.LOCK_ANDL, dst=I.Direct(32,reduceOperand(operand src)),
				   src=operand dst}), an)
	  | IX.LOCK_ORL(src, dst) =>
	    emit (I.INSTR(I.BINARY{binOp=I.LOCK_ORL, dst=I.Direct(32,reduceOperand(operand src)),
				   src=operand dst}), an)
    (* (*Note: once we have 64 bit lock prefixed AND and OR in MLRISC, this should work*)
	  | IX.LOCK_ANDQ (src, dst) =>
	    emit (I.INSTR(I.BINARY{binOp=I.LOCK_ANDQ, dst=I.Direct(64,reduceOperand(operand src)),
				   src=operand dst}), an)
	  | IX.LOCK_ORQ(src, dst) =>
	    emit (I.INSTR(I.BINARY{binOp=I.LOCK_ORQ, dst=I.Direct(64,reduceOperand(operand src)),
				   src=operand dst}), an)  *)
    end
							   
    fun compileRext _ = raise Fail "AMD64CompExtFn.compileRext"
    fun compileFext _ = raise Fail "AMD64CompExtFn.compileFext"
    fun compileCCext _ = raise Fail "AMD64CompExtFn.compileCCext"

  end
