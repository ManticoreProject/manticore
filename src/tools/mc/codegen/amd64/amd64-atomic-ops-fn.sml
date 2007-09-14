(* amd64-atomic-ops-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor AMD64AtomicOpsFn (

    structure MTy : MLRISC_TYPES
      where T.Extension = AMD64Extension
    structure I : AMD64INSTR
      where T = MTy.T

  ) : ATOMIC_OPS = struct

    structure MTy = MTy
    structure T = MTy.T
    structure IX = AMD64InstrExt
    structure Cells = AMD64Cells

    fun copyDef (ty, dst, T.REG(_, src)) = T.COPY(ty, [dst], [src])
      | copyDef (ty, dst, rexp) = T.MV(ty, dst, rexp)

  (* 32-bit compare and swap operation *)
    fun genCompareAndSwap32 {addr, cmpVal, newVal} = let
	  val oldVal = Cells.newReg()
	  val oldVal' = T.REG(32, oldVal)
	  val stms = [
		  copyDef (32, Cells.rax, cmpVal),
		  T.EXT(IX.LOCK_CMPXCHGL(newVal, addr)),
		  T.COPY(32, [oldVal], [Cells.rax])
		]
	  in
	    (T.CC(T.Basis.EQ, Cells.eflags), oldVal', stms)
	  end

  (* 64-bit compare and swap operation *)
    fun genCompareAndSwap64 {addr, cmpVal, newVal} = let
	  val oldVal = Cells.newReg()
	  val oldVal' = T.REG(64, oldVal)
	  val stms = [
		  copyDef (64, Cells.rax, cmpVal),
		  T.EXT(IX.LOCK_CMPXCHGQ(newVal, addr)),
		  T.COPY(64, [oldVal], [Cells.rax])
		]
	  in
	    (T.CC(T.Basis.EQ, Cells.eflags), oldVal', stms)
	  end

  (* word-sized compare and swap operation *)
    val genCompareAndSwapWord = genCompareAndSwap64

  end
