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

  (* 32-bit test and set operation *)
    fun genTestAndSet32 {addr, newVal} = let
	  val oldVal = Cells.newReg()
	  val oldVal' = T.REG(32, oldVal)
	  val stms = [
		  T.EXT(IX.LOCK_XCHGL(T.REG(32, newVal), addr)),
		  T.COPY(32, [oldVal], [newVal])
		]
	  in
	    (oldVal', stms)
	  end

  (* 64-bit test and set operation *)
    fun genTestAndSet64 {addr, newVal} = let
	  val oldVal = Cells.newReg()
	  val oldVal' = T.REG(64, oldVal)
	  val stms = [
		  T.EXT(IX.LOCK_XCHGQ(T.REG(64, newVal), addr)),
		  T.COPY(64, [oldVal], [newVal])
		]
	  in
	    (oldVal', stms)
	  end

  (* word-sized test and set operation *)
    val genTestAndSetWord = genTestAndSet64

   (* 32-bit fetch and add operation *)
    fun genFetchAndAdd32 {addr, x} = let
         val r = Cells.newReg ()
         val r' = T.REG(32, r)
         val stms = [
	         copyDef (32, r, x),
	         T.EXT(IX.LOCK_XADDL(r', addr))
               ]
         in
             (r', stms)
         end

   (* 64-bit fetch and add operation *)
    fun genFetchAndAdd64 {addr, x} = let
         val r = Cells.newReg ()
         val r' = T.REG(64, r)
         val stms = [
	         copyDef (64, r, x),
	         T.EXT(IX.LOCK_XADDQ(r', addr))
               ]
         in
             (r', stms)
         end

  (* pause instruction to support efficient spin locks *)
    fun genPause () = [T.EXT IX.PAUSE]

  (* sequentializing operation for all write-to-memory instructions prior to this instruction *)
    fun genFenceWrite () = [T.EXT IX.SFENCE]

  (* sequentializing operation for all load-from-memory instructions prior to this instruction *)
    fun genFenceRead () = [T.EXT IX.LFENCE]

  (* sequentializing operation for all load-from-memory and write-to-memory instructions prior to this instruction *)
    fun genFenceRW () = [T.EXT IX.MFENCE]

  end
