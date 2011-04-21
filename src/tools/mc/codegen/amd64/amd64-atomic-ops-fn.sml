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

  (* atomic swap operation *)
    fun genSwap {ty, addr, newVal} = let
	  val oldVal = Cells.newReg()
	  val oldVal' = T.REG(ty, oldVal)
	  val xchg = (case ty
		 of 32 => IX.LOCK_XCHGL(T.REG(32, newVal), addr)
		  | 64 => IX.LOCK_XCHGQ(T.REG(64, newVal), addr)
		(* end case *))
	  val stms = [T.EXT xchg, T.COPY(ty, [oldVal], [newVal]) ]
	  in
	    (oldVal', stms)
	  end

  (* atomic compare and swap operation *)
    fun genCompareAndSwap {ty, addr, cmpVal, newVal} = let
	  val oldVal = Cells.newReg()
	  val oldVal' = T.REG(ty, oldVal)
	  val cmpxchg = (case ty
		 of 32 => IX.LOCK_CMPXCHGL(newVal, addr)
		  | 64 => IX.LOCK_CMPXCHGQ(newVal, addr)
		(* end case *))
	  val stms = [
		  copyDef (ty, Cells.rax, cmpVal),
		  T.EXT cmpxchg,
		  T.COPY(ty, [oldVal], [Cells.rax])
		]
	  in
	    (T.CC(T.Basis.EQ, Cells.eflags), oldVal', stms)
	  end

  (* atomic test and set operation *)
(* FIXME: once we have support for the BTS instruction, we should use it *)
    fun genTestAndSet {ty, addr} = let
	  val r' = Cells.newReg()
	  val r = T.REG(ty, r')
	  val xchg = (case ty
		 of 32 => IX.LOCK_XCHGL(r, addr)
		  | 64 => IX.LOCK_XCHGQ(r, addr)
		(* end case *))
	  val stms = [T.MV (ty, r', T.LI 1), T.EXT xchg]
	  val cc = T.CMP(ty, T.EQ, r, T.LI 1)
	  in
	    (cc, stms)
	  end

   (* atomic fetch and add operation *)
    fun genFetchAndAdd {ty, addr, x} = let
	  val r = Cells.newReg ()
	  val r' = T.REG(ty, r)
	  val xadd = (case ty
		 of 32 => IX.LOCK_XADDL(r', addr)
		  | 64 => IX.LOCK_XADDQ(r', addr)
		(* end case *))
         val stms = [copyDef (ty, r, x), T.EXT xadd]
         in
             (r', stms)
         end

  (* pause instruction to support efficient spin locks *)
    fun genPause () = [T.EXT IX.PAUSE]

  (* sequentializing operation for all write-to-memory instructions
   * prior to this instruction
   *)
    fun genFenceWrite () = [T.EXT IX.SFENCE]

  (* sequentializing operation for all load-from-memory instructions
   * prior to this instruction
   *)
    fun genFenceRead () = [T.EXT IX.LFENCE]

  (* sequentializing operation for all load-from-memory and write-to-memory
   * instructions prior to this instruction
   *)
    fun genFenceRW () = [T.EXT IX.MFENCE]

    fun genTimeStampCounter () = let
      val r = Cells.newReg ()
      val r' = T.REG(64, r)
      val c32 = T.LI (T.I.fromInt (64, 32))
      val stms = [
          (* ensures that all operations before the rtdsc finish executing *)
	    T.EXT IX.LFENCE,
	    T.EXT IX.RDTSC,
	    T.MV (64, r, T.ORB (64, T.REG(64, Cells.rax), 
				    T.SLL(64, T.REG(64, Cells.rdx), c32)))
          ]
      in
	(r', stms)
      end

  end
