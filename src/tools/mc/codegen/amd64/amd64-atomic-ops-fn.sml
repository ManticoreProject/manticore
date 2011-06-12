(* amd64-atomic-ops-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor AMD64AtomicOpsFn (

    structure MTy : MLRISC_TYPES
      where type ('s,'r,'f,'c) T.Extension.sx = ('s,'r,'f,'c) AMD64Extension.sx
      where type ('s,'r,'f,'c) T.Extension.rx = ('s,'r,'f,'c) AMD64Extension.rx
      where type ('s,'r,'f,'c) T.Extension.fx = ('s,'r,'f,'c) AMD64Extension.fx
      where type ('s,'r,'f,'c) T.Extension.ccx = ('s,'r,'f,'c) AMD64Extension.ccx
    structure I : AMD64INSTR
          where type T.cond = MTy.T.cond
          where type T.fcond = MTy.T.fcond
          where type T.rounding_mode = MTy.T.rounding_mode
          where type T.div_rounding_mode = MTy.T.div_rounding_mode
          where type T.ext = MTy.T.ext
          where type T.stm = MTy.T.stm
          where type T.rexp = MTy.T.rexp
          where type T.rep = MTy.T.rep
          where type T.oper = MTy.T.oper
          where type T.fexp = MTy.T.fexp
          where type T.ccexp = MTy.T.ccexp
          where type T.mlrisc = MTy.T.mlrisc
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

  end
