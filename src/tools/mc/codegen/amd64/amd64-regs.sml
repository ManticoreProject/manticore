(* amd64-regs.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Determines static register allocation.
 *)

structure AMD64Regs : MANTICORE_REGS = struct
 
  type gpr = CellsBasis.cell
  type fpr = CellsBasis.cell

  structure C = AMD64Cells

  val argReg = C.rax
  val closReg = C.rdi
  val retReg = C.rsi
  val exhReg = C.rbx
  val spReg = C.rsp
  val fpReg = SOME C.rbp
  val apReg = C.rcx
  val dedicatedRegs = [argReg, closReg, retReg, exhReg, spReg, apReg, valOf fpReg]
  val dedicatedFRegs = []

  val allRegs = C.Regs CellsBasis.GP {from=0, to=15, step=1}
  val allRegsSet = foldl C.addReg C.empty allRegs

  val miscRegs =
      let val rSet = foldl C.rmvReg allRegsSet dedicatedRegs
      in 
	  C.getReg rSet 
      end

  (* this list of callee-save gprs complies with the SVID C ABI. *)
  val saveRegs = [C.rbx, C.rsp, C.rbp] @
		 C.Regs CellsBasis.GP {from=12, to=15, step=1}
  val availRegs = miscRegs
  (* This list of argument gprs complies with the SVID C ABI.  The list is
   * in the order of the actual arguments. *)
  val argRegs = [C.rdi, C.rsi, C.rdx, C.rcx] @ 
		C.Regs CellsBasis.GP {from=8, to=9, step=1}

  val miscFRegs = []
  val argFRegs = []
  val saveFRegs = []
  val availFRegs = []
  val allFRegs = []
 
end (* AMD64Regs *)
