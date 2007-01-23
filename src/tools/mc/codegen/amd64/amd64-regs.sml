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

  val stdArgReg = C.rax
  val clReg = C.rcx
  val kReg = C.rdx
  val exnReg = C.rbx
  val spReg = C.rsp
  val fpReg = NONE  (* shouldn't we use the frame pointer for something else? *)
  val apReg = C.rdi
  val dedicatedRegs = [C.rdi, C.rsp]
  val dedicatedFRegs = []

  val miscRegs = C.Regs CellsBasis.GP {from=0, to=3, step=1} @ 
		 C.Regs CellsBasis.GP {from=5, to=6, step=1} @
		 C.Regs CellsBasis.GP {from=8, to=15, step=1}

  (* this list of callee-save gprs complies with the SVID C ABI. *)
  val saveRegs = [C.rbx, C.rsp, C.rbp] @
		 C.Regs CellsBasis.GP {from=12, to=15, step=1}
  val availRegs = miscRegs
  (* This list of argument gprs complies with the SVID C ABI.  The list is
   * in the order of the actual arguments. *)
  val argRegs = [C.rdi, C.rsi, C.rdx, C.rcx] @ 
		C.Regs CellsBasis.GP {from=8, to=9, step=1}
  val allRegs = miscRegs @ dedicatedRegs

  val miscFRegs = []
  val argFRegs = []
  val saveFRegs = []
  val availFRegs = []
  val allFRegs = []
 
end (* AMD64Regs *)
