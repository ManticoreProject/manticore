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
    val retReg = C.GPReg 8
    val exhReg = C.GPReg 9
    val spReg = C.rsp
    val fpReg = SOME C.rbp
    val apReg = C.rsi
    val limReg = C.GPReg 11
    val dedicatedRegs = [argReg, closReg, retReg, exhReg, spReg, apReg, valOf fpReg, limReg]
    val dedicatedFRegs = []
  
    val allRegs = C.Regs CellsBasis.GP {from=0, to=15, step=1}
    val allRegsSet = foldl C.addReg C.empty allRegs
  
    val miscRegs =
	let val rSet = foldl C.rmvReg allRegsSet dedicatedRegs
	in 
	    C.getReg rSet 
	end

  (* Callee/caller-save registers as specified by the SVID *)
    val svidCalleeSaves = 
	C.rbx :: C.Regs CellsBasis.GP {from=12, to=15, step=1} 
    val svidCallerSaves =
	[C.rax, C.rcx, C.rdx, C.rsi, C.rdi] @ 
	C.Regs CellsBasis.GP {from=8, to=11, step=1}
  
    val saveRegs = svidCallerSaves
    val availRegs = miscRegs
    val argRegs = [argReg, closReg, retReg, exhReg]

    val allFRegs = C.Regs CellsBasis.FP {from=0, to=15, step=1}
    val svidCallerSaveSSE = allFRegs  
    val miscFRegs = allFRegs
    val argFRegs = C.Regs CellsBasis.FP {from=2, to=7, step=1}
    val saveFRegs = svidCallerSaveSSE
    val availFRegs = allFRegs
   
  end (* AMD64Regs *)
