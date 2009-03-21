(* manticore-regs-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Register conventions specialized to an architecture.
 *)

signature MANTICORE_REGS = sig

    type gpr = CellsBasis.cell
    type fpr = CellsBasis.cell

    val gprWidths : int list            (* legal widths for general-purpose registers *)
    val fprWidths : int list            (* legal widths for floating-point registers *)

    val argReg : gpr       		(* function argument register *)	   
    val closReg : gpr		        (* closure-pointer register *)
    val retReg : gpr		        (* return-continuation register *)
    val exhReg : gpr		        (* exception register *)
    val spReg : gpr   		        (* stack-pointer register *)
    val fpReg : gpr option	        (* frame-pointer register *)
    val apReg : gpr     		(* allocation-pointer register *)
    val dedicatedRegs : gpr list	(* dedicated general-purpose registers *)
					(* (this includes spReg and apReg) *)
    val dedicatedFRegs : fpr list	(* dedicated floating-point registers *) 

    (* Invariants:
     *   allRegs = dedicatedRegs + miscRegs
     *   availRegs <= miscRegs + saveRegs
     *)
    val miscRegs : gpr list     (* non-dedicated general-purpose registers *)
    val saveRegs : gpr list     (* caller-save registers *)
    val availRegs : gpr list    (* registers available for allocation *)
    val argRegs : gpr list      (* registers for passing function arguments *)
    val allRegs : gpr list      (* all general-purpose registers *)

    val miscFRegs : fpr list	(* non-dedicated floating-point registers *)
    val argFRegs : fpr list	(* floating-point registers used to pass *)
				(* arguments and results *)
    val saveFRegs : fpr list	(* caller-save floating-point registers *)
    val availFRegs : gpr list	(* registers available for allocation *)
    val allFRegs : fpr list	(* all of the floating-point registers *)

  end
