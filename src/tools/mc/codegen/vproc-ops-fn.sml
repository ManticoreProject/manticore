(* vproc-ops-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate code for accessing and modifying fields in the vproc structure.
 *)

signature VPROC_OPS = sig

    structure MTy : MLRISC_TYPES
    structure VarDef : VAR_DEF

    (* this expression is a pointer to the host vproc structure. *)
    val genHostVP : MTy.mlrisc_tree
    (* load a value from a given offset off the vproc structure. *)
    val genVPLoad : VarDef.var_def_tbl -> (CFG.offset * CFG.var) -> MTy.mlrisc_tree
    (* store a value at an offset from the vproc structure. *)
    val genVPStore : VarDef.var_def_tbl -> (CFG.offset * CFG.var * CFG.var) -> MTy.T.stm

end (* VPROC_OPS *)

functor VProcOpsFn (
    structure MTy : MLRISC_TYPES
    structure VarDef : VAR_DEF where MTy = MTy
    structure Regs : MANTICORE_REGS
    structure Spec : TARGET_SPEC
    structure Types : ARCH_TYPES
    structure MLTreeComp : MLTREECOMP 
) : VPROC_OPS = struct

  structure MTy = MTy
  structure VarDef = VarDef
  structure W = Word64
  structure Cells = MLTreeComp.I.C
  structure T = MTy.T

  val ty = MTy.wordTy
  val memory = ManticoreRegion.memory

  val genHostVP = MTy.EXP(ty, T.ANDB (ty, T.REG (ty, Regs.apReg), T.LI Spec.ABI.vpMask))

  fun genVPLoad varDefTbl (offset, vproc) =
      MTy.EXP (ty, T.LOAD (ty, T.ADD (ty, VarDef.defOf varDefTbl vproc, T.LI offset), memory))

  fun genVPStore varDefTbl (offset, vproc, v) =
      T.STORE (ty, T.ADD (ty, VarDef.defOf varDefTbl vproc, T.LI offset),
	       VarDef.defOf varDefTbl v, memory)

end (* VProcOpsFn *)
