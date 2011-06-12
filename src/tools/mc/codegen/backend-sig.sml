(* backend-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Glue code between the code generator and MLRISC.
 *)

signature BACK_END =
  sig
    structure ManticorePseudoOps : MANTICORE_PSEUDO_OPS
	where type P.T.Region.region = ManticoreRegion.region

    structure MLTreeComp : MLTREECOMP
	where type TS.T.Region.region = ManticorePseudoOps.P.T.Region.region
          where type TS.S.P.T.cond = ManticorePseudoOps.P.T.cond
          where type TS.S.P.T.fcond = ManticorePseudoOps.P.T.fcond
          where type TS.S.P.T.rounding_mode = ManticorePseudoOps.P.T.rounding_mode
          where type TS.S.P.T.div_rounding_mode = ManticorePseudoOps.P.T.div_rounding_mode
          where type TS.S.P.T.ext = ManticorePseudoOps.P.T.ext
          where type TS.S.P.T.stm = ManticorePseudoOps.P.T.stm
          where type TS.S.P.T.rexp = ManticorePseudoOps.P.T.rexp
          where type TS.S.P.T.rep = ManticorePseudoOps.P.T.rep
          where type TS.S.P.T.oper = ManticorePseudoOps.P.T.oper
          where type TS.S.P.T.fexp = ManticorePseudoOps.P.T.fexp
          where type TS.S.P.T.ccexp = ManticorePseudoOps.P.T.ccexp
          where type TS.S.P.T.mlrisc = ManticorePseudoOps.P.T.mlrisc
          where type TS.S.P.Client.pseudo_op = ManticorePseudoOps.pseudo_op_ext
          where type TS.T.cond = ManticorePseudoOps.P.T.cond
          where type TS.T.fcond = ManticorePseudoOps.P.T.fcond
          where type TS.T.rounding_mode = ManticorePseudoOps.P.T.rounding_mode
          where type TS.T.div_rounding_mode = ManticorePseudoOps.P.T.div_rounding_mode
          where type TS.T.ext = ManticorePseudoOps.P.T.ext
          where type TS.T.stm = ManticorePseudoOps.P.T.stm
          where type TS.T.rexp = ManticorePseudoOps.P.T.rexp
          where type TS.T.rep = ManticorePseudoOps.P.T.rep
          where type TS.T.oper = ManticorePseudoOps.P.T.oper
          where type TS.T.fexp = ManticorePseudoOps.P.T.fexp
          where type TS.T.ccexp = ManticorePseudoOps.P.T.ccexp
          where type TS.T.mlrisc = ManticorePseudoOps.P.T.mlrisc
    structure MLTreeUtils : MLTREE_UTILS
          where type T.cond = MLTreeComp.TS.T.cond
          where type T.fcond = MLTreeComp.TS.T.fcond
          where type T.rounding_mode = MLTreeComp.TS.T.rounding_mode
          where type T.div_rounding_mode = MLTreeComp.TS.T.div_rounding_mode
          where type T.ext = MLTreeComp.TS.T.ext
          where type T.stm = MLTreeComp.TS.T.stm
          where type T.rexp = MLTreeComp.TS.T.rexp
          where type T.rep = MLTreeComp.TS.T.rep
          where type T.oper = MLTreeComp.TS.T.oper
          where type T.fexp = MLTreeComp.TS.T.fexp
          where type T.ccexp = MLTreeComp.TS.T.ccexp
          where type T.mlrisc = MLTreeComp.TS.T.mlrisc

    structure CFGGen : CONTROL_FLOWGRAPH_GEN
          where type I.instruction = MLTreeComp.I.instruction
          where type ('a,'b,'c,'d) S.stream = ('a,'b,'c,'d) MLTreeComp.TS.S.stream
          where type S.P.T.cond = MLTreeComp.TS.T.cond
          where type S.P.T.fcond = MLTreeComp.TS.T.fcond
          where type S.P.T.rounding_mode = MLTreeComp.TS.T.rounding_mode
          where type S.P.T.div_rounding_mode = MLTreeComp.TS.T.div_rounding_mode
          where type S.P.T.ext = MLTreeComp.TS.T.ext
          where type S.P.T.stm = MLTreeComp.TS.T.stm
          where type S.P.T.rexp = MLTreeComp.TS.T.rexp
          where type S.P.T.rep = MLTreeComp.TS.T.rep
          where type S.P.T.oper = MLTreeComp.TS.T.oper
          where type S.P.T.fexp = MLTreeComp.TS.T.fexp
          where type S.P.T.ccexp = MLTreeComp.TS.T.ccexp
          where type S.P.T.mlrisc = MLTreeComp.TS.T.mlrisc
          where type CFG.block_kind = MLTreeComp.CFG.block_kind
          where type CFG.block = MLTreeComp.CFG.block
          where type CFG.edge_kind = MLTreeComp.CFG.edge_kind
          where type CFG.edge_info = MLTreeComp.CFG.edge_info
          where type CFG.info = MLTreeComp.CFG.info

(*	where CFG = MLTreeComp.CFG
	where I = MLTreeComp.I
	where S = MLTreeComp.TS.S *)

    structure SpillLoc : SPILL_LOC
    structure Spec : TARGET_SPEC
    structure Regs : MANTICORE_REGS
    structure MTy : MLRISC_TYPES
          where type T.cond = MLTreeComp.TS.T.cond
          where type T.fcond = MLTreeComp.TS.T.fcond
          where type T.rounding_mode = MLTreeComp.TS.T.rounding_mode
          where type T.div_rounding_mode = MLTreeComp.TS.T.div_rounding_mode
          where type T.ext = MLTreeComp.TS.T.ext
          where type T.stm = MLTreeComp.TS.T.stm
          where type T.rexp = MLTreeComp.TS.T.rexp
          where type T.rep = MLTreeComp.TS.T.rep
          where type T.oper = MLTreeComp.TS.T.oper
          where type T.fexp = MLTreeComp.TS.T.fexp
          where type T.ccexp = MLTreeComp.TS.T.ccexp
          where type T.mlrisc = MLTreeComp.TS.T.mlrisc
    structure LabelCode : LABEL_CODE
          where type MTy.mlrisc_kind = MTy.mlrisc_kind
          where type MTy.mlrisc_tree = MTy.mlrisc_tree
          where type MTy.mlrisc_reg = MTy.mlrisc_reg
          where type MTy.T.cond = MTy.T.cond
          where type MTy.T.fcond = MTy.T.fcond
          where type MTy.T.rounding_mode = MTy.T.rounding_mode
          where type MTy.T.div_rounding_mode = MTy.T.div_rounding_mode
          where type MTy.T.ext = MTy.T.ext
          where type MTy.T.stm = MTy.T.stm
          where type MTy.T.rexp = MTy.T.rexp
          where type MTy.T.rep = MTy.T.rep
          where type MTy.T.oper = MTy.T.oper
          where type MTy.T.fexp = MTy.T.fexp
          where type MTy.T.ccexp = MTy.T.ccexp
          where type MTy.T.mlrisc = MTy.T.mlrisc
    structure Alloc : ALLOC
          where type MTy.mlrisc_kind = MTy.mlrisc_kind
          where type MTy.mlrisc_tree = MTy.mlrisc_tree
          where type MTy.mlrisc_reg = MTy.mlrisc_reg
          where type MTy.T.cond = MTy.T.cond
          where type MTy.T.fcond = MTy.T.fcond
          where type MTy.T.rounding_mode = MTy.T.rounding_mode
          where type MTy.T.div_rounding_mode = MTy.T.div_rounding_mode
          where type MTy.T.ext = MTy.T.ext
          where type MTy.T.stm = MTy.T.stm
          where type MTy.T.rexp = MTy.T.rexp
          where type MTy.T.rep = MTy.T.rep
          where type MTy.T.oper = MTy.T.oper
          where type MTy.T.fexp = MTy.T.fexp
          where type MTy.T.ccexp = MTy.T.ccexp
          where type MTy.T.mlrisc = MTy.T.mlrisc
    structure AtomicOps : ATOMIC_OPS
          where type MTy.mlrisc_kind = MTy.mlrisc_kind
          where type MTy.mlrisc_tree = MTy.mlrisc_tree
          where type MTy.mlrisc_reg = MTy.mlrisc_reg
          where type MTy.T.cond = MTy.T.cond
          where type MTy.T.fcond = MTy.T.fcond
          where type MTy.T.rounding_mode = MTy.T.rounding_mode
          where type MTy.T.div_rounding_mode = MTy.T.div_rounding_mode
          where type MTy.T.ext = MTy.T.ext
          where type MTy.T.stm = MTy.T.stm
          where type MTy.T.rexp = MTy.T.rexp
          where type MTy.T.rep = MTy.T.rep
          where type MTy.T.oper = MTy.T.oper
          where type MTy.T.fexp = MTy.T.fexp
          where type MTy.T.ccexp = MTy.T.ccexp
          where type MTy.T.mlrisc = MTy.T.mlrisc
    structure Types : ARCH_TYPES
    structure Copy : COPY
          where type MTy.mlrisc_kind = MTy.mlrisc_kind
          where type MTy.mlrisc_tree = MTy.mlrisc_tree
          where type MTy.mlrisc_reg = MTy.mlrisc_reg
          where type MTy.T.cond = MTy.T.cond
          where type MTy.T.fcond = MTy.T.fcond
          where type MTy.T.rounding_mode = MTy.T.rounding_mode
          where type MTy.T.div_rounding_mode = MTy.T.div_rounding_mode
          where type MTy.T.ext = MTy.T.ext
          where type MTy.T.stm = MTy.T.stm
          where type MTy.T.rexp = MTy.T.rexp
          where type MTy.T.rep = MTy.T.rep
          where type MTy.T.oper = MTy.T.oper
          where type MTy.T.fexp = MTy.T.fexp
          where type MTy.T.ccexp = MTy.T.ccexp
          where type MTy.T.mlrisc = MTy.T.mlrisc
    structure VarDef : VAR_DEF		     
          where type MTy.mlrisc_kind = MTy.mlrisc_kind
          where type MTy.mlrisc_tree = MTy.mlrisc_tree
          where type MTy.mlrisc_reg = MTy.mlrisc_reg
          where type MTy.T.cond = MTy.T.cond
          where type MTy.T.fcond = MTy.T.fcond
          where type MTy.T.rounding_mode = MTy.T.rounding_mode
          where type MTy.T.div_rounding_mode = MTy.T.div_rounding_mode
          where type MTy.T.ext = MTy.T.ext
          where type MTy.T.stm = MTy.T.stm
          where type MTy.T.rexp = MTy.T.rexp
          where type MTy.T.rep = MTy.T.rep
          where type MTy.T.oper = MTy.T.oper
          where type MTy.T.fexp = MTy.T.fexp
          where type MTy.T.ccexp = MTy.T.ccexp
          where type MTy.T.mlrisc = MTy.T.mlrisc
    structure Transfer : TRANSFER
          where type MTy.mlrisc_kind = MTy.mlrisc_kind
          where type MTy.mlrisc_tree = MTy.mlrisc_tree
          where type MTy.mlrisc_reg = MTy.mlrisc_reg
          where type MTy.T.cond = MTy.T.cond
          where type MTy.T.fcond = MTy.T.fcond
          where type MTy.T.rounding_mode = MTy.T.rounding_mode
          where type MTy.T.div_rounding_mode = MTy.T.div_rounding_mode
          where type MTy.T.ext = MTy.T.ext
          where type MTy.T.stm = MTy.T.stm
          where type MTy.T.rexp = MTy.T.rexp
          where type MTy.T.rep = MTy.T.rep
          where type MTy.T.oper = MTy.T.oper
          where type MTy.T.fexp = MTy.T.fexp
          where type MTy.T.ccexp = MTy.T.ccexp
          where type MTy.T.mlrisc = MTy.T.mlrisc
          where type VarDef.var_def_tbl = VarDef.var_def_tbl
          where type SpillLoc.frame = SpillLoc.frame
    structure VProcOps : VPROC_OPS
          where type MTy.mlrisc_kind = MTy.mlrisc_kind
          where type MTy.mlrisc_tree = MTy.mlrisc_tree
          where type MTy.mlrisc_reg = MTy.mlrisc_reg
          where type MTy.T.cond = MTy.T.cond
          where type MTy.T.fcond = MTy.T.fcond
          where type MTy.T.rounding_mode = MTy.T.rounding_mode
          where type MTy.T.div_rounding_mode = MTy.T.div_rounding_mode
          where type MTy.T.ext = MTy.T.ext
          where type MTy.T.stm = MTy.T.stm
          where type MTy.T.rexp = MTy.T.rexp
          where type MTy.T.rep = MTy.T.rep
          where type MTy.T.oper = MTy.T.oper
          where type MTy.T.fexp = MTy.T.fexp
          where type MTy.T.ccexp = MTy.T.ccexp
          where type MTy.T.mlrisc = MTy.T.mlrisc
          where type VarDef.var_def_tbl = VarDef.var_def_tbl

  (* literals that MLRISC introduces during instruction selection *)
    val literals : (Label.label * ManticorePseudoOps.PseudoOps.pseudo_op) list ref

    (* take a control-flow graph, do RA, optimization, etc. and then
     * emit it to the assembly file.
     *)
    val compileCFG : CFGGen.CFG.cfg -> unit
						     
  end (* BACK_END *)
