(* vproc-ops-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate code for the following vproc operations:
 * - let vproc = host_vp ()                 (inline assembly)
 * - let () = enqueue (vproc, tid, k)     (calls into runtime-system)
 *   Runtime convention: 
 *        enqueue (argRegs[0], argRegs[1], argRegs[2])
 * - let (tid, k) = dequeue (vproc)         (calls into runtime-system)
 *   Runtime convention: 
 *        (argRegs[0], argRegs[1]) := dequeue (argRegs[0])
 * - let vprocs = provision n          (??)
 * - let () = release vproc            (??)
 * - let tid = newTid ()               (??)
 * - let () = setTid tid               (??)
 * - let tid = getTid ()               (??)
 *)

signature VPROC_OPS = sig

    structure MTy : MLRISC_TYPES

    val genHostVP : MTy.mlrisc_tree

end (* VPROC_OPS *)

functor VProcOpsFn (
    structure MTy : MLRISC_TYPES
    structure Regs : MANTICORE_REGS
    structure Spec : TARGET_SPEC
    structure Types : ARCH_TYPES
    structure MLTreeComp : MLTREECOMP 
) : VPROC_OPS = struct

  structure MTy = MTy
  structure W = Word64
  structure Cells = MLTreeComp.I.C
  structure T = MTy.T

  val ty = MTy.wordTy

  (* Assume that the runtime system aligns the heap on a
   * vpHeapSzB boundary. *)
  val vpHeapSzB = Word.toLargeWord Spec.vpHeapSzB
  val vpHeapMask = W.notb (W.- (W.fromLargeWord vpHeapSzB, 0w1))

  val genHostVP =
      MTy.EXP (ty, T.ANDB (ty, T.REG (ty, Regs.apReg), 
				      T.LI (W.toLargeInt vpHeapMask)))

end (* VProcOpsFn *)
