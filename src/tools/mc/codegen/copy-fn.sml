(* copy-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Functions for copying pseudo registers.
 *)

functor CopyFn (
	structure MTy : MLRISC_TYPES
	structure Spec : TARGET_SPEC
	structure Cells : CELLS
) : COPY = struct

  structure MTy = MTy
  structure T = MTy.T

  val ty = (Word.toInt Spec.wordSzB) * 8
  val maxFty = 64 (* FIXME *)

  fun parallelCopy (vs1, vs2) = T.COPY (ty, vs1, vs2)
  fun copyExpr (ty, v, e) = T.MV (ty, v, e)

  fun parallelFCopy (vs1, vs2) = T.FCOPY (maxFty, vs1, vs2)
  fun copyFExpr (fty, v, e) = T.FMV (ty, v, e)

  fun copy {src, dst} =
      let (* FIXME *)
	  fun mkCopies (MTy.GPReg (_, v1), MTy.GPR (_, v2), 
			(regs, exprs, fregs, fexprs)) =
	      ( (v1,v2) :: regs, exprs, fregs, fexprs)
	    | mkCopies (MTy.GPReg (_, v1), MTy.EXP (ty, e), 
			(regs, exprs, fregs, fexprs)) = 
	      (regs, (ty, v1,e) :: exprs, fregs, fexprs)
	    | mkCopies (MTy.FPReg (_, v1), MTy.FPR (fty, v2), 
			(regs, exprs, fregs, fexprs)) =
	      ( regs, exprs, (v1,v2) :: fregs, fexprs)
	    | mkCopies (MTy.FPReg (_, v1), MTy.FEXP (fty, e), 
			(regs, exprs, fregs, fexprs)) =
	      (regs, exprs, fregs, (fty, v1,e) :: fexprs)
	    | mkCopies (_, _, x) = x
	  val (regs, exprs, fregs, fexprs) = 
	      ListPair.foldl mkCopies ([], [], [], []) (dst, src)
	  val cexps = map copyExpr exprs @ map copyFExpr fexprs
      in
	  parallelCopy (ListPair.unzip regs)  :: 
	  parallelFCopy (ListPair.unzip fregs) :: cexps
      end (* copy *)

  fun fresh regs =
      let fun mkTemp _ = MTy.GPReg (ty, Cells.newReg ())
	  val regs = map MTy.regToTree regs
	  val regs' = map mkTemp regs
      in
	  {stms=copy {src=regs, dst=regs'}, regs=regs'}
      end

end (* CopyFn *)
