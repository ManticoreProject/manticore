(* amd64-copy-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Functions for copying pseudo registers.
 *)

functor AMD64CopyFn (
	structure MTy : MLRISC_TYPES
	structure Spec : TARGET_SPEC
(*	structure Cells : CELLS*)
	structure MLTreeUtils : MLTREE_UTILS
				where T = MTy.T
) : COPY = struct

    structure MTy = MTy
    structure T = MTy.T
    structure Cells = AMD64Cells

    val ty = MTy.wordTy

    fun note' stm =
	  T.ANNOTATION(stm, #create MLRiscAnnotations.COMMENT 
	    (MLTreeUtils.stmToString stm))

    fun move (ty, v, e) = T.MV (ty, v, e)
    fun fmove (fty, v, e) = note' (T.FMV (fty, v, e))

    fun copy {src, dst} = let
	(* For some reason, MLRISC generates a movzbq and movzwq instructions for 
	 * 8 and 16 bit loads respectively.  When we want to do a load of an 8 bit
	 * value into an 8 bit register, this generates an invalid instruction.
	 * to get around this, I'm doing a 64-bit load, and then a 16-16 bit 
	 * register move (8-8 bit respectively).  These must be done prior to 
	 * the other moves and are thus not "parallel" so we stuff them into
	 * this prelims ref cell and tack it onto the beginning - ml9951
	 *)
	val prelims = ref []
	(* parallel copy for gprs *)
	fun rcopy ( (8, dst, src),
		    (T.COPY(8, dsts, srcs), c16s, c32s, c64s)) =
	    (T.COPY(8, dst::dsts, src::srcs), c16s, c32s, c64s)
	  | rcopy ( (16, dst, src),
		    (c8s, T.COPY(16, dsts, srcs), c32s, c64s)) =
	    (c8s, T.COPY(16, dst::dsts, src::srcs), c32s, c64s)
	  | rcopy ( (32, dst, src),
		    (c8s, c16s, T.COPY(32,dsts,srcs), c64s)) =
	    (c8s, c16s, T.COPY(32, dst::dsts, src::srcs), c64s)
	  | rcopy ( (64, dst, src),
		    (c8s, c16s, c32s, T.COPY(64, dsts, srcs))) =
	    (c8s, c16s, c32s, T.COPY(64, dst::dsts, src::srcs))
	  | rcopy ( (s, dst, src), (c8s, c16s, c32s, c64s)) =
	    raise Fail ("rcopy does not currently support size " ^ Int.toString s ^ "\n")
	  (* parallel copy for fprs *)
	  fun fcopy ( (32, dst, src), 
		      (T.FCOPY (32, dsts, srcs), fc64s, fc80s) ) =
	      (T.FCOPY (32, dst :: dsts, src :: srcs), fc64s, fc80s)
	    | fcopy ( (64, dst, src),
		      (fc32s, T.FCOPY (64, dsts, srcs), fc80s) ) =
	      (fc32s, T.FCOPY (64, dst :: dsts, src :: srcs), fc80s)
	    | fcopy ( (80, dst, src),
		      (fc32s, fc64s, T.FCOPY (80, dsts, srcs)) ) =
	      (fc32s, fc64s, T.FCOPY (80, dst :: dsts, src :: srcs))
	    | fcopy ( (s, dst, src), (fc32s, fc64s, fc80s)) =
	    raise Fail ("rcopy does not currently support size " ^ Int.toString s ^ "\n")
	(* copy mltrees to registers *)
	  fun mkCopies (MTy.GPReg (_, v1), MTy.GPR (ty, v2), 
			(regs, exprs, fregs, fexprs)) =
	      ( (ty, v1,v2) :: regs, exprs, fregs, fexprs)
	    | mkCopies (MTy.GPReg (_, v1), MTy.EXP (ety, e), 
			(regs, exprs, fregs, fexprs)) = 
	      if ety < 32
	      then
		  let val temp = Cells.newReg()
		      val _ = prelims := move(64, temp, e) :: !prelims
		  in ((ety,v1,temp)::regs,exprs, fregs, fexprs) end
	      else 
	      (regs, (ety,v1,e) :: exprs, fregs, fexprs) 
	    | mkCopies (MTy.FPReg (_, v1), MTy.FPR (fty, v2), 
			(regs, exprs, fregs, fexprs)) = 
	      ( regs, exprs, (fty,v1,v2) :: fregs, fexprs)
	    | mkCopies (MTy.FPReg (_, v1), MTy.FEXP (fty, e), 
			(regs, exprs, fregs, fexprs)) = 
	      (regs, exprs, fregs, (fty,v1,e) :: fexprs) 
	    | mkCopies (MTy.GPReg (ty, v1), MTy.CEXP e,
			(regs, exprs, fregs, fexprs)) =
	      ( regs, (ty,v1,MTy.cexpToExp e) :: exprs, fregs, fexprs )
	    | mkCopies (dst, src, _) = 
              raise Fail (concat ["mkCopies {dst=", MTy.treeToString (MTy.regToTree dst), 
                                  ",src=", MTy.treeToString src, "}"])

	  val (regs, exprs, fregs, fexprs) = 
	      ListPair.foldl mkCopies ([], [], [], []) (dst, src)

	  val (pc8, pc16, pc32, pc64) = 
	      foldl rcopy (T.COPY (8, [], []), T.COPY (16, [], []), T.COPY (32, [], []), T.COPY (64, [], [])) regs
	  val (fpc32, fpc64, fpc80) =
	      foldl fcopy (T.FCOPY (32, [], []), T.FCOPY (64, [], []), 
			   T.FCOPY (80, [], [])) fregs

	  val cexps = map move exprs @ map fmove fexprs

	  fun isNonEmptyCpy (T.COPY (_, [], [])) = false
	    | isNonEmptyCpy (T.FCOPY (_, [], [])) = false
	    | isNonEmptyCpy _ = true

	  in
	    (!prelims @ List.filter isNonEmptyCpy [pc8, pc16, pc32, pc64, fpc32, fpc64, fpc80]) @ cexps
	  end (* copy *)

    fun fresh regs = let
	  fun mkTemp (MTy.GPReg (ty, _)) = MTy.GPReg (ty, Cells.newReg ())
	    | mkTemp (MTy.FPReg (fty, _)) = MTy.FPReg (fty, Cells.newFreg ())
	  val regs' = map mkTemp regs
	  val regs = map MTy.regToTree regs
	  in
	    {stms=copy {src=regs, dst=regs'}, regs=regs'}
	  end

  end (* AMD64CopyFn *)
