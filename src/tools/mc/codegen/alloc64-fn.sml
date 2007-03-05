(* alloc-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate code for allocating blocks of memory in the heap.
 *)

functor Alloc64Fn (
	structure MTy : MLRISC_TYPES
	structure Regs : MANTICORE_REGS
	structure Spec : TARGET_SPEC
	structure Types : ARCH_TYPES
	structure MLTreeComp : MLTREECOMP 
) : ALLOC = struct

  structure MTy = MTy
  structure T = MTy.T
  structure M = CFG
  structure Var = M.Var
  structure Ty = CFGTy
  structure W = Word64
  structure Cells = MLTreeComp.I.C

  val wordSzB = Word.toInt Spec.wordSzB
  val wordAlignB = Word.toInt Spec.wordAlignB
  val ty = MTy.wordTy

  val memory = ManticoreRegion.memory
  val apReg = Regs.apReg

  fun intLit i = T.LI (T.I.fromInt (ty, i))
  fun litFromInt i = T.LI (T.I.fromInt (ty, i))
  fun regExp r = T.REG (ty, r)
  fun move' (ty, r, e) = T.MV (ty, r, e)
  fun move (r, e) = move' (ty, r, e)
  fun offAp i = T.ADD (ty, regExp apReg, litFromInt i)
  fun gpReg r = MTy.GPReg (ty, r)
  fun mltGPR r = MTy.GPR (ty, r)

  val alignedTySzB = Types.alignedTySzB

  fun offsetOf {tys, i} =
      let fun offset (ty :: tys, j, sz) =
	      if (j >= i) then sz
	      else offset (tys, j+1, alignedTySzB ty + sz)
	    | offset ([], _, _) = raise Fail ("offset of type "^
			CFGTy.toString (M.T_Tuple tys)^Int.toString (length tys))
      in 
	  offset (tys, 0, 0) 
      end (* offsetOf *)

  fun select {lhsTy : T.ty, mty : M.ty, i : int, base : T.rexp} =
      let fun offsetOf' ( 
	      M.T_Tuple tys
	    | M.T_Code tys
	    | M.T_OpenTuple tys ) = 
	      (offsetOf {tys=tys, i=i}, List.nth (tys, i))
	    | offsetOf' (M.T_StdCont {clos, arg}) = 
	      (offsetOf {tys=[clos, arg], i=i}, List.nth ([clos, arg], i))
	    | offsetOf' (M.T_Wrap ty) = 
	      (offsetOf {tys=[M.T_Raw ty], i=0}, M.T_Raw ty)
	    | offsetOf' _ = 
	      raise Fail ("offsetOf': non-tuple type "^CFGTy.toString mty)
	  val (offset, lhsMTy) = offsetOf' mty
      in 
	  (case MTy.cfgTyToMLRisc lhsMTy
	    of MTy.K_FLOAT => 
	       MTy.FEXP (lhsTy, T.FLOAD (lhsTy, 
			T.ADD (ty, base, intLit offset), memory))
	     | MTy.K_INT =>
	       MTy.EXP (lhsTy, T.LOAD (lhsTy, 
			T.ADD (ty, base, intLit offset), memory))
	  (* esac *))
      end (* select *)

  fun isTyPointer ( M.T_Any | M.T_Wrap _ | M.T_Tuple _ | 
		    M.T_OpenTuple _ | M.T_Code _ ) = true
    | isTyPointer _ = false

  fun setBit (w, i, ty) = if (isTyPointer ty) then W.orb (w, W.<< (0w1, i)) else w

  fun genAlloc [] = { ptr=MTy.EXP (ty, litFromInt 0), stms=[] }
    | genAlloc args = 
      let fun initLoc ((ty, mltree), (i, stms, totalSize, tyMask)) =
	      let val store = MTy.store (offAp totalSize, mltree, memory)
		  val tyMask' = setBit (tyMask, Word.fromInt i, ty)
		  val totalSize' = alignedTySzB ty + totalSize
	      in
		  (i+1, store :: stms, totalSize', tyMask')
	      end (* initLoc *)
	  val (nWords, stms, totalSize, hdrWord) = foldl initLoc (0, [], 0, 0w0) args
(* FIXME: using all mixed objects at first. *)
	  val hdrWord = W.toLargeInt (
		W.+ (W.orb (W.<< (hdrWord, 0w7), 
			    W.<< (W.fromInt nWords, 0w1)), 0w1) )
	  val stms = 
	      MTy.store (offAp (~wordSzB), MTy.EXP (ty, T.LI hdrWord), memory) 
	        :: stms
	  val ptrReg = Cells.newReg ()
      in	  
	  if ((Word.fromInt totalSize) > Spec.maxObjectSzB)
	  then raise Fail "object size too large"
	  else
	      { ptr=mltGPR ptrReg,
		stms=move (ptrReg, regExp apReg) ::
		     rev (move (apReg, offAp (totalSize+wordSzB)) :: stms) }
      end (* genAlloc *)

  fun genWrap (mty, arg) = genAlloc [(mty, arg)]

  (* The allocation chunk size is 2^n; the high (32-n) bits of allocMask are 1
   * and the low n bits are 0. *)
  val apMask = T.LI (Word.toLargeInt (Word.notb (Spec.allocChunkSzB - 0w1)))

  (* generate code to check the limit pointer.  In pseudo-code, the test is
   *
   *      if (-sz > (ap | allocMask)) goto NoGC;
   *      ap = callgc (ap);
   *	NoGC:;
   *
   * The oring of the allocation-pointer (ap) with allocMask has the effect
   * computing the negation of the amount of free space in the allocation
   * chunk. *)
  fun genAllocCheck szB =
      T.CMP (ty, T.Basis.GT, T.LI (Word.toLargeInt szB), 
	     T.ORB (ty, T.REG (ty, Regs.apReg), apMask))

end (* Alloc64Fn *)
