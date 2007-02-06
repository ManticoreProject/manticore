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
) : ALLOC = struct

  structure MTy = MTy
  structure T = MTy.T
  structure M = CFG
  structure Var = M.Var
  structure Ty = CFGTy
  structure W = Word64

  val wordSzB = Word.toInt Spec.wordSzB
  val wordAlignB = Word.toInt Spec.wordAlignB
  val ty = wordSzB * 8

  val memory = ManticoreRegion.memory
  val apReg = Regs.apReg

  fun litFromInt i = T.LI (T.I.fromInt (ty, i))
  fun regExp r = T.REG (ty, r)
  fun move (r, e) = T.MV (ty, r, e)
  fun offAp i = T.ADD (ty, regExp apReg, litFromInt i)
  fun gpReg r = MTy.GPReg (ty, r)
  fun mltGPR r = MTy.GPR (ty, r)

  val alignedTySzB = Types.alignedTySzB

  fun offsetOf {tys, i} =
      let fun offset (ty :: tys, j, sz) =
	      if (j >= i) then sz
	      else offset (tys, j+1, alignedTySzB ty + sz)
      in 
	  offset (tys, 0, 0) 
      end (* offsetOf *)

  fun isTyPointer ( M.T_Any | M.T_Wrap _ | M.T_Tuple _ ) = true
    | isTyPointer _ = false

  fun setBit (w, i, ty) = if (isTyPointer ty) then W.orb (w, W.<< (0w1, i)) else w

  fun genAlloc args = 
      let fun initLoc ((ty, mltree), (i, stms, totalSize, tyMask)) =
	      let val store = MTy.store (offAp totalSize, mltree, memory)
		  val tyMask' = setBit (tyMask, Word.fromInt i, ty)
		  val totalSize' = alignedTySzB ty + totalSize
	      in
		  (i+1, store :: stms, totalSize', tyMask')
	      end (* initLoc *)
	  val (nWords, stms, totalSize, hdrWord) = foldl initLoc (0, [], 0, 0w0) args
	  val hdrWord = W.toLargeInt (W.orb (W.<< (hdrWord, 0w8), W.fromInt nWords))
	  val stms = MTy.store (offAp (~wordSzB), MTy.EXP (ty, T.LI hdrWord), memory) 
		     :: stms
      in	  
	  if ((Word.fromInt totalSize) > Spec.maxObjectSzB)
	  then raise Fail "object size too large"
	  else rev (move (apReg, offAp (totalSize+wordSzB)) :: stms)
      end (* genAlloc *)

  (* The allocation chunk size is 2^n; the high (32-n) bits of allocMask are 1
   * and the low n bits are 0. *)
  val apMask = T.LI (W.toLargeInt (W.notb (Spec.allocChunkSzB - 0w1)))

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
