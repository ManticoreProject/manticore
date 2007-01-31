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

  val wordSzB = Word.toInt Spec.wordSzB
  val wordAlignB = Word.toInt Spec.wordAlignB
  val ty = wordSzB * 8

  val memory = ManticoreRegion.memory
  val apReg = Regs.apReg

  fun litFromInt i = T.LI (T.I.fromInt (ty, i))
  fun regExp r = T.REG (ty, r)
  fun move (r, e) = T.MV (ty, r, e)
  fun addAp i = T.ADD (ty, regExp apReg, litFromInt i)

  val alignedTySzB = Types.alignedTySzB

  (* for now, the offset of the ith element in a tuple is the sum of the
   * sizes of previous types. *)
  fun offsetOf {tys, i} =
      let fun offset (ty :: tys, j, sz) =
	      if (j >= i) then sz
	      else offset (tys, j+1, alignedTySzB ty + sz)
      in 
	  offset (tys, 0, 0) 
      end (* offsetOf *)

  fun genAlloc args = 
      let fun initLoc ((ty, mltree), (stms, totalSize)) =
	      let val store = MTy.store (addAp totalSize, mltree, memory)
	      in
		  (store :: stms, alignedTySzB ty + totalSize)
	      end (* initLoc *)
	  val (stms, totalSize) = foldl initLoc ([], 0) args
      in	  
	  rev (move (apReg, addAp totalSize) :: stms)
      end (* genAlloc *)

end (* Alloc64Fn *)
