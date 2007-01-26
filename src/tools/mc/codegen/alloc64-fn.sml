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
) : ALLOC = struct

  structure MTy = MTy
  structure T = MTy.T
  structure M = CFG
  structure Var = M.Var
  structure Ty = CFGTy

  val wordSzB = 8
  val ty = wordSzB * 8

  val memory = ManticoreRegion.memory
  val apReg = Regs.apReg

  fun litFromInt i = T.LI (T.I.fromInt (ty, i))
  fun regExp r = T.REG (ty, r)
  fun mkExp e = MTy.EXP (ty, e)
  fun load addr = T.LOAD (ty, addr, memory)
  fun store (ea, data) = T.STORE (ty, ea, data, memory)
  fun move (r, e) = T.MV (ty, r, e)

  fun sizeOfTy (M.T_Raw rt | M.T_Wrap rt) =
      (case rt
	of Ty.T_Byte => 8
	 | Ty.T_Short => 16
	 | Ty.T_Int => 32
	 | Ty.T_Long => 64
	 | Ty.T_Float => 32
	 | Ty.T_Double => 64
	 | Ty.T_Vec128 => 128
      (* escac *))
    | sizeOfTy (M.T_Tuple ts) = foldl (fn (t, acc) => sizeOfTy t + acc) 0 ts
    | sizeOfTy _ = wordSzB

  fun genAlloc args = 
      let val tys = map (fn (ty, _) => ty) args
	  val totalSize = foldl (fn (ty, acc) => sizeOfTy ty + acc) 0 tys
	  val apMv = move (apReg, T.ADD (ty, regExp apReg , litFromInt totalSize))
(* perform and record the stores here *)
      in	  
	  [apMv]
      end (* genAlloc *)

end (* Alloc64Fn *)
