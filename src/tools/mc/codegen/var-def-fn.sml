(* var-def-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A lookup table for Manticore variables.
 *)

functor VarDefFn (
	structure Spec : TARGET_SPEC
	structure MTy : MLRISC_TYPES
	structure MLTreeComp : MLTREECOMP where TS.T = MTy.T
) : VAR_DEF = struct

  structure Cells = MLTreeComp.I.C
  structure MTy = MTy
  structure Tbl = CFG.Var.Tbl
  structure T = MTy.T

  val wordTy = MTy.wordTy
  (* representations of some simple constants *)
  val valTRUE = T.LI Spec.trueRep
  val valFALSE = T.LI Spec.falseRep

  (* the ML-Risc tree that converts a cexp to an rexp  *)
  fun cexpToExp exp = T.COND (wordTy, exp, valTRUE, valFALSE)

  type var_def_tbl = MTy.mlrisc_tree Tbl.hash_table

  fun newTbl () = Tbl.mkTable (256, Fail "varDefTbl")
  fun getDefOf vdt v = (case Tbl.find vdt v
	 of SOME def => def
	  | NONE => raise Fail(concat["getDefOf(", CFG.Var.toString v, ")"])
	(* end case *))
  fun setDefOf vdt (v,e) = Tbl.insert vdt (v,e)

  fun defOf vdt v =
      (case getDefOf vdt v
	of MTy.EXP (_, e) => e
	 | MTy.GPR (ty, r) => T.REG (ty, r)
	 | MTy.CEXP ce => cexpToExp ce
	 | _ => raise Fail "defOf"
      (* esac *))

  fun fdefOf vdt v =       
      (case getDefOf vdt v
	of MTy.FEXP (_, fe) => fe
	 | MTy.FPR (fty, r) => T.FREG (fty, r)
	 | _ => raise Fail "fdefOf"
      (* esac *))

  fun cdefOf vdt v = 
      (case getDefOf vdt v
	of MTy.CEXP ce => ce
	 | MTy.GPR r => T.CMP (wordTy, T.Basis.NE, T.REG r, valFALSE)
	 | MTy.EXP (_, exp) => T.CMP (wordTy, T.Basis.NE, exp, valFALSE)
	 | _ => raise Fail ("cdefOf " ^ CFG.Var.toString v)
      (* esac *))

  fun bind vdt (ty, x, e) = setDefOf vdt (x, MTy.EXP (ty, e))
  fun cbind vdt (x, c) = setDefOf vdt (x, MTy.CEXP c)
  fun fbind vdt (ty, x, fe) = setDefOf vdt (x, MTy.FEXP (ty, fe))

end (* VarDefFn *)
