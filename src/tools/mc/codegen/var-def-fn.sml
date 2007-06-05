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

  (* the ML-Risc tree that converts a cexp to a GPR *)
  fun cexpToGPR (dstReg, srcExp) = T.MV (wordTy, dstReg, cexpToExp srcExp)

  type var_def_tbl = MTy.mlrisc_tree Tbl.hash_table

  fun newTbl () = Tbl.mkTable (256, Fail "varDefTbl")

  fun getDefOf vdt v = (case Tbl.find vdt v
	 of SOME def => def
	  | NONE => raise Fail(concat["getDefOf(", CFG.Var.toString v, ")"])
	(* end case *))

  fun getAndClrDefOf vdt = Tbl.remove vdt

  fun useDefOf vdt v = if (CFG.Var.useCount v = 1)
	then getAndClrDefOf vdt v
	else getDefOf vdt v

  fun setDefOf vdt (v,e) = Tbl.insert vdt (v,e)

  fun defOf vdt v = (case useDefOf vdt v
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

  (* emit any MLRISC trees in the table that contain loads *)
  fun flushLoads vdt = let
      (* test an rexp for memory loads *)
      fun hasLoad (T.REG _) = false
	| hasLoad (T.LI _) = false
	| hasLoad (T.LABEL _) = false
	| hasLoad (T.CONST _) = false
	| hasLoad (T.LABEXP _) = false
	| hasLoad (T.NEG(_, e)) = hasLoad e
	| hasLoad (T.ADD(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.SUB(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.MULS(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.DIVS(_, _, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.REMS(_, _, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.MULU(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.DIVU(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.REMU(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.NEGT(_, e)) = hasLoad e
	| hasLoad (T.ADDT(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.SUBT(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.MULT(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.DIVT(_, _, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.ANDB(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.ORB(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.XORB(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.EQVB(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.NOTB(_, e)) = hasLoad e
	| hasLoad (T.SRA(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.SRL(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.SLL(_, e1, e2)) = hasLoad e1 orelse hasLoad e2
	| hasLoad (T.SX(_, _, e)) = hasLoad e
	| hasLoad (T.ZX(_, _, e)) = hasLoad e
	| hasLoad (T.CVTF2I(_, _, _, f)) = true (* FIXME check the fexp! *)
	| hasLoad (T.COND(_, c, e1, e2)) = true (* FIXME check the ccexp! *)
	| hasLoad (T.LOAD _) = true
	| hasLoad _ = raise Fail "strange rexp"
      val stms = ref []
      fun mv (x, def as MTy.EXP (ty, e)) = if (hasLoad e)
          then let
            val r = Cells.newReg ()
            in
              stms := T.MV (ty, r, e) :: !stms;
	      MTy.GPR (ty, r)
            end
          else def
	| mv (x, def as MTy.FEXP (ty, fe)) = let
	  val r = Cells.newFreg ()
	  in
	      stms := T.FMV (ty, r, fe) :: !stms;
	      MTy.FPR (ty, r)
	  end
	| mv (x, MTy.CEXP cexp) = let
	  val r = Cells.newReg ()
	  in
	      stms := cexpToGPR (r, cexp) :: !stms;
	      MTy.GPR (wordTy, r)
	  end
	| mv (_, e) = e
      in
        Tbl.modifyi mv vdt;
	!stms
      end

  fun gprBind vdt (ty, x, e) = if (CFG.Var.useCount x > 1)
      then (case e
	     of T.REG (ty, r) => (setDefOf vdt (x, MTy.GPR (ty, r)); [])
	      | _ => let
		    val r = Cells.newReg ()
		in
		    setDefOf vdt (x, MTy.GPR (ty, r));
		    [T.MV (ty, r, e)]
		end
		    (* esac *))
      else (setDefOf vdt (x, MTy.EXP (ty, e)); [])

  fun cbind vdt (x, c) = if (CFG.Var.useCount x > 1)
      then let
	val r = Cells.newReg ()
	in
           setDefOf vdt (x, MTy.GPR (wordTy, r));
	   [cexpToGPR (r, c)]
	end
      else (setDefOf vdt (x, MTy.CEXP c); [])

  fun fbind vdt (ty, x, fe) = if (CFG.Var.useCount x > 1)
      then (case fe
	     of T.FREG (_, r) => (setDefOf vdt (x, MTy.FPR (ty, r)); [])
	      | _ => let
		val r = Cells.newFreg ()
		in
		    setDefOf vdt (x, MTy.FPR (ty, r));
		    [T.FMV (ty, r, fe)]
		end
	    (* esac *))
      else (setDefOf vdt (x, MTy.FEXP (ty, fe)); [])

  (* Bind a variable to its MLRISC tree. If this variable has a useCount > 1,
   * emit its tree and bind it to a temporary register.  *)
  fun bind vdt (x, t) = (case t
      of MTy.GPR (ty, r) => gprBind vdt (ty, x, T.REG (ty, r))
       | MTy.FPR (fty, r) => fbind vdt (fty, x, T.FREG (fty, r))
       | MTy.EXP (ty, e) => gprBind vdt (ty, x, e)
       | MTy.FEXP (fty, e) => fbind vdt (fty, x, e)
       | MTy.CEXP c => cbind vdt (x, c)
      (* esac *))

end (* VarDefFn *)
