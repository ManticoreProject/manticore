(* mlrisc-types-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Special representations of MLRISC types.
 *)

signature MLRISC_TYPES = sig

    structure T : MLTREE
	where Region = ManticoreRegion 

    datatype mlrisc_kind = K_INT | K_FLOAT | K_COND

    datatype mlrisc_tree = GPR  of (T.ty * T.var)
			 | FPR  of (T.fty * T.var)
			 | EXP  of (T.ty * T.rexp)
			 | FEXP of (T.fty * T.fexp)
			 | CEXP of T.ccexp

    datatype mlrisc_reg = GPReg of (T.ty * T.var)
			| FPReg of (T.fty * T.var)

    val kindOf : mlrisc_tree -> mlrisc_kind
    val treeToMLRisc : mlrisc_tree -> T.mlrisc
    val mlriscTypeOf : T.rexp -> T.ty
    val mlriscFTypeOf : T.fexp -> T.ty
    val regToTree : mlrisc_reg -> mlrisc_tree
    val gprToExp : mlrisc_reg -> T.mlrisc
    val cfgTyToMLRisc : CFGTy.ty -> mlrisc_kind
    val treeToString : mlrisc_tree -> string

    val wordTy : T.ty

    (* type dependent load/stores *)
    val store : (T.rexp * mlrisc_tree * T.Region.region) -> T.stm

end (* MLRISC_TYPES *)

functor MLRiscTypesFn (
	structure Spec : TARGET_SPEC
	structure T : MLTREE
		where Region = ManticoreRegion
) : MLRISC_TYPES = struct

  val ty = (Word.toInt Spec.wordSzB) * 8
  val wordTy = ty

  structure T = T
  structure F = Format
  structure MTS = MLTreeSize (
    structure T = T
    val intTy = ty )

  datatype mlrisc_kind = K_INT | K_FLOAT | K_COND
					   
  datatype mlrisc_tree = GPR  of T.ty * T.var
		       | FPR  of T.fty * T.var
		       | EXP  of T.ty * T.rexp
		       | FEXP of T.fty * T.fexp
		       | CEXP of T.ccexp  

  datatype mlrisc_reg = GPReg of (T.ty * T.var)
		      | FPReg of (T.fty * T.var)

  fun kindOf ( (GPR _ | EXP _) ) = K_INT
    | kindOf ( (FPR _ | FEXP _) ) = K_FLOAT
    | kindOf ( CEXP _  ) = K_COND   

  fun treeToString (GPR(ty, x)) = CellsBasis.toStringWithSize(x, ty)
    | treeToString (FPR(ty, x)) = CellsBasis.toStringWithSize(x, ty)
    | treeToString (FEXP (ty, T.FREG (_, x))) = CellsBasis.toStringWithSize(x, ty)
    | treeToString (EXP(ty, exp)) = F.format "EXP:%d" [F.INT ty]
    | treeToString (FEXP(ty, exp)) = F.format "FEXP:%d" [F.INT ty]
    | treeToString (CEXP exp) = "CEXP"
				
  fun treeToMLRisc (GPR (ty, r)) = T.GPR (T.REG (ty, r))
    | treeToMLRisc (FPR (ty, r)) = T.FPR (T.FREG (ty, r))
    | treeToMLRisc (EXP (_, exp)) = T.GPR exp
    | treeToMLRisc (FEXP (_, fexp)) = T.FPR fexp
    | treeToMLRisc (CEXP cexp) = T.CCR cexp

  val mlriscTypeOf = MTS.size
  val mlriscFTypeOf = MTS.fsize

  fun mlriscToTree (T.CCR cexp) = CEXP cexp
    | mlriscToTree (T.GPR exp) = EXP (mlriscTypeOf exp, exp)
    | mlriscToTree (T.FPR fexp) = FEXP (mlriscFTypeOf fexp, fexp)

  fun regToTree (GPReg (ty, v)) = GPR (ty, v)
    | regToTree (FPReg (fty, fv)) = FPR (fty, fv)

  val valTRUE = T.LI Spec.trueRep
  val valFALSE = T.LI Spec.falseRep
  fun cexpToExp exp = T.COND(ty, exp, valTRUE, valFALSE)

  (* type dependent store *)
  fun store (dst, GPR(ty, r), rg) = T.STORE(ty, dst, T.REG(ty, r), rg)
    | store (dst, FPR(fty, r), rg) = T.FSTORE(fty, dst, T.FREG(fty, r), rg)
    | store (dst, EXP(ty, rexp), rg) = T.STORE(ty, dst, rexp, rg)
    | store (dst, FEXP(fty, fexp), rg) = T.FSTORE(fty, dst, fexp, rg)
    | store (dst, CEXP cexp, rg) = T.STORE(ty, dst, cexpToExp cexp, rg)

  fun gprToExp (GPReg (ty, r)) = T.GPR (T.REG (ty, r))
    | gprToExp (FPReg (fty, r)) = T.FPR (T.FREG (fty, r))

  structure Ty = CFGTy

  fun cfgTyToMLRisc ty =
      (case ty 
	of ( Ty.T_Raw Ty.T_Float | Ty.T_Raw Ty.T_Double ) => K_FLOAT
	 | _ => K_INT
      (* esac *))

end (* MLRiscTypesFn *)
