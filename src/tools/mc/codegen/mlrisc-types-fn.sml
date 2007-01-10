(* mlrisc-types-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utility code for dealing with MLRisc types and trees.
 *)

signature MLRISC_TYPES =
  sig

    structure T : MLTREE
      where Region = ManticoreRegion

    datatype mlrisc_kind = K_INT | K_FLT | K_COND

    val kindToString : mlrisc_kind -> string

    datatype mlrisc_tree
      = GPR of (T.ty * T.var)
      | FPR of (T.fty * T.var)
      | EXP of (T.ty * T.rexp)
      | FEXP of (T.fty * T.fexp)
      | CEXP of T.ccexp

    val treeToString : mlrisc_tree -> string
    val kindOf : mlrisc_tree -> mlrisc_kind
    val mlriscToTree : T.mlrisc -> mlrisc_tree
    val treeToMLRisc : mlrisc_tree -> T.mlrisc

    datatype mlrisc_reg
      = GPReg of (T.ty * T.var)
      | FPReg of (T.fty * T.var)

    val regToString : mlrisc_reg -> string
    val kindOfReg : mlrisc_reg -> mlrisc_kind
    val regToTree : mlrisc_reg -> mlrisc_tree
    val regToMLRisc : mlrisc_reg -> T.mlrisc

  (* MLRisc types *)
    val i32Ty : T.ty
    val i64Ty : T.ty
    val f32Ty : T.fty
    val f64Ty : T.fty
    val fxTy : T.fty		(* extended floating-point *)
    val pTy : T.ty		(* pointers *)
    val bTy : T.ty		(* size of boolean *)

    val mlriscTypeOf  : T.rexp -> T.ty
    val mlriscFTypeOf : T.fexp -> T.fty

  (* type dependent load/stores *)
    val store : (T.rexp * mlrisc_tree * T.Region.region) -> T.stm

  end

functor MLRiscTypesFn (

    structure Spec : TARGET_SPEC
    structure T : MLTREE
      where Region = ManticoreRegion

  ) : MLRISC_TYPES = struct

    structure T = T
    structure Ty = BOLTypes
    structure F = Format

  (* MLRisc kinds *)
    datatype mlrisc_kind = K_INT | K_FLT | K_COND

    fun kindToString K_INT = "K_INT"
      | kindToString K_FLT = "K_FLT"
      | kindToString K_COND = "K_COND"

    datatype mlrisc_tree
      = GPR of (T.ty * T.var)
      | FPR of (T.fty * T.var)
      | EXP of (T.ty * T.rexp)
      | FEXP of (T.fty * T.fexp)
      | CEXP of T.ccexp

    fun treeToMLRisc (GPR(ty, r)) = T.GPR(T.REG(ty, r))
      | treeToMLRisc (FPR(ty, r)) = T.FPR(T.FREG(ty, r))
      | treeToMLRisc (EXP(_, exp)) = T.GPR exp
      | treeToMLRisc (FEXP(_, fexp)) = T.FPR fexp
      | treeToMLRisc (CEXP cexp) = T.CCR cexp

    fun treeToString (GPR(ty, x)) = CellsBasis.toStringWithSize(x, ty)
      | treeToString (FPR(ty, x)) = CellsBasis.toStringWithSize(x, ty)
      | treeToString (EXP(ty, exp)) = F.format "EXP:%d" [F.INT ty]
      | treeToString (FEXP(ty, exp)) = F.format "FEXP:%d" [F.INT ty]
      | treeToString (CEXP exp) = "CEXP"

    fun kindOf (GPR _) = K_INT
      | kindOf (FPR _) = K_FLT
      | kindOf (EXP _) = K_INT
      | kindOf (FEXP _) = K_FLT
      | kindOf (CEXP _) = K_COND

    datatype mlrisc_reg
      = GPReg of (T.ty * T.var)
      | FPReg of (T.fty * T.var)

    fun regToString (GPReg(ty, x)) = CellsBasis.toStringWithSize(x, ty)
      | regToString (FPReg(ty, x)) = CellsBasis.toStringWithSize(x, ty)
    fun kindOfReg (GPReg _) = K_INT
      | kindOfReg (FPReg _) = K_FLT
    fun regToTree (GPReg r) = GPR r
      | regToTree (FPReg r) = FPR r
    fun regToMLRisc (GPReg r) = T.GPR(T.REG r)
      | regToMLRisc (FPReg r) = T.FPR(T.FREG r)

  (* MLRisc types *)
    val i32Ty = 32			(* 32-bit integers *)
    val i64Ty = 64
    val f32Ty = 32
    val f64Ty = 64
    val pTy = (8 * Spec.wordSzB)	(* pointers *)
    val fxTy = (8 * Spec.extendedSzB)	(* extended floating-point *)
    val bTy = (8* Spec.boolSzB)

(** NOTE: the following two functions really ought to be part of MLRISC!! **)
  (* return the type of an MLRisc expression *)
    fun mlriscTypeOf (T.REG(ty, _)) = ty
      | mlriscTypeOf (T.LI _) = raise Fail "mlriscTypeOf: LI"
      | mlriscTypeOf (T.LABEL _) = pTy
      | mlriscTypeOf (T.CONST _) = raise Fail "mlriscTypeOf: CONST"
      | mlriscTypeOf (T.LABEXP _) = pTy
      | mlriscTypeOf (T.NEG(ty, _)) = ty
      | mlriscTypeOf (T.ADD(ty, _, _)) = ty
      | mlriscTypeOf (T.SUB(ty, _, _)) = ty
      | mlriscTypeOf (T.MULS(ty, _, _)) = ty
      | mlriscTypeOf (T.DIVS(_, ty, _, _)) = ty
      | mlriscTypeOf (T.REMS(_, ty, _, _)) = ty
      | mlriscTypeOf (T.MULU(ty, _, _)) = ty
      | mlriscTypeOf (T.DIVU(ty, _, _)) = ty
      | mlriscTypeOf (T.REMU(ty, _, _)) = ty
      | mlriscTypeOf (T.NEGT(ty, _)) = ty
      | mlriscTypeOf (T.ADDT(ty, _, _)) = ty
      | mlriscTypeOf (T.SUBT(ty, _, _)) = ty
      | mlriscTypeOf (T.MULT(ty, _, _)) = ty
      | mlriscTypeOf (T.DIVT(_, ty, _, _)) = ty
      | mlriscTypeOf (T.ANDB(ty, _, _)) = ty
      | mlriscTypeOf (T.ORB(ty, _, _)) = ty
      | mlriscTypeOf (T.XORB(ty, _, _)) = ty
      | mlriscTypeOf (T.EQVB(ty, _, _)) = ty
      | mlriscTypeOf (T.NOTB(ty, _)) = ty
      | mlriscTypeOf (T.SRA(ty, _, _)) = ty
      | mlriscTypeOf (T.SRL(ty, _, _)) = ty
      | mlriscTypeOf (T.SLL(ty, _, _)) = ty
      | mlriscTypeOf (T.SX(_, ty, _)) = ty
      | mlriscTypeOf (T.ZX(_, ty, _)) = ty
      | mlriscTypeOf (T.CVTF2I(ty, _, _, _)) = ty
      | mlriscTypeOf (T.COND(ty, _, _, _)) = ty
      | mlriscTypeOf (T.LOAD(ty, _, _)) = ty
      | mlriscTypeOf (T.PRED(rexp, _)) = mlriscTypeOf rexp
      | mlriscTypeOf (T.LET(_, rexp)) = mlriscTypeOf rexp
      | mlriscTypeOf (T.REXT(ty, _)) = ty
      | mlriscTypeOf (T.MARK(rexp, _)) = mlriscTypeOf rexp
      | mlriscTypeOf (T.OP _) = raise Fail "mlriscTypeOf(OP)"
      | mlriscTypeOf (T.ARG _) = raise Fail "mlriscTypeOf(ARG)"
      | mlriscTypeOf (T.$ _) = raise Fail "mlriscTypeOf($)"
      | mlriscTypeOf (T.PARAM _) = raise Fail "mlriscTypeOf(PARAM)"
      | mlriscTypeOf (T.BITSLICE _) = raise Fail "mlriscTypeOf(BITSLICE)"
      | mlriscTypeOf (T.???) = raise Fail "mlriscTypeOf(???)"

  (* return the type of an MLRisc floating-point expression *)
    fun mlriscFTypeOf (T.FREG(ty, _)) = ty
      | mlriscFTypeOf (T.FLOAD(ty, _, _)) = ty
      | mlriscFTypeOf (T.FADD(ty, _, _)) = ty
      | mlriscFTypeOf (T.FMUL(ty, _, _)) = ty
      | mlriscFTypeOf (T.FSUB(ty, _, _)) = ty
      | mlriscFTypeOf (T.FDIV(ty, _, _)) = ty
      | mlriscFTypeOf (T.FABS(ty, _)) = ty
      | mlriscFTypeOf (T.FNEG(ty, _)) = ty
      | mlriscFTypeOf (T.FSQRT(ty, _)) = ty
      | mlriscFTypeOf (T.FCOND(ty, _, _, _)) = ty
      | mlriscFTypeOf (T.FCOPYSIGN(ty, _, _)) = ty
      | mlriscFTypeOf (T.CVTI2F(ty, _, _)) = ty
      | mlriscFTypeOf (T.CVTF2F(ty, _, _)) = ty
      | mlriscFTypeOf (T.FPRED(fexp, _)) = mlriscFTypeOf fexp
      | mlriscFTypeOf (T.FEXT(ty, _)) = ty
      | mlriscFTypeOf (T.FMARK(fexp, _)) = mlriscFTypeOf fexp

    fun mlriscToTree (T.CCR cexp) = CEXP cexp
      | mlriscToTree (T.GPR exp) = EXP(mlriscTypeOf exp, exp)
      | mlriscToTree (T.FPR fexp) = FEXP(mlriscFTypeOf fexp, fexp)

    val i32Ty = 32
    val valTRUE = T.LI Spec.trueRep
    val valFALSE = T.LI Spec.falseRep
    fun cexpToExp exp = T.COND(i32Ty, exp, valTRUE, valFALSE)

  (* type dependent store *)
    fun store (dst, GPR(ty, r), rg) = T.STORE(ty, dst, T.REG(ty, r), rg)
      | store (dst, FPR(fty, r), rg) = T.FSTORE(fty, dst, T.FREG(fty, r), rg)
      | store (dst, EXP(ty, rexp), rg) = T.STORE(ty, dst, rexp, rg)
      | store (dst, FEXP(fty, fexp), rg) = T.FSTORE(fty, dst, fexp, rg)
      | store (dst, CEXP cexp, rg) = T.STORE(i32Ty, dst, cexpToExp cexp, rg)

  end
