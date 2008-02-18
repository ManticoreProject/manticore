(* prim-gen-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

signature PRIM_GEN =
  sig

    structure BE : BACK_END

    type ctx = {varDefTbl : BE.VarDef.var_def_tbl}
	       
    val genPrim : ctx -> {gen : (CFG.var * CFG.prim) -> BE.MTy.T.stm list}
			
  end (* PRIM_GEN *)

functor PrimGenFn (structure BE : BACK_END) : PRIM_GEN =
  struct

    structure BE = BE
    structure MTy = BE.MTy
    structure T = MTy.T
    structure M = CFG
    structure Var = M.Var
    structure Ty = CFGTy
    structure P = Prim
    structure Cells = BE.MLTreeComp.I.C

    type ctx = {varDefTbl : BE.VarDef.var_def_tbl}

    val anyTy = 64	(* MLRISC type of "any" *)
    val i32ty = 32
    val i64ty = 64
    val f32ty = 32
    val f64ty = 64
    val boolTy = 64

    fun wordLit i = T.LI (T.I.fromInt (i64ty, i))

    fun genPrim {varDefTbl} = let
	  val getDefOf = BE.VarDef.getDefOf varDefTbl
	  val setDefOf = BE.VarDef.setDefOf varDefTbl
	  val defOf = BE.VarDef.defOf varDefTbl
	  val fdefOf = BE.VarDef.fdefOf varDefTbl
	  val cdefOf = BE.VarDef.cdefOf varDefTbl
	  val gprBind = BE.VarDef.gprBind varDefTbl
	  val cbind = BE.VarDef.cbind varDefTbl
	  val fbind = BE.VarDef.fbind varDefTbl

	  fun gen (v, p) = let
		fun genCmp (ty, c, (v1, v2)) = 
		      cbind (v, T.CMP (ty, c, defOf v1, defOf v2))
		fun genFCmp (ty, c, (v1, v2)) = 
		      cbind (v, T.FCMP (ty, c, fdefOf v1, fdefOf v2))
		fun genArith1 (ty, oper, v') = 
		      gprBind (ty, v, oper (ty, defOf v'))
		fun genArith2 (ty, oper, (v1, v2)) = 
		      gprBind (ty, v, oper (ty, defOf v1, defOf v2))
		fun genFArith1 (fty, oper, v') = 
		      fbind (fty, v, oper (fty, fdefOf v'))
		fun genFArith2 (fty, oper, (v1, v2)) = 
		      fbind (fty, v, oper (fty, fdefOf v1, fdefOf v2))
		fun divs (ty, a, b) = T.DIVS(T.DIV_TO_ZERO, ty, a, b)
		fun rems (ty, a, b) = T.REMS(T.DIV_TO_ZERO, ty, a, b)
		in
		  case p
		   of P.isBoxed p => 
			cbind (v, T.CMP(anyTy, T.EQ, T.ANDB(anyTy, defOf p, wordLit 1), wordLit 0))
		    | P.isUnboxed p => 
			cbind (v, T.CMP(anyTy, T.NE, T.ANDB(anyTy, defOf p, wordLit 1), wordLit 0))
		    | P.Equal a => genCmp (anyTy, T.EQ, a)
		    | P.NotEqual a => genCmp (anyTy, T.NE, a)
		    | P.BNot a => genArith1 (i64ty, T.NOTB, a)
		    | P.BEq a => genCmp (anyTy, T.EQ, a)
		    | P.BNEq a => genCmp (anyTy, T.NE, a)
		   (* 32-bit integer primitives *)				  
		    | P.I32Add a => genArith2 (i32ty, T.ADD, a)
		    | P.I32Sub a => genArith2 (i32ty, T.SUB, a)
		    | P.I32Mul a => genArith2 (i32ty, T.MULS, a)
		    | P.I32Div a => genArith2 (i32ty, divs, a)
		    | P.I32Mod a => genArith2 (i32ty, rems, a)
		    | P.I32Neg a => genArith1 (i32ty, T.NEG, a)
		    | P.I32Eq a => genCmp (i32ty, T.EQ, a)
		    | P.I32NEq a => genCmp (i32ty, T.NE, a)
		    | P.I32Lt a => genCmp (i32ty, T.LT, a)
		    | P.I32Lte a => genCmp (i32ty, T.LE, a)
		    | P.I32Gt a => genCmp (i32ty, T.GT, a)
		    | P.I32Gte a => genCmp (i32ty, T.GE, a)
		   (* 64-bit integer primitives *)
		    | P.I64Add a => genArith2 (i64ty, T.ADD, a)
		    | P.I64Sub a => genArith2 (i64ty, T.SUB, a)
		    | P.I64Mul a => genArith2 (i64ty, T.MULS, a)
		    | P.I64Div a => genArith2 (i64ty, divs, a)
		    | P.I64Mod a => genArith2 (i64ty, rems, a)
		    | P.I64Neg a => genArith1 (i64ty, T.NEG, a)
		    | P.I64Eq a => genCmp (i64ty, T.EQ, a)
		    | P.I64NEq a => genCmp (i64ty, T.NE, a)
		    | P.I64Lt a => genCmp (i64ty, T.LT, a)
		    | P.I64Lte a => genCmp (i64ty, T.LE, a)
		    | P.I64Gt a => genCmp (i64ty, T.GT, a)
		    | P.I64Gte a => genCmp (i64ty, T.GE, a)
		  (* 32-bit floating-point *)
		    | P.F32Add a => genFArith2 (f32ty, T.FADD, a)
		    | P.F32Sub a => genFArith2 (f32ty, T.FSUB, a)
		    | P.F32Mul a => genFArith2 (f32ty, T.FMUL, a)
		    | P.F32Div a => genFArith2 (f32ty, T.FDIV, a)
		    | P.F32Neg a => genFArith1 (f32ty, T.FNEG, a)
		    | P.F32Sqrt a => genFArith1 (f32ty, T.FSQRT, a)
		    | P.F32Abs a => genFArith1 (f32ty, T.FABS, a)
		    | P.F32Eq a => genFCmp (f32ty, T.==, a)
		    | P.F32NEq a => genFCmp (f32ty, T.<>, a)
		    | P.F32Lt a => genFCmp (f32ty, T.<, a)
		    | P.F32Lte a => genFCmp (f32ty, T.<=, a)
		    | P.F32Gt a => genFCmp (f32ty, T.>, a)
		    | P.F32Gte a => genFCmp (f32ty, T.>=, a)
		  (* 64-bit floating-point *)
		    | P.F64Add a => genFArith2 (f64ty, T.FADD, a)
		    | P.F64Sub a => genFArith2 (f64ty, T.FSUB, a)
		    | P.F64Mul a => genFArith2 (f64ty, T.FMUL, a)
		    | P.F64Div a => genFArith2 (f64ty, T.FDIV, a)
		    | P.F64Neg a => genFArith1 (f64ty, T.FNEG, a)
		    | P.F64Sqrt a => genFArith1 (f64ty, T.FSQRT, a)
		    | P.F64Abs a => genFArith1 (f64ty, T.FABS, a)
		    | P.F64Eq a => genFCmp (f64ty, T.==, a)
		    | P.F64NEq a => genFCmp (f64ty, T.<>, a)
		    | P.F64Lt a => genFCmp (f64ty, T.<, a)
		    | P.F64Lte a => genFCmp (f64ty, T.<=, a)
		    | P.F64Gt a => genFCmp (f64ty, T.>, a)
		    | P.F64Gte a => genFCmp (f64ty, T.>=, a)
		  (* conversions *)
		    | P.I32ToI64X x => gprBind (i32ty, v, T.SX(i32ty, i64ty, defOf x))
		    | P.I32ToI64 x => gprBind (i32ty, v, T.ZX(i32ty, i64ty, defOf x))
		    | P.I32ToF32 x => fbind (f32ty, v, T.CVTI2F(f32ty, i32ty, defOf x))
		    | P.I32ToF64 x => fbind (f64ty, v, T.CVTI2F(f64ty, i32ty, defOf x))
		    | P.I64ToF32 x => fbind (f32ty, v, T.CVTI2F(f32ty, i64ty, defOf x))
		    | P.I64ToF64 x => fbind (f64ty, v, T.CVTI2F(f64ty, i64ty, defOf x))
		    | P.F64ToI32 x => gprBind (i32ty, v, T.CVTF2I(i32ty, T.Basis.TO_NEAREST, f64ty, fdefOf x))
		  (* atomic operations *)
		    | P.I32FetchAndAdd(addr, x) => let
			val (r, stms) = BE.AtomicOps.genFetchAndAdd32 {
				 addr=T.LOAD(i32ty, defOf addr, ()),
				 x=defOf x
			       }
			in
			  BE.VarDef.flushLoads varDefTbl
			  @ stms
			  @ gprBind (i32ty, v, r)
			end
		    | P.I64FetchAndAdd(addr, x) => let
			val (r, stms) = BE.AtomicOps.genFetchAndAdd64 {
				 addr=T.LOAD(i64ty, defOf addr, ()),
				 x=defOf x
			       }
			in
			  BE.VarDef.flushLoads varDefTbl
			  @ stms
			  @ gprBind (i64ty, v, r)
			end
		    | P.CAS(addr, key, new) => let
			val (_, r, stms) = BE.AtomicOps.genCompareAndSwapWord{
				    addr = T.LOAD(anyTy, defOf addr, ()),
				    cmpVal = defOf key, newVal = defOf new
				  }
			in
			  BE.VarDef.flushLoads varDefTbl
			  @ stms
			  @ gprBind (anyTy, v, r)
			end
		    | P.TAS addr => let
			val tmp = Cells.newReg ()
			val (r, stms) = BE.AtomicOps.genTestAndSetWord{
				    addr = T.LOAD(boolTy, defOf addr, ()),
				    newVal = tmp
				  }
			in
			  BE.VarDef.flushLoads varDefTbl
			  @ [T.MV (boolTy, tmp, T.LI BE.Spec.trueRep)]
			  @ stms
			  @ gprBind (boolTy, v, r)
			end
		    | P.BCAS(addr, key, new) => let
			val (cc, _, stms) = BE.AtomicOps.genCompareAndSwapWord{
				    addr = T.LOAD(boolTy, defOf addr, ()),
				    cmpVal = defOf key, newVal = defOf new
				  }
			in
			  BE.VarDef.flushLoads varDefTbl
			  @ stms
			  @ cbind (v, cc)
			end
(*
		    | _ => raise Fail(concat[
			  "genPrim(", CFG.Var.toString v, ", ",
			  PrimUtil.fmt CFG.Var.toString p, ")"
			])
*)
		  (* esac *)
		end (* gen *)
	  in
	    {gen=gen}
	  end (* genPrim *)


  end (* PrimGenFn *)
