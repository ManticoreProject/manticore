(* prim-gen-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

signature PRIM_GEN =
  sig

    structure BE : BACK_END

    type ctx = {varDefTbl : BE.VarDef.var_def_tbl}

    val genPrim0 : ctx -> CFG.prim -> BE.MTy.T.stm list
    val genPrim : ctx -> (CFG.var * CFG.prim) -> BE.MTy.T.stm list

    val genCond : ctx -> CFG.cond * Label.label -> BE.MTy.T.stm list

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
    val i8Ty = 8
    val i16Ty = 16
    val i32Ty = 32
    val i64Ty = 64
    val f32Ty = 32
    val f64Ty = 64
    val adrTy = 64
    val boolTy = 64

    fun wordLit i = T.LI(T.I.fromInt (i64Ty, i))

    fun arrayOffset {base, i, wordSzB} = T.ADD(MTy.wordTy, base, T.MULS(MTy.wordTy, wordLit wordSzB, i))

    fun genPrim0 {varDefTbl} = let
	  val defOf = BE.VarDef.defOf varDefTbl
	  val fdefOf = BE.VarDef.fdefOf varDefTbl
	  val gprBind = BE.VarDef.gprBind varDefTbl
	  fun gen p = (case p
	      (* array store operations *)
	       of P.ArrStoreI32 (base, i, x) => let
		    val addr = arrayOffset {base=defOf base, i=defOf i, wordSzB=4}
		    in
		      BE.VarDef.flushLoads varDefTbl @
		      [T.STORE(i32Ty, addr, defOf x, ManticoreRegion.memory)]
		   end
		| P.ArrStoreI64(base, i, x) => let
		    val addr = arrayOffset {base=defOf base, i=defOf i, wordSzB=8}
		    in
		      BE.VarDef.flushLoads varDefTbl @
		      [T.STORE(i64Ty, addr, defOf x, ManticoreRegion.memory)]
		    end
		| P.ArrStoreF32 (base, i, x) => let
		    val addr = arrayOffset {base=defOf base, i=defOf i, wordSzB=4}
		    in
		      BE.VarDef.flushLoads varDefTbl @
		      [T.FSTORE(f32Ty, addr, fdefOf x, ManticoreRegion.memory)]
		    end
		| P.ArrStoreF64 (base, i, x) => let
		    val addr = arrayOffset {base=defOf base, i=defOf i, wordSzB=8}
		    in
		      BE.VarDef.flushLoads varDefTbl @
		      [T.FSTORE(f64Ty, addr, fdefOf x, ManticoreRegion.memory)]
		   end
		| P.ArrStore(base, i, x) => let
		    val addr = arrayOffset {base=defOf base, i=defOf i, wordSzB=8}
		    in
		      BE.VarDef.flushLoads varDefTbl @
		      [T.STORE(anyTy, addr, defOf x, ManticoreRegion.memory)]
		    end
             (* address stores *)
		| P.AdrStoreI8(addr, x) => let
		  (* note that we bind the definitions of the address and source before calling flushLoads. this
		   * trick avoids flushing those definitions and gives much better code.
		   *)
		    val addr' = defOf addr and x' = defOf x
		    in
		      BE.VarDef.flushLoads varDefTbl @
		      [T.STORE(i8Ty, addr', x', ManticoreRegion.memory)]
		    end
		| P.AdrStoreI16(addr, x) => let
		  (* note that we bind the definitions of the address and source before calling flushLoads. this
		   * trick avoids flushing those definitions and gives much better code.
		   *)
		    val addr' = defOf addr and x' = defOf x
		    in
		      BE.VarDef.flushLoads varDefTbl @
		      [T.STORE(i16Ty, addr', x', ManticoreRegion.memory)]
		    end
		| P.AdrStoreI32(addr, x) => let
		  (* note that we bind the definitions of the address and source before calling flushLoads. this
		   * trick avoids flushing those definitions and gives much better code.
		   *)
		    val addr' = defOf addr and x' = defOf x
		    in
		      BE.VarDef.flushLoads varDefTbl @
		      [T.STORE(i32Ty, addr', x', ManticoreRegion.memory)]
		    end
		| P.AdrStoreI64(addr, x) => let
		  (* note that we bind the definitions of the address and source before calling flushLoads. this
		   * trick avoids flushing those definitions and gives much better code.
		   *)
		    val addr' = defOf addr and x' = defOf x
		    in
		      BE.VarDef.flushLoads varDefTbl @
		      [T.STORE(i64Ty, addr', x', ManticoreRegion.memory)]
		    end
		| P.AdrStoreF32(addr, x) => let
		  (* note that we bind the definitions of the address and source before calling flushLoads. this
		   * trick avoids flushing those definitions and gives much better code.
		   *)
		    val addr' = defOf addr and x' = fdefOf x
		    in
		      BE.VarDef.flushLoads varDefTbl @
		      [T.FSTORE(f32Ty, addr', x', ManticoreRegion.memory)]
		    end
		| P.AdrStoreF64(addr, x) => let
		  (* note that we bind the definitions of the address and source before calling flushLoads. this
		   * trick avoids flushing those definitions and gives much better code.
		   *)
		    val addr' = defOf addr and x' = fdefOf x
		    in
		      BE.VarDef.flushLoads varDefTbl @
		      [T.FSTORE(f64Ty, addr', x', ManticoreRegion.memory)]
		    end
		| P.AdrStoreAdr(addr, x) => let
		  (* note that we bind the definitions of the address and source before calling flushLoads. this
		   * trick avoids flushing those definitions and gives much better code.
		   *)
		    val addr' = defOf addr and x' = defOf x
		    in
		      BE.VarDef.flushLoads varDefTbl @
		      [T.STORE(anyTy, addr', x', ManticoreRegion.memory)]
		    end
		| P.AdrStore(addr, x) => let
		  (* note that we bind the definitions of the address and source before calling flushLoads. this
		   * trick avoids flushing those definitions and gives much better code.
		   *)
		    val addr' = defOf addr and x' = defOf x
		    in
		      BE.VarDef.flushLoads varDefTbl @
		      [T.STORE(anyTy, addr', x', ManticoreRegion.memory)]
		    end
	     (* memory-system operations *)
		| P.Pause => BE.AtomicOps.genPause()
		| P.FenceRead => let
		    val stms = BE.AtomicOps.genFenceRead()
		    in
		      BE.VarDef.flushLoads varDefTbl
		      @ stms
		    end
		| P.FenceWrite => let
		    val stms = BE.AtomicOps.genFenceWrite()
		    in
		      BE.VarDef.flushLoads varDefTbl
		      @ stms
		    end
		| P.FenceRW	 => let
		    val stms = BE.AtomicOps.genFenceRW()
		    in
		      BE.VarDef.flushLoads varDefTbl
		      @ stms
		    end
		| _ => raise Fail(concat[
		       "genPrim0(", PrimUtil.fmt CFG.Var.toString p, ")"
		     ])
	      (* end case *))
	  in
	    gen
	  end

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
		fun genLoad (sz, bind, ld) (base, i) = let
		      val elemSzB = sz div 8
		      val addr = arrayOffset {base=defOf base, i=defOf i, wordSzB=elemSzB}
		      in
			bind (sz, v, ld(sz, addr, ManticoreRegion.memory))
		      end
		in
		  case p
		   (* 32-bit integer primitives *)				  
		   of P.I32Add a => genArith2 (i32Ty, T.ADD, a)
		    | P.I32Sub a => genArith2 (i32Ty, T.SUB, a)
		    | P.I32Mul a => genArith2 (i32Ty, T.MULS, a)
		    | P.I32Div a => genArith2 (i32Ty, divs, a)
		    | P.I32Mod a => genArith2 (i32Ty, rems, a)
		    | P.I32LSh a => genArith2 (i32Ty, T.SLL, a)
		    | P.I32Neg a => genArith1 (i32Ty, T.NEG, a)
		   (* 64-bit integer primitives *)
		    | P.I64Add a => genArith2 (i64Ty, T.ADD, a)
		    | P.I64Sub a => genArith2 (i64Ty, T.SUB, a)
		    | P.I64Mul a => genArith2 (i64Ty, T.MULS, a)
		    | P.I64Div a => genArith2 (i64Ty, divs, a)
		    | P.I64Mod a => genArith2 (i64Ty, rems, a)
		    | P.I64LSh a => genArith2 (i64Ty, T.SLL, a)
		    | P.I64Neg a => genArith1 (i64Ty, T.NEG, a)
		    | P.U64Mul a => genArith2 (i64Ty, T.MULU, a)
		    | P.U64Div a => genArith2 (i64Ty, T.DIVU, a)
		  (* 32-bit floating-point *)
		    | P.F32Add a => genFArith2 (f32Ty, T.FADD, a)
		    | P.F32Sub a => genFArith2 (f32Ty, T.FSUB, a)
		    | P.F32Mul a => genFArith2 (f32Ty, T.FMUL, a)
		    | P.F32Div a => genFArith2 (f32Ty, T.FDIV, a)
		    | P.F32Neg a => genFArith1 (f32Ty, T.FNEG, a)
		    | P.F32Sqrt a => genFArith1 (f32Ty, T.FSQRT, a)
		    | P.F32Abs a => genFArith1 (f32Ty, T.FABS, a)
		  (* 64-bit floating-point *)
		    | P.F64Add a => genFArith2 (f64Ty, T.FADD, a)
		    | P.F64Sub a => genFArith2 (f64Ty, T.FSUB, a)
		    | P.F64Mul a => genFArith2 (f64Ty, T.FMUL, a)
		    | P.F64Div a => genFArith2 (f64Ty, T.FDIV, a)
		    | P.F64Neg a => genFArith1 (f64Ty, T.FNEG, a)
		    | P.F64Sqrt a => genFArith1 (f64Ty, T.FSQRT, a)
		    | P.F64Abs a => genFArith1 (f64Ty, T.FABS, a)
		  (* conversions *)
		    | P.I32ToI64X x => gprBind (i64Ty, v, T.SX(i64Ty, i32Ty, defOf x))
		    | P.I32ToI64 x => gprBind (i64Ty, v, T.ZX(i64Ty, i32Ty, defOf x))
		    | P.I32ToF32 x => fbind (f32Ty, v, T.CVTI2F(f32Ty, i32Ty, defOf x))
		    | P.I32ToF64 x => fbind (f64Ty, v, T.CVTI2F(f64Ty, i32Ty, defOf x))
		    | P.I64ToF32 x => fbind (f32Ty, v, T.CVTI2F(f32Ty, i64Ty, defOf x))
		    | P.I64ToF64 x => fbind (f64Ty, v, T.CVTI2F(f64Ty, i64Ty, defOf x))
		    | P.F64ToI32 x => gprBind (i32Ty, v, T.CVTF2I(i32Ty, T.Basis.TO_NEAREST, f64Ty, fdefOf x))
		  (* address arithmetic *)
		    | P.AdrAddI32(a, b) =>
			gprBind (adrTy, v, T.ADD(adrTy, defOf a, T.SX(i64Ty, i32Ty, defOf b)))
		    | P.AdrSubI32(a, b) =>
			gprBind (adrTy, v, T.ADD(adrTy, defOf a, T.SX(i64Ty, i32Ty, defOf b)))
		    | P.AdrAddI64 a => genArith2 (adrTy, T.ADD, a)
		    | P.AdrSubI64 a => genArith2 (adrTy, T.SUB, a)
		  (* address loads *)
		    | P.AdrLoadI8 addr => gprBind (i32Ty, v, T.SX(i32Ty, 8, T.LOAD(8, defOf addr, ManticoreRegion.memory)))
		    | P.AdrLoadU8 addr => gprBind (i32Ty, v, T.ZX(i32Ty, 8, T.LOAD(8, defOf addr, ManticoreRegion.memory)))
		    | P.AdrLoadI16 addr => gprBind (i32Ty, v, T.SX(i32Ty, 16, T.LOAD(16, defOf addr, ManticoreRegion.memory)))
		    | P.AdrLoadU16 addr => gprBind (i32Ty, v, T.ZX(i32Ty, 16, T.LOAD(16, defOf addr, ManticoreRegion.memory)))
		    | P.AdrLoadI32 addr => gprBind (i32Ty, v, T.LOAD(32, defOf addr, ManticoreRegion.memory))
		    | P.AdrLoadI64 addr => gprBind (i64Ty, v, T.LOAD(64, defOf addr, ManticoreRegion.memory))
		    | P.AdrLoadF32 addr => fbind (f32Ty, v, T.FLOAD(32, defOf addr, ManticoreRegion.memory))
		    | P.AdrLoadF64 addr => fbind (f64Ty, v, T.FLOAD(64, defOf addr, ManticoreRegion.memory))
		    | P.AdrLoadAdr addr => gprBind (adrTy, v, T.LOAD(adrTy, defOf addr, ManticoreRegion.memory))
		    | P.AdrLoad addr => gprBind (anyTy, v, T.LOAD(anyTy, defOf addr, ManticoreRegion.memory))
		  (* array load operations *)
		    | P.ArrLoadI32(base, i) => genLoad (i32Ty, gprBind, T.LOAD) (base, i)
		    | P.ArrLoadI64(base, i) => genLoad (i64Ty, gprBind, T.LOAD) (base, i)
		    | P.ArrLoadF32(base, i) => genLoad (f32Ty, fbind, T.FLOAD) (base, i)
		    | P.ArrLoadF64(base, i) => genLoad (f64Ty, fbind, T.FLOAD) (base, i)
		    | P.ArrLoad(base, i) => genLoad (anyTy, gprBind, T.LOAD) (base, i)
		  (* atomic operations *)
		    | P.I32FetchAndAdd(addr, x) => let
			val (r, stms) = BE.AtomicOps.genFetchAndAdd32 {
				 addr=T.LOAD(i32Ty, defOf addr, ()),
				 x=defOf x
			       }
			in
			  BE.VarDef.flushLoads varDefTbl
			  @ stms
			  @ gprBind (i32Ty, v, r)
			end
		    | P.I64FetchAndAdd(addr, x) => let
			val (r, stms) = BE.AtomicOps.genFetchAndAdd64 {
				 addr=T.LOAD(i64Ty, defOf addr, ()),
				 x=defOf x
			       }
			in
			  BE.VarDef.flushLoads varDefTbl
			  @ stms
			  @ gprBind (i64Ty, v, r)
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
		    | _ => raise Fail(concat[
			  "genPrim(", CFG.Var.toString v, ", ",
			  PrimUtil.fmt CFG.Var.toString p, ")"
			])
		end (* gen *)
	  in
	    gen
	  end (* genPrim *)

    fun genCond {varDefTbl} = let
	  val getDefOf = BE.VarDef.getDefOf varDefTbl
	  val setDefOf = BE.VarDef.setDefOf varDefTbl
	  val defOf = BE.VarDef.defOf varDefTbl
	  val fdefOf = BE.VarDef.fdefOf varDefTbl
	  val cdefOf = BE.VarDef.cdefOf varDefTbl
	  val gprBind = BE.VarDef.gprBind varDefTbl
	  val cbind = BE.VarDef.cbind varDefTbl
	  val fbind = BE.VarDef.fbind varDefTbl
	  fun gen (cond, trueLab) = let
		fun genCmp (ty, c, (v1, v2)) = 
		      [T.BCC(T.CMP (ty, c, defOf v1, defOf v2), trueLab)]
		fun genFCmp (ty, c, (v1, v2)) = 
		      [T.BCC(T.FCMP (ty, c, fdefOf v1, fdefOf v2), trueLab)]
		in
		  case cond
		   of P.isBoxed p => 
			[T.BCC(T.CMP(anyTy, T.EQ, T.ANDB(anyTy, defOf p, wordLit 1), wordLit 0), trueLab)]
		    | P.isUnboxed p => 
			[T.BCC(T.CMP(anyTy, T.NE, T.ANDB(anyTy, defOf p, wordLit 1), wordLit 0), trueLab)]
		    | P.Equal a => genCmp (anyTy, T.EQ, a)
		    | P.NotEqual a => genCmp (anyTy, T.NE, a)
		    | P.EnumEq a => genCmp (i32Ty, T.EQ, a)
		    | P.EnumNEq a => genCmp (i32Ty, T.NE, a)
		   (* 32-bit integer primitives *)				  
		    | P.I32Eq a => genCmp (i32Ty, T.EQ, a)
		    | P.I32NEq a => genCmp (i32Ty, T.NE, a)
		    | P.I32Lt a => genCmp (i32Ty, T.LT, a)
		    | P.I32Lte a => genCmp (i32Ty, T.LE, a)
		    | P.I32Gt a => genCmp (i32Ty, T.GT, a)
		    | P.I32Gte a => genCmp (i32Ty, T.GE, a)
		    | P.U32Lt a => genCmp (i32Ty, T.LTU, a)
		   (* 64-bit integer primitives *)
		    | P.I64Eq a => genCmp (i64Ty, T.EQ, a)
		    | P.I64NEq a => genCmp (i64Ty, T.NE, a)
		    | P.I64Lt a => genCmp (i64Ty, T.LT, a)
		    | P.I64Lte a => genCmp (i64Ty, T.LE, a)
		    | P.I64Gt a => genCmp (i64Ty, T.GT, a)
		    | P.I64Gte a => genCmp (i64Ty, T.GE, a)
		    | P.U64Lt a => genCmp (i64Ty, T.LTU, a)
		  (* 32-bit floating-point *)
		    | P.F32Eq a => genFCmp (f32Ty, T.==, a)
		    | P.F32NEq a => genFCmp (f32Ty, T.<>, a)
		    | P.F32Lt a => genFCmp (f32Ty, T.<, a)
		    | P.F32Lte a => genFCmp (f32Ty, T.<=, a)
		    | P.F32Gt a => genFCmp (f32Ty, T.>, a)
		    | P.F32Gte a => genFCmp (f32Ty, T.>=, a)
		  (* 64-bit floating-point *)
		    | P.F64Eq a => genFCmp (f64Ty, T.==, a)
		    | P.F64NEq a => genFCmp (f64Ty, T.<>, a)
		    | P.F64Lt a => genFCmp (f64Ty, T.<, a)
		    | P.F64Lte a => genFCmp (f64Ty, T.<=, a)
		    | P.F64Gt a => genFCmp (f64Ty, T.>, a)
		    | P.F64Gte a => genFCmp (f64Ty, T.>=, a)
		    | P.AdrEq a => genCmp (anyTy, T.EQ, a)
		    | P.AdrNEq a => genCmp (anyTy, T.NE, a)
		  (* atomic operations *)
(* FIXME
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
				    addr = T.LOAD(anyTy, defOf addr, ()),
				    cmpVal = defOf key, newVal = defOf new
				  }
			in
			  BE.VarDef.flushLoads varDefTbl
			  @ stms
			  @ cbind (v, cc)
			end
*)
		end (* gen *)
	  in
	    gen
	  end (* genCond *)

  end (* PrimGenFn *)
