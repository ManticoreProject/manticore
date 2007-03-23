(* prim-gen-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

signature PRIM_GEN = sig

    structure BE : BACK_END

    type ctx = {varDefTbl : BE.VarDef.var_def_tbl}
	       
    val genPrim : ctx -> {gen : (CFG.var * CFG.prim) -> unit}
			
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

    type ctx = {varDefTbl : BE.VarDef.var_def_tbl}

    val i32ty = 32
    val i64ty = 64
    val f32ty = 32
    val f64ty = 64

    fun wordLit i = T.LI (T.I.fromInt (i64ty, i))

    fun genPrim {varDefTbl} = let
	  val getDefOf = BE.VarDef.getDefOf varDefTbl
	  val setDefOf = BE.VarDef.setDefOf varDefTbl
	  val defOf = BE.VarDef.defOf varDefTbl
	  val fdefOf = BE.VarDef.fdefOf varDefTbl
	  val cdefOf = BE.VarDef.cdefOf varDefTbl
	  val bind = BE.VarDef.bind varDefTbl
	  val cbind = BE.VarDef.cbind varDefTbl
	  val fbind = BE.VarDef.fbind varDefTbl

	  fun gen (v, p) = let
		fun genCmp (ty, c, (v1, v2)) = 
		      cbind (v, T.CMP (ty, c, defOf v1, defOf v2))
		fun genArith (ty, oper, (v1, v2)) = 
		      bind (ty, v, oper (ty, defOf v1, defOf v2))
		fun genFArith (fty, oper, (v1, v2)) =
		      fbind (fty, v, oper (fty, fdefOf v1, fdefOf v2))
		in
		  case p
		   (* 64-bit integer primitives *)
		    of P.I64Add a => genArith (i64ty, T.ADD, a)
		     | P.I64Sub a => genArith (i64ty, T.SUB, a)
		     | P.I64Lte a => genCmp (i64ty, T.LE, a)
		     | P.I64Eq a => genCmp (i64ty, T.EQ, a)
		     (* 32-bit integer primitives *)				  
		     | P.I32Add a => genArith (i32ty, T.ADD, a)
		     | P.I32Sub a => genArith (i32ty, T.SUB, a)
		     | P.I32Mul a => genArith (i32ty, T.MULS, a)
		     | P.I32Gte a => genCmp (i32ty, T.GE, a)
		     | P.I32Lte a => genCmp (i32ty, T.LE, a)
		     | P.I32Eq a => genCmp (i32ty, T.EQ, a)
		    (* 32-bit floating-point *)
		     | P.F32Add a => genFArith (f32ty, T.FADD, a)
		     | P.F32Sub a => genFArith (f32ty, T.FSUB, a)
		     (* test whether a value is boxed by testing
		      * its bottom bit. *)
		     | P.isBoxed v => 
		       cbind (v, T.CMP (i64ty, T.EQ, 
			T.ANDB (i64ty, defOf v, wordLit 1), wordLit 0))
		     | P.isUnboxed v => 
		       cbind (v, T.CMP (i64ty, T.EQ, 
			T.ANDB (i64ty, defOf v, wordLit 1), wordLit 1))

		     | _ => raise Fail(concat[
			  "genPrim(", CFG.Var.toString v, ", ",
			  PrimUtil.fmt CFG.Var.toString p, ")"
			])
		  (* esac *)
		end (* gen *)
	  in
	    {gen=gen}
	  end (* genPrim *)


  end (* PrimGenFn *)
