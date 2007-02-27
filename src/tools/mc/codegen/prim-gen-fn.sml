signature PRIM_GEN = sig

    structure BE : BACK_END

    type ctx = {varDefTbl : BE.VarDef.var_def_tbl}
	       
    val genPrim : ctx -> {gen : (CFG.var * CFG.prim) -> unit}
			
end (* PRIM_GEN *)

functor PrimGenFn (
	structure BE : BACK_END
) : PRIM_GEN = struct

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

  fun genPrim {varDefTbl} = 
      let val getDefOf = BE.VarDef.getDefOf varDefTbl
	  val setDefOf = BE.VarDef.setDefOf varDefTbl
	  val defOf = BE.VarDef.defOf varDefTbl
	  val fdefOf = BE.VarDef.fdefOf varDefTbl
	  val cdefOf = BE.VarDef.cdefOf varDefTbl
	  val bind = BE.VarDef.bind varDefTbl
	  val cbind = BE.VarDef.cbind varDefTbl
	  val fbind = BE.VarDef.fbind varDefTbl

	  fun gen (v, p) = 
	      let fun genCmp (ty, c, (v1, v2)) = 
		      cbind (v, T.CMP (ty, c, defOf v1, defOf v2))
		  fun genArith (ty, oper, (v1, v2)) = 
		      bind (ty, v, oper (ty, defOf v1, defOf v2))
		  fun genFArith (fty, oper, (v1, v2)) =
		      fbind (fty, v, oper (fty, fdefOf v1, fdefOf v2))
	      in
		  (case p
		    of (* 64-bit integer primitives *)
		       P.I64Add a => genArith (i64ty, T.ADD, a)
		     | P.I64Sub a => genArith (i64ty, T.SUB, a)
		     | P.I64Lte a => genCmp (i64ty, T.LE, a)
		     | P.I64Eq a => genCmp (i64ty, T.EQ, a)
		     (* 32-bit integer primitives *)				  
		     | P.I32Add a => genArith (i32ty, T.ADD, a)
		     | P.I32Sub a => genArith (i32ty, T.SUB, a)
		     | P.I32Lte a => genCmp (i32ty, T.LE, a)
		     | P.I32Eq a => genCmp (i32ty, T.EQ, a)
					  
		     | P.F32Add a => genFArith (f32ty, T.FADD, a)
		     | P.F32Sub a => genFArith (f32ty, T.FSUB, a)

		     | _ => raise Fail "genPrim"
		  (* esac *))
	      end (* gen *)
      in
	  {gen=gen}
      end (* genPrim *)


end (* PrimGenFn *)
