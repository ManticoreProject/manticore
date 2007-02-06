signature PRIM_GEN = sig

    structure BE : BACK_END

    type ctx = {varDefTbl : BE.VarDef.var_def_tbl}
	       
    val genPrim : ctx -> {gen : CFG.prim -> BE.MTy.mlrisc_tree}
			
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
  fun i64Exp e = MTy.EXP (i64ty, e)
  fun cExp e = MTy.CEXP e

  fun genPrim {varDefTbl} = 
      let val getDefOf = BE.VarDef.getDefOf varDefTbl
	  val setDefOf = BE.VarDef.setDefOf varDefTbl
	  val defOf = BE.VarDef.defOf varDefTbl
	  val fdefOf = BE.VarDef.fdefOf varDefTbl
	  val cdefOf = BE.VarDef.cdefOf varDefTbl

	  fun genCmp64 (ty, c, v1, v2) = cExp (T.CMP (ty, c, defOf v1, defOf v2))
	  fun genArith64 (ty, oper, v1, v2) = i64Exp (oper (ty, defOf v1, defOf v2))

	  fun gen p = 
	      (case p
		of P.I64Add (v1, v2) => genArith64 (i64ty, T.ADD, v1, v2)
		 | P.I64Sub (v1, v2) => genArith64 (i64ty, T.SUB, v1, v2)
		 | P.I64Lte (v1, v2) => genCmp64 (i64ty, T.LT, v1, v2)
		 | P.I64Eq (v1, v2) => genCmp64 (i64ty, T.EQ, v1, v2)
		 | _ => raise Fail ""
	      (* esac *))
      in
	  {gen=gen}
      end (* genPrim *)


end (* PrimGenFn *)
