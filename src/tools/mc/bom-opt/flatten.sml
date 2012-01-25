(* inline.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Flattening transformation in B.
 *
 *)

structure Flatten : sig

  type env = BOMUtil.subst

  val transform : BOM.module -> BOM.module
  val lambda_trans : env -> BOM.lambda -> BOM.lambda
  val exp_trans : env -> BOM.exp -> BOM.exp
  val rhs_trans : env -> BOM.rhs -> BOM.rhs
  val var_trans : env -> BOM.var -> BOM.var

end = struct

  structure B = BOM
  structure U = BOMUtil

  fun var_trans (env : env) (v as VarRep.V{name, id, kind, useCnt, ty, props}) = v

  fun rhs_trans (env : env) (r) = r

  fun exp_trans (env : env) (exp as B.E_Pt {ppt, term}) =
	let val newterm =
		case term
		  of B.E_Let{vlist, e1, e2} =>
			B.E_Let{map (var_trans env) vlist,
			exp_trans env e1,
			exp_trans env e2}
		   | B.E_Stmt{vlist, rhs, e} =>
			B.E_Stmt{map (var_trans env) vlist,
			rhs_trans env rhs,
			exp_trans env e}
		   | B.E_Fun{lamlist, e} =>
			B.E_Fun{map (lambda_trans env) lamlist,
			exp_trans env e}
		   | B.E_Cont{lam, e} =>
			B.E_Cont{lambda_trans env lam,
			exp_trans env e}
		   | B.E_If{cond, e1, e2} =>
			B.E_If{cond,
			exp_trans env e1,
			exp_trans env e2}
		   | B.E_Case{v, pelist, eopt} =>
			B.E_Case{v,
			pelist,
			(case eopt of SOME(e) => SOME(exp_trans env e) | NONE => NONE)}
		   | B.E_Apply{v, vlist1, vlist2} =>
			B.E_Apply{var_trans env v,
			map (var_trans env) vlist1,
			map (var_trans env) vlist2}
		   | B.E_Throw{v, vlist} =>
			B.E_Throw{var_trans env v,
			map (var_trans env) vlist}
		   | B.E_Ret{vlist} =>
			B.E_Ret{map (var_trans env) vlist}
		   | B.E_HLOp{op, vlist1, vlist2} =>
			B.E_HLOp{op,
			map (var_trans env) vlist1,
			map (var_trans env) vlist2}
	in B.E_Pt{ppt, newterm} end
	

  fun lambda_trans (env : env) (lam as B.FB {f, params, exh, body}) =
	B.FB {f=f,
		params=params,
		exh=exh,
		body=(exp_trans env body)}

  fun module (B.MODULE {name, externs, hlops, rewrites, body}) =
	B.MODULE {name=name,
		externs=externs,
		hlops=hlops,
		rewrites=rewrites,
		body=(lambda_trans U.empty body)}

  fun transform m = 
	if not(!BOMOptControls.flattenFlg) then m 
	else let
	  val _ = TextIO.print "The compiler *would* be flattening now.\n"
	  in
	    module m
	  end

end
