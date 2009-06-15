(* expand-hlops.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure ExpandHLOps : sig

  (* replace the high-level operators in the module with their definitions; returns
   * NONE if there was no change to the module.
   *)
    val expand : BOM.module -> BOM.module option

  (* cleanup pass once HLOp expansion is completed *)
    val finish : BOM.module -> BOM.module

  end = struct

    structure B = BOM
    structure BTy = BOMTy
    structure BU = BOMUtil
    structure H = HLOp
    structure VSet = B.Var.Set

    fun expand (B.MODULE{name, externs, hlops, body}) = let
	  val changed = ref false
	  fun cvtExp (e as B.E_Pt(_, t)) = (case t
		 of B.E_Let(lhs, e1, e2) => B.mkLet(lhs, cvtExp e1, cvtExp e2)
		  | B.E_Stmt(lhs, rhs, e) => B.mkStmt(lhs, rhs, cvtExp e)
		  | B.E_Fun(fbs, e) => B.mkFun(List.map cvtLambda fbs, cvtExp e)
		  | B.E_Cont(fb, e) => B.mkCont(cvtLambda fb, cvtExp e)
		  | B.E_If(x, e1, e2) => B.mkIf(x, cvtExp e1, cvtExp e2)
		  | B.E_Case(x, cases, dflt) => B.mkCase(x,
		      List.map (fn (p, e) => (p, cvtExp e)) cases,
		      Option.map cvtExp dflt)
		  | B.E_Apply _ => e
		  | B.E_Throw _ => e
		  | B.E_Ret _ => e
		  | B.E_HLOp(hlOp, args, rets) => let
	              val (inline, defn as B.FB{f, ...}, cfuns) = (
			    case HLOpEnv.findDef hlOp
			      of SOME{inline, def, externs, ...} => 
				 (* found the definition in inline BOM *)
				   (inline, def, externs)
			       | NONE => raise Fail("unable to find " ^ HLOp.toString hlOp)
			    (* end case *))
		      in
			changed := true;
			if inline
			  then (
			    Census.initLambda defn;
			    BU.applyLambda(defn, args, rets))
			  else (
			    Census.incAppCnt f;
			    B.mkApply(f, args, rets))
		      end
		(* end case *))
	  and cvtLambda (B.FB{f, params, exh, body}) =
                  B.FB{f=f, params=params, exh=exh, body=cvtExp body}
	  val body = cvtLambda body
	  in
	    if !changed
	      then SOME(B.mkModule(name, externs, hlops, body))
	      else NONE
	  end

    fun finish (B.MODULE{name, externs, hlops, body}) = let
	(* decrement the use count of an HLOp by one and clear the HLOp property,
	 * since we are done with them now
	 *)
	  fun doHLOp f = (
		B.Var.clrHLOp f;
		Census.decUseCnt f)
	  in
	    B.Var.Set.app doHLOp hlops;
	    B.MODULE{
		name = name, externs = externs, hlops = B.Var.Set.empty,
		body = body}
	  end

  end
