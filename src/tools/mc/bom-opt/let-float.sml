(* let-float.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure LetFloat : sig

  (* flatten out unnecessary block-nesting; if the second argument is true,
   * then we apply this transformation to the whole tree; otherwise we
   * restrict it to the top.
   *)
    val denest : (BOM.exp * bool) -> BOM.exp
    val denestLambda : (BOM.lambda * bool) -> BOM.lambda

  end = struct

    structure B = BOM

  (* flatten out unnecessary block-nesting; if deep is true, then we
   * apply this transformation to the whole expression, otherwise it
   * is only applied to the topmost block.
   *)
    fun denest (e, deep) = let
	  fun setBinding rhs (VarRep.V{kind, ...}) = (kind := B.VK_Let rhs)
	  fun doE (e as B.E_Pt(pt, t), k) = (case (deep, t)
		 of (_, B.E_Let(xs, e', e)) => let
		      fun k' e'' = (
			    List.app (setBinding e'') xs;
			    B.E_Pt(pt, B.E_Let(xs, e'', doE(e, k))))
		      in
			doE (e', k')
		      end
		  | (_, B.E_Stmt(xs, rhs, e)) =>
		      B.E_Pt(pt, B.E_Stmt(xs, rhs, doE(e, k)))
		  | (true, B.E_Fun(fbs, e)) =>
		      B.mkFun(List.map doFB fbs, doE(e, k))
		  | (false, B.E_Fun(fbs, e)) =>
		      B.mkFun(fbs, doE(e, k))
		  | (true, B.E_Cont(fb, e)) =>
		      k (B.mkCont(doFB fb, doE(e, fn e => e)))
		  | (false, B.E_Cont(fb, e)) =>
		      k (B.mkCont(fb, doE(e, fn e => e)))
		  | (true, B.E_If(x, e1, e2)) =>
		      k (B.E_Pt(pt, B.E_If(x, denest' e1, denest' e2)))
		  | (true, B.E_Case(x, cases, NONE)) =>
		      k (B.E_Pt(pt, B.E_Case(x, List.map doCase cases, NONE)))
		  | (true, B.E_Case(x, cases, SOME e)) =>
		      k (B.E_Pt(pt,
			B.E_Case(x, List.map doCase cases, SOME(denest' e))))
		  | _ => k e
		(* end case *))
	  and doFB (B.FB{f, params, exh, body}) =
		B.FB{f=f, params=params, exh=exh, body=denest' body}
	  and doCase (p, e) = (p, denest' e)
	  and denest' e = doE (e, fn e' => e')
	  in
	    denest' e
	  end

    fun denestLambda (B.FB{f, params, exh, body}, deep) =
	  B.FB{f=f, params=params, exh=exh, body=denest(body, deep)}

  end
