(* subst-var.sml
 * 
 * COPYRIGHT (c) 2006 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure SubstVar =
  struct

    structure A = AST
    structure VarMap = Var.Map

    type subst = Var.var VarMap.map


    val id : subst = VarMap.empty

    fun add ((k, v), s) = VarMap.insert (s, k, v)
    val find = VarMap.find

    fun exp s e = let
	  fun exp (A.LetExp (b, e)) = A.LetExp (binding s b, exp e)
	    | exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
	    | exp (A.CaseExp (e, ms, t)) = A.CaseExp (exp e, List.map (match s) ms, t)
	    | exp (A.PCaseExp (es, pms, t)) = A.PCaseExp (List.map exp es, List.map (pmatch s) pms, t)
	    | exp (A.HandleExp (e, ms, t)) = A.HandleExp (exp e, List.map (match s) ms, t)
	    | exp (A.RaiseExp (e, t)) = A.RaiseExp (exp e, t)
	    | exp (A.FunExp (x, e, t)) = A.FunExp (x, exp e, t)
	    | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
	    | exp (m as A.VarArityOpExp _) = m
	    | exp (A.TupleExp es) = A.TupleExp (List.map exp es)
	    | exp (A.RangeExp (e1, e2, oe3, t)) =
	      A.RangeExp (exp e1, exp e2, Option.map exp oe3, t)
	    | exp (A.PTupleExp es) = A.PTupleExp (List.map exp es)
	    | exp (A.PArrayExp (es, t)) = A.PArrayExp (List.map exp es, t)
	    | exp (A.PCompExp (e, pes, opred)) = 
	      A.PCompExp (exp e, 
			  List.map (fn (p,e) => (pat s p, exp e)) pes,
			  Option.map exp opred)
	    | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (List.map exp es, t)
	    | exp (A.SpawnExp e) = A.SpawnExp (exp e) 
	    | exp (k as A.ConstExp _) = k
	    | exp (A.VarExp (x, ts)) = A.VarExp (s x, ts)
	    | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
	    | exp (ov as A.OverloadExp _) = ov
	    | exp (A.ExpansionOptsExp (opts, e)) = A.ExpansionOptsExp (opts, exp e)
	    | exp (A.FTupleExp es) = A.FTupleExp (List.map exp es)
	    | exp (A.FArrayExp (es, n, t)) = A.FArrayExp (List.map exp es, ntree s n, t)
          in
	     exp e
          end

    and ntree s n = (case n
          of A.Lf (e1, e2) => A.Lf (exp s e1, exp s e2)
	   | A.Nd ns => A.Nd (List.map (ntree s) ns)
          (* end case *))

    and pat s p = (case p
          of A.ConPat(dcon, tys, p) => A.ConPat(dcon, tys, pat s p)
	   | A.TuplePat ps => A.TuplePat (List.map (pat s) ps)
	   | A.VarPat v => A.VarPat (s v)
	   | A.WildPat t => A.WildPat t
	   | A.ConstPat c => A.ConstPat c
          (* end case *))

    and const s (A.DConst (dc, tys)) = A.DConst(dc, tys)
      | const s (A.LConst (l, ty)) = A.LConst (l, ty)

    and match s (A.PatMatch (p, e)) = A.PatMatch (pat s p, exp s e)
      | match s (A.CondMatch (p, cond, e)) = A.CondMatch (pat s p, exp s cond, exp s e)

    and binding s (A.ValBind (p, e)) = A.ValBind (pat s p, exp s e)
      | binding s (A.PValBind (p, e)) = A.PValBind (pat s p, exp s e)
      | binding s (A.FunBind ls) = A.FunBind(List.map (lambda s) ls)
      | binding s (A.PrimVBind (v, rhs)) = A.PrimVBind (v, rhs)
      | binding s (A.PrimCodeBind code) = A.PrimCodeBind code

    and lambda s (A.FB (f, x, e)) = A.FB(s f, s x, exp s e)
		  
    and pmatch s (A.PMatch (ps, e)) = A.PMatch (List.map (ppat s) ps, exp s e)
      | pmatch s (A.Otherwise (ts, e)) = A.Otherwise (ts, exp s e)

    and ppat s (w as A.NDWildPat _) = w
      | ppat s (A.HandlePat (p, t)) = A.HandlePat (pat s p, t)
      | ppat s (A.Pat p) = A.Pat (pat s p)

    and module s (A.M_Id (info, mr)) = A.M_Id (info, mr)
      | module s (A.M_Body (info, tds)) = A.M_Body(info, List.map (topDec s) tds)

    and topDec s (A.TD_Module (info, mr, mt, md)) = A.TD_Module (info, mr, mt, module s md)
      | topDec s (A.TD_DCon dc) = A.TD_DCon dc
      | topDec s (A.TD_Binding b) = A.TD_Binding (binding s b)

    and topDecs s tds = List.map (topDec s) tds

  end (* SubstVar *)
