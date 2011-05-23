(* subst-ty.sml
 * 
 * COPYRIGHT (c) 2006 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure SubstTy =
  struct

    structure A = AST

    fun substTy ({tyFn, tySFn, dcFn}, ty) = tyFn ty
    fun substTyS ({tyFn, tySFn, dcFn}, tyS) = tySFn tyS
    fun substDCon ({tyFn, tySFn, dcFn}, dc) = dcFn dc
    fun substForVar (s, v) = (
	  Var.setType(v, ref (substTyS (s, Var.typeOf v))); 
	  v)

    fun exp s e = let
	  fun exp (A.LetExp (b, e)) = A.LetExp (binding s b, exp e)
	    | exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
	    | exp (A.CaseExp (e, ms, t)) = A.CaseExp (exp e, List.map (match s) ms, substTy(s, t))
	    | exp (A.PCaseExp (es, pms, t)) = A.PCaseExp (List.map exp es, List.map (pmatch s) pms, substTy(s, t))
	    | exp (A.HandleExp (e, ms, t)) = A.HandleExp (exp e, List.map (match s) ms, substTy(s, t))
	    | exp (A.RaiseExp (e, t)) = A.RaiseExp (exp e, substTy(s, t))
	    | exp (A.FunExp (x, e, t)) = A.FunExp (substForVar(s, x), exp e, substTy(s, t))
	    | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, substTy(s, t))
	    | exp (m as A.VarArityOpExp _) = m
	    | exp (A.TupleExp es) = A.TupleExp (List.map exp es)
	    | exp (A.RangeExp (e1, e2, oe3, t)) =
	      A.RangeExp (exp e1, exp e2, Option.map exp oe3, substTy(s, t))
	    | exp (A.PTupleExp es) = A.PTupleExp (List.map exp es)
	    | exp (A.PArrayExp (es, t)) = A.PArrayExp (List.map exp es, substTy(s, t))
	    | exp (A.PCompExp (e, pes, opred)) = 
	      A.PCompExp (exp e, 
			  List.map (fn (p,e) => (pat s p, exp e)) pes,
			  Option.map exp opred)
	    | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (List.map exp es, substTy(s, t))
	    | exp (A.SpawnExp e) = A.SpawnExp (exp e) 
	    | exp (k as A.ConstExp c) = A.ConstExp (const s c)
	    | exp (A.VarExp (x, ts)) = A.VarExp (substForVar(s, x), ts)
	    | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
	    | exp (ov as A.OverloadExp _) = ov
	    | exp (A.ExpansionOptsExp (opts, e)) = A.ExpansionOptsExp (opts, exp e)
	    | exp (A.FTupleExp es) = A.FTupleExp (List.map exp es)
	    | exp (A.FArrayExp (es, n, t)) = A.FArrayExp (List.map exp es, ntree s n, substTy (s, t))
	    | exp (A.FlOp oper) = A.FlOp (fl_op s oper)
	    | exp (A.PArrayOp oper) = A.PArrayOp (pop s oper)
          in
	     exp e
          end

    and ntree s n = (case n
          of A.Lf (e1, e2) => A.Lf (exp s e1, exp s e2)
	   | A.Nd ns => A.Nd (List.map (ntree s) ns)
          (* end case *))

    and pat s p = (case p
          of A.TuplePat ps => A.TuplePat (List.map (pat s) ps)
	   | A.VarPat v => A.VarPat (substForVar(s, v))
	   | A.WildPat t => A.WildPat (substTy(s, t))
	   | A.ConstPat c => A.ConstPat (const s c)
	   | A.ConPat (dc, tys, p) => 
               A.ConPat (substDCon (s, dc), 
			 List.map (fn ty => substTy(s, ty)) tys, 
			 pat s p)
          (* end case *))

    and const s (A.DConst (dc, tys)) = A.DConst(substDCon(s, dc), List.map (fn ty => substTy(s, ty)) tys)
      | const s (A.LConst (l, ty)) = A.LConst (l, substTy(s, ty))

    and match s (A.PatMatch (p, e)) = A.PatMatch (pat s p, exp s e)
      | match s (A.CondMatch (p, cond, e)) = A.CondMatch (pat s p, exp s cond, exp s e)

    and binding s (A.ValBind (p, e)) = A.ValBind (pat s p, exp s e)
      | binding s (A.PValBind (p, e)) = A.PValBind (pat s p, exp s e)
      | binding s (A.FunBind ls) = A.FunBind(List.map (lambda s) ls)
      | binding s (A.PrimVBind (v, prim)) = A.PrimVBind (substForVar(s, v), prim)
      | binding s (A.PrimCodeBind code) = A.PrimCodeBind code

    and lambda s (A.FB (f, x, e)) = A.FB(substForVar(s, f), substForVar(s, x), exp s e)
		  
    and pmatch s (A.PMatch (ps, e)) = A.PMatch (List.map (ppat s) ps, exp s e)
      | pmatch s (A.Otherwise (ts, e)) =
          A.Otherwise (List.map (fn t => substTy (s, t)) ts, exp s e)

    and ppat s (w as A.NDWildPat _) = w
      | ppat s (A.HandlePat (p, t)) = A.HandlePat (pat s p, substTy(s, t))
      | ppat s (A.Pat p) = A.Pat (pat s p)

    and module s (A.M_Id (info, mr)) = A.M_Id (info, mr)
      | module s (A.M_Body (info, tds)) = A.M_Body(info, List.map (topDec s) tds)

    and topDec s (A.TD_Module (info, mr, mt, md)) = A.TD_Module (info, mr, mt, module s md)
      | topDec s (A.TD_DCon dc) = A.TD_DCon dc
      | topDec s (A.TD_Binding b) = A.TD_Binding (binding s b)

    and topDecs s tds = List.map (topDec s) tds

    and fl_op s oper = (case oper
      of A.ID t => A.ID (substTy (s, t))
       | A.Unzip t => A.Unzip (substTy (s, t))
       | A.Cat t => A.Cat (substTy (s, t))
       | A.Map (o1, n) => A.Map (fl_op s o1, n)
       | A.Compose (o1, o2) => A.Compose (fl_op s o1, fl_op s o2)
       | A.CrossCompose os => A.CrossCompose (List.map (fl_op s) os)
      (* end case *))

    and pop s oper = let
      fun `f x y = f (x, y)
      fun ps (A.PSub_Nested t) = A.PSub_Nested (substTy (s, t))
	| ps (A.PSub_Flat t) = A.PSub_Flat (substTy (s, t))
	| ps (A.PSub_Tuple ss) = A.PSub_Tuple (List.map ps ss)
      fun p (A.PA_Length t) = A.PA_Length (substTy (s, t))
	| p (A.PA_Sub s) = A.PA_Sub (ps s)
	| p (A.PA_Tab t) = A.PA_Tab (substTy (s, t))
	| p (A.PA_TabFTS t) = A.PA_TabFTS (substTy (s, t))
	| p (A.PA_TabTupleFTS ts) = A.PA_TabTupleFTS (List.map (`substTy s) ts)
	| p (A.PA_Map t) = A.PA_Map (substTy (s, t))
	| p (A.PA_Reduce t) = A.PA_Reduce (substTy (s, t))
	| p (A.PA_Range t) = A.PA_Range (substTy (s, t))
	| p (A.PA_App t) = A.PA_App (substTy (s, t))
	| p (A.PA_Tab2D t) = A.PA_Tab2D (substTy (s, t))
      in
        p oper
      end

  end (* SubstTy *)
