(* var-subst.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure FTVarSubst = 
  struct
  
    structure F = FLAST
    structure M = FTVar.Map

    structure FTy = FTTypes
    
    type subst = FTVar.var M.map

  (* id : subst *)
    val id: subst = M.empty

  (* add : (F.var * F.var) * subst -> subst *)
  (* Suitable for folding (List.foldx). *)
    fun add ((k: F.var, v: F.var), s: subst) : subst = 
      M.insert (s, k, v)

  (* add': F.var * F.var * F.subst -> F.subst *)
  (* Suitable for "pair" folding (ListPair.foldx). *)
    fun add' (k: F.var, v: F.var, s: subst) : subst =
      M.insert (s, k, v)

  (* idSubst : F.var -> subst *)
    fun idSubst (v: F.var) : subst = add((v, v), id)

  (* Construct the singleton substitution [x -> y] *)
  (*   i.e. "replace x with y" *)
    fun singleton (x: F.var, y: F.var) : subst = add ((x, y), id)

  (* substVar : subst -> F.var -> F.var *)
  (* Substitute a var for a var. Suitable for use in e.g. patterns. *)
    fun substVar (s: subst) (x: F.var) : F.var = 
      (case M.find (s, x)
	 of NONE => x
	  | SOME y => y
        (* end case *))

  (* expWalk : (F.var * F.ty list -> F.exp) -> subst -> F.exp 
	       -> {exp: F.exp -> F.exp, binding: F.binding -> F.binding}
   * Given a function that builds an exp from the args of a VarExp 
   *   and a substitution, construct two substitution functions, 
   *   one for exps and one for bindings. 
   *)
    fun expWalk (f: F.var * F.ty list -> F.exp) (s: subst)
	: {exp: F.exp -> F.exp, binding: F.binding -> F.binding} = let
      fun exp (F.LetExp (b, e)) = F.LetExp (binding b, exp e)
	| exp (F.IfExp (e1, e2, e3, t)) = F.IfExp (exp e1, exp e2, exp e3, t)
	| exp (F.CaseExp (e, ms, t)) = F.CaseExp (exp e, map match ms, t)
	| exp (F.PCaseExp (es, pms, t)) = F.PCaseExp (map exp es, map pmatch pms, t)
	| exp (F.HandleExp (e, ms, t)) = F.HandleExp (exp e, map match ms, t)
	| exp (F.RaiseExp (e, t)) = F.RaiseExp (exp e, t)
	| exp (F.FunExp (x, e, t)) = F.FunExp (x, exp e, t)
	| exp (F.ApplyExp (e1, e2, t)) = F.ApplyExp (exp e1, exp e2, t)
	| exp (m as F.VarArityOpExp _) = m
	| exp (F.TupleExp (es, t)) = F.TupleExp (map exp es, t)
	| exp (F.RangeExp (e1, e2, oe3, t)) = F.RangeExp (exp e1, exp e2, Option.map exp oe3, t)
	| exp (F.PTupleExp es) = F.PTupleExp (map exp es)
	| exp (F.PArrayExp (es, t)) = F.PArrayExp (map exp es, t)
	| exp (F.FArrayExp (es, n, t)) = F.FArrayExp (map exp es, ntree n, t)
	| exp (F.PCompExp (e, pes, opred)) = let
	    val pes' = map (fn (p,e) => (pat p, exp e)) pes
	    val opred' = Option.map exp opred
            in
              F.PCompExp (exp e, pes', opred')
	    end
	| exp (F.PChoiceExp (es, t)) = F.PChoiceExp (map exp es, t)
	| exp (F.SpawnExp e) = F.SpawnExp (exp e) 
	| exp (k as F.ConstExp _) = k
	| exp (v as F.VarExp (x, ts)) = f (x, ts)
	| exp (F.SeqExp (e1, e2)) = F.SeqExp (exp e1, exp e2)
	| exp (ov as F.OverloadExp r) = (* FIXME is this right? *)
            (case !r
	       of F.Unknown (ty, xs) => let
                    val xs' = List.map (substVar s) xs
                    in
		      F.OverloadExp (ref (F.Unknown (ty, xs')))
		    end
		| F.Instance x => let
                    val x' = substVar s x
                    in
                      F.OverloadExp (ref (F.Instance x'))
		    end
	      (* end case *))
	| exp (F.ExpansionOptsExp (os, e)) = F.ExpansionOptsExp (os, exp e)
      and ntree (F.Lf (e1, e2)) = F.Lf (exp e1, exp e2)
	| ntree (F.Nd es) = F.Nd (map ntree es)
      and match (F.PatMatch (p, e)) = F.PatMatch (pat p, exp e)
	| match (F.CondMatch (p, cond, e)) = F.CondMatch (pat p, exp cond, exp e)
      and pmatch (F.PMatch (pps, e)) = F.PMatch (map ppat pps, exp e)
	| pmatch (F.Otherwise (ts, e)) = F.Otherwise (ts, exp e)
      and ppat (F.NDWildPat t) = F.NDWildPat t
	| ppat (F.HandlePat (p, t)) = F.HandlePat (pat p, t)
	| ppat (F.Pat p) = F.Pat (pat p)
      and pat (F.ConPat (c, ts, p)) = F.ConPat (c, ts, pat p)
	| pat (F.TuplePat ps) = F.TuplePat (List.map pat ps)
	| pat (v as F.VarPat x) = F.VarPat (substVar s x)		    
	| pat (F.WildPat t) = F.WildPat t
	| pat (k as F.ConstPat _) = k
      and binding (F.ValBind (p, e)) = F.ValBind (pat p, exp e)
	| binding (F.PValBind (p, e)) = F.PValBind (pat p, exp e)
	| binding (F.FunBind ls) = F.FunBind (List.map lambda ls)
	| binding (primV as F.PrimVBind _) = primV
	| binding (code as F.PrimCodeBind _) = code 
      and lambda (F.FB (f, x, e)) = F.FB (substVar s f, x, exp e)
      in
        {exp=exp, binding=binding}
      end

  (* applySubst : subst -> F.exp -> F.exp *)
  (* Substitute variables for variables. *)
    fun applySubst (s: subst) : F.exp -> F.exp = let
      fun f (x, ts) =
        (case M.find (s, x)
	   of NONE   => F.VarExp (x, ts)
	    | SOME y => F.VarExp (y, ts)
	  (* end case *))
      val {exp, ...} = expWalk f s
      in
        exp
      end

  (* One-shot variable-variable substitution. *)
  (* substitute "this" for "that" in "e" *)
    fun subst1 (this: F.var, that: F.var, e: F.exp) : F.exp = 
      applySubst (singleton (this, that)) e

  (* perform the substitution e' [x -> e] *)
  (* Substitute the same expression for all variables in the subst. *)
  (* FIXME An odd function. Is this used anywhere? *)
    fun substForExp s e = let
      fun f (x, ts) = 
        (case M.find (s, x)
	   of SOME _ => e
	    | NONE   => F.VarExp (x, ts)
	  (* end case *))
      val {exp, ...} = expWalk f s
      in
        exp
      end

  end
