(* var-subst.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure VarSubst = 
  struct
  
    structure A = AST
    structure T = Types
    structure M = Var.Map
    
    type subst = Var.var M.map

  (* id : subst *)
    val id: subst = M.empty

  (* add : (A.var * A.var) * A.subst -> A.subst *)
  (* Suitable for folding (List.foldx). *)
    fun add ((k: A.var, v: A.var), s: subst) : subst = 
      M.insert (s, k, v)

  (* add': A.var * A.var * A.subst -> A.subst *)
  (* Suitable for "pair" folding (ListPair.foldx). *)
    fun add' (k: A.var, v: A.var, s: subst) : subst =
      M.insert (s, k, v)

  (* idSubst : A.var -> subst *)
    fun idSubst (v: A.var) : subst = add((v, v), id)

  (* Construct the singleton substitution [x -> y] *)
  (*   i.e. "replace x with y" *)
    fun singleton (x: A.var, y: A.var) : subst = add ((x, y), id)

  (* substVar : subst -> A.var -> A.var *)
  (* Substitute a var for a var. Suitable for use in e.g. patterns. *)
    fun substVar (s: subst) (x: A.var) : A.var = 
      (case M.find (s, x)
	 of NONE => x
	  | SOME y => y
        (* end case *))

  (* expWalk : (A.var * A.ty list -> A.exp) -> subst -> A.exp 
	       -> {exp: A.exp -> A.exp, binding: A.binding -> A.binding}
   * Given a function that builds an exp from the args of a VarExp 
   *   and a substitution, construct two substitution functions, 
   *   one for exps and one for bindings. 
   *)
    fun expWalk (f: A.var * (A.ty list) -> A.exp) (s: subst)
	: {exp: A.exp -> A.exp, binding: A.binding -> A.binding} = let
      fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
	| exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
	| exp (A.CaseExp (e, ms, t)) = A.CaseExp (exp e, map match ms, t)
	| exp (A.PCaseExp (es, pms, t)) = A.PCaseExp (map exp es, map pmatch pms, t)
	| exp (A.HandleExp (e, ms, t)) = A.HandleExp (exp e, map match ms, t)
	| exp (A.RaiseExp (e, t)) = A.RaiseExp (exp e, t)
	| exp (A.FunExp (x, e, t)) = A.FunExp (x, exp e, t)
	| exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
	| exp (m as A.VarArityOpExp _) = m
	| exp (A.TupleExp es) = A.TupleExp (map exp es)
	| exp (A.RangeExp (e1, e2, oe3, t)) = A.RangeExp (exp e1, exp e2, Option.map exp oe3, t)
	| exp (A.PTupleExp es) = A.PTupleExp (map exp es)
	| exp (A.PArrayExp (es, t)) = A.PArrayExp (map exp es, t)
	| exp (A.PCompExp (e, pes, opred)) = let
	    val pes' = map (fn (p,e) => (pat p, exp e)) pes
	    val opred' = Option.map exp opred
            in
              A.PCompExp (exp e, pes', opred')
	    end
	| exp (A.PChoiceExp (es, t)) = A.PChoiceExp (map exp es, t)
	| exp (A.SpawnExp e) = A.SpawnExp (exp e) 
	| exp (k as A.ConstExp _) = k
	| exp (v as A.VarExp (x, ts)) = f (x, ts)
	| exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
	| exp (ov as A.OverloadExp r) = (* FIXME is this right? *)
            (case !r
	       of A.Unknown (ty, xs) => let
                    val xs' = List.map (substVar s) xs
                    in
		      A.OverloadExp (ref (A.Unknown (ty, xs')))
		    end
		| A.Instance x => let
                    val x' = substVar s x
                    in
                      A.OverloadExp (ref (A.Instance x'))
		    end
	      (* end case *))
	| exp (A.ExpansionOptsExp (os, e)) = A.ExpansionOptsExp (os, exp e)
      and match (A.PatMatch (p, e)) = A.PatMatch (pat p, exp e)
	| match (A.CondMatch (p, cond, e)) = A.CondMatch (pat p, exp cond, exp e)
      and pmatch (A.PMatch (pps, e)) = A.PMatch (map ppat pps, exp e)
	| pmatch (A.Otherwise e) = A.Otherwise (exp e)
      and ppat (A.NDWildPat t) = A.NDWildPat t
	| ppat (A.HandlePat (p, t)) = A.HandlePat (pat p, t)
	| ppat (A.Pat p) = A.Pat (pat p)
      and pat (A.ConPat (c, ts, p)) = A.ConPat (c, ts, pat p)
	| pat (A.TuplePat ps) = A.TuplePat (List.map pat ps)
	| pat (v as A.VarPat x) = A.VarPat (substVar s x)		    
	| pat (A.WildPat t) = A.WildPat t
	| pat (k as A.ConstPat _) = k
      and binding (A.ValBind (p, e)) = A.ValBind (pat p, exp e)
	| binding (A.PValBind (p, e)) = A.PValBind (pat p, exp e)
	| binding (A.FunBind ls) = A.FunBind (List.map lambda ls)
	| binding (primV as A.PrimVBind _) = primV
	| binding (code as A.PrimCodeBind _) = code 
      and lambda (A.FB (f, x, e)) = A.FB (substVar s f, x, exp e)
      in
        {exp=exp, binding=binding}
      end

  (* applySubst : subst -> A.exp -> A.exp *)
  (* Substitute variables for variables. *)
    fun applySubst (s: subst) : A.exp -> A.exp = let
      fun f (x, ts) =
        (case M.find (s, x)
	   of NONE   => A.VarExp (x, ts)
	    | SOME y => A.VarExp (y, ts)
	  (* end case *))
      val {exp, ...} = expWalk f s
      in
        exp
      end

  (* One-shot variable-variable substitution. *)
  (* substitute "this" for "that" in "e" *)
    fun subst1 (this: A.var, that: A.var, e: A.exp) : A.exp = 
      applySubst (singleton (this, that)) e

  (* perform the substitution e' [x -> e] *)
  (* Substitute the same expression for all variables in the subst. *)
  (* FIXME An odd function. Is this used anywhere? *)
    fun substForExp s e = let
      fun f (x, ts) = 
        (case M.find (s, x)
	   of SOME _ => e
	    | NONE   => A.VarExp (x, ts)
	  (* end case *))
      val {exp, ...} = expWalk f s
      in
        exp
      end

  end
