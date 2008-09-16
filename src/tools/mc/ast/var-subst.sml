(* var-subst.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

 (* : sig

    type subst

    val id        : subst
    val singleton : AST.var * AST.var -> subst 
    val add       : subst -> AST.var * AST.var -> subst 
    val pat       : subst -> AST.pat -> AST.pat
    val touchExp  : subst -> AST.exp -> AST.exp
    
    val subst1    : AST.var * AST.var * AST.exp -> AST.exp 

  end *)

structure VarSubst = 
  struct
  
    structure A = AST
    structure T = Types
(*    structure F = Futures*)
    
  (* fail : string -> 'a *)
    fun fail msg = raise Fail msg

  (* todo : string -> 'a *)
    fun todo thing = fail ("todo: " ^ thing)
			 
    structure VarMap = Var.Map

    type subst = Var.var VarMap.map

  (* id : subst *)
    val id : subst = VarMap.empty

    fun add ((k, v), s) = VarMap.insert (s, k, v)

    fun idSubst v = add((v, v), id)

    fun substVar s v = (case VarMap.find (s, v)
		       of NONE => v
			| SOME x => x)

(*
  (* pat : subst -> A.pat -> A.pat *)
    fun pat s p =
	let fun f (A.ConPat (c, ts, p)) = A.ConPat (c, ts, f p)
	      (*                                       ^^           *)
	      (* FIXME: I may need to futurize some of these types. *)
	      | f (A.TuplePat ps) = A.TuplePat (List.map f ps)
	      | f (v as A.VarPat x) = A.VarPat (var s x)		    
	      | f (A.WildPat t) = A.WildPat t
	      | f (k as A.ConstPat _) = k
	in
	    f p
	end
*)
  (* expWalk : (A.var * A.ty list -> A.exp) -> subst -> A.exp -> A.exp *)
    fun expWalk f s = let
      fun exp (A.LetExp (b, e)) = A.LetExp (binding s b, exp e)
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
	| exp (ov as A.OverloadExp _) = ov
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
      and binding s (A.ValBind (p, e)) = A.ValBind (pat p, exp e)
	| binding s (A.PValBind (p, e)) = A.PValBind (pat p, exp e)
	| binding s (A.FunBind ls) = A.FunBind (List.map (lambda s) ls)
      and lambda s (A.FB (f, x, e)) = A.FB(substVar s f, x, exp e)

      in
        {exp=exp, binding=binding}
      end

(*
  (* exp : subst -> A.exp -> A.exp *)
  (* Given a subst like [x -> y] and an expression (x + 2), *)
  (*   produces (y + 2). *)
    and exp s =
	let fun f (x, ts) = (case VarMap.find (s, x)
			      of NONE => A.VarExp (x, ts)
			       | SOME x' => A.VarExp (x', ts))
	in
	    expWalk f s
	end
*)

(*    and module s m =
	(case m
	  of A.M_Body (info, tds) => A.M_Body (info, topDecs s tds)
	   | m => m
	(* end case *))

    and topDecs s ds = List.rev (List.foldl (fn (td, tds) => topDec s td :: tds) [] ds)

    and topDec s d =
	(case d
	  of A.TD_Module (info, mr, mt, m) => A.TD_Module(info, mr, mt, module s m)
	   | A.TD_DCon dc => A.TD_DCon dc
	   | A.TD_Binding b => A.TD_Binding (binding s b)
	(* end case *))
*)

  (* perform the substitution e[x -> e2] *)
    fun substForExp s e2 = let
        fun f (x, ts) = (case VarMap.find (s, x)
			      of NONE => A.VarExp (x, ts)
			       | SOME x' => e2)
	val {exp, binding} = expWalk f s
        in
	   exp
        end

  (* touchExp : subst -> A.exp * A.exp -> A.exp *)
  (* Given a subst like [x -> xf] and an expression (x + 2), *)
  (*   produces ((touch xf) + 2). *)
  (* n.b. Type-preserving when x : 'a and xf : 'a future. *)
(*    fun touchExp s =
	let fun f (x, ts) = (case VarMap.find (s, x)
				  of NONE => A.VarExp (x, ts)
				   | SOME x' => F.mkTouch (A.VarExp (x', ts)))
	in
	    expWalk f s
	end
*)

    (* substitute "this" for "that" in "e" *)
    fun subst1 (this, that, e) = let
      val s = add ((this, that), id)
      fun var (x, ts) = (case VarMap.find (s, x)
			  of NONE => A.VarExp (x, ts)
			   | SOME x' => A.VarExp (x', ts))
      val {exp, binding} = expWalk var s
      in
        exp e        
      end

  end
