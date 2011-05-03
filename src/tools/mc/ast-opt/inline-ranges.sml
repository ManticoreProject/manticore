(* inline-ranges.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Inline simple ranges.
 *
 * A simple range is either
 * - a RangeExp whose subexpressions are variables or consts, or
 * - a tuple of simple ranges
 *)

(* TODO: Make this inline *all* ranges by binding range endpoints to names first. *)

structure InlineRanges : sig

    val inlineRanges : AST.exp -> AST.exp

end = struct

  structure A = AST
  structure B = Basis
  structure T = Types

  structure VM = Var.Map

  type env = A.exp VM.map
  fun extend (env, x, e) = VM.insert (env, x, e)

  local
    fun simple e = (case e
      of A.ConstExp _ => true
       | A.VarExp _ => true
       | _ => false
      (* end case *))
  in
    fun simpleRanges (A.RangeExp (e1, e2, optE, _)) =
          simple e1 andalso simple e2 andalso
	  (case optE of NONE => true | SOME e3 => simple e3)
      | simpleRanges (A.TupleExp es) = List.all simpleRanges es
      | simpleRanges _ = false
  end (* local *)

  val changed = ref false
  fun resetChanged () = changed := false
  fun noteChange x = (changed := true; x)

  val go : env -> A.exp -> A.exp = let
    val $ = List.map
    fun exp env e = (case e
      of A.LetExp (b, e) => let
           val (b', env') = binding (env, b)
           in
	     A.LetExp (b', exp env' e)
           end
       | A.IfExp (e1, e2, e3, t) => 
           A.IfExp (exp env e1, exp env e2, exp env e3, t)
       | A.CaseExp (e, ms, t) => A.CaseExp (exp env e, $(match env) ms, t)
       | A.PCaseExp (es, ms, t) => 
           A.PCaseExp ($(exp env) es, $(pmatch env) ms, t)
       | A.HandleExp (e, ms, t) => A.HandleExp (exp env e, $(match env) ms, t)
       | A.RaiseExp (e, t) => A.RaiseExp (exp env e, t)
       | A.FunExp (x, e, t) => A.FunExp (x, exp env e, t)
       | A.ApplyExp (e1, e2, t) => A.ApplyExp (exp env e1, exp env e2, t)
       | v as A.VarArityOpExp _ => v
       | A.TupleExp es => A.TupleExp ($(exp env) es)
       | A.RangeExp (e1, e2, optE3, t) =>
           A.RangeExp (exp env e1, exp env e2, Option.map (exp env) optE3, t)
       | A.PTupleExp es => A.PTupleExp ($(exp env) es)
       | A.PArrayExp (es, t) => A.PArrayExp ($(exp env) es, t)
       | A.PCompExp (e, pes, optE) => 
           A.PCompExp (exp env e, 
		       $ (fn (p,e) => (p, exp env e)) pes, 
		       Option.map (exp env) optE)
       | A.PChoiceExp (es, t) => A.PChoiceExp ($(exp env) es, t)
       | A.SpawnExp e => A.SpawnExp (exp env e)
       | c as A.ConstExp _ => c
       | ve as A.VarExp (x, ts) => (case VM.find (env, x)
           of SOME range => noteChange range
	    | NONE => ve
           (* end case *))
       | A.SeqExp (e1, e2) => A.SeqExp (exp env e1, exp env e2)
       | ov as A.OverloadExp _ => ov
       | A.ExpansionOptsExp (opts, e) => A.ExpansionOptsExp (opts, exp env e)
       | p as A.PArrayOp _ => p
       | A.FTupleExp es => A.FTupleExp ($(exp env) es)
       | A.FArrayExp (es, s, t) => A.FArrayExp ($(exp env) es, ntree env s, t)
       | f as A.FlOp _ => f
      (* end case *))
    and binding (env, b) = (case b
      of A.ValBind (p, e) => valBind (env, A.ValBind, p, e)
       | A.PValBind (p, e) => valBind (env, A.PValBind, p, e)
       | A.PrimVBind _ => (b, env)
       | A.PrimCodeBind _ => (b, env)
       | A.FunBind lams => (A.FunBind ($(lambda env) lams), env)
      (* end case *))
    and valBind (env, k, p, e) = (case p
      of A.VarPat x =>
           if simpleRanges e then
             (k (p, e), extend (env, x, e))
	   else
	     (k (p, exp env e), env)
       | _ => (k (p, exp env e), env)
      (* end case *))
    and ntree env n = (case n
      of A.Lf (e1, e2) => A.Lf (exp env e1, exp env e2)
       | A.Nd ns => A.Nd ($(ntree env) ns)
      (* end case *))
    and lambda env (A.FB (f, x, b)) = A.FB (f, x, exp env b)
    and match env m = (case m
      of A.PatMatch (p, e) => A.PatMatch (p, exp env e)
       | A.CondMatch (p, e1, e2) => A.CondMatch (p, exp env e1, exp env e2)
      (* end case *))
    and pmatch env m = (case m
      of A.PMatch (ps, e) => A.PMatch (ps, exp env e)
       | A.Otherwise (ts, e) => A.Otherwise (ts, exp env e)
      (* end case *))   
    in
      exp
    end

  val inlineRangesInternal : A.exp -> A.exp = let
    fun lp e = let
      val _ = resetChanged ()
      val e' = go VM.empty e
      in
        if !changed then lp e' else e'
      end
    in
      lp
    end

  val inlineRanges : A.exp -> A.exp = BasicControl.mkKeepPassSimple {
    output = PrintAST.outputExpNoTypes,
    ext = "ast",
    passName = "inline-ranges",
    pass = inlineRangesInternal,
    registry = ASTOptControls.registry
  }

end
