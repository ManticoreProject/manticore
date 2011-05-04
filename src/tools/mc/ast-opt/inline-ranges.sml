(* inline-ranges.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Inline ranges.
 *
 *)

structure InlineRanges : sig

    val inlineRanges : AST.exp -> AST.exp

end = struct

  structure A = AST
  structure B = Basis
  structure T = Types
  structure AU = ASTUtil

  structure VM = Var.Map

  type env = A.exp VM.map
  fun extend (env, x, e) = VM.insert (env, x, e)

  fun todo () = raise Fail "todo"

  fun println s = (print s; print "\n")

  fun printEnv env = let
    fun tos env = let
      val xrs = VM.listItemsi env
      fun s (x, r) = Var.nameOf x ^ "-->" ^ "rng"
      in
        String.concatWith "," (List.map s xrs)
      end
    in
      println (tos env)
    end

  fun constOrVar e = (case e
    of A.ConstExp _ => true
     | A.VarExp _ => true
     | _ => false
    (* end case *))

  val changed = ref false
  fun resetChanged () = changed := false
  fun noteChange x = (changed := true; x)

  val go : env -> A.exp -> A.exp = let
    val $ = List.map
    fun exp env e = (case e
      of A.LetExp (b, e) => letExp (env, b, e)
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
    and letExp (env, b, e) = (case b
      of A.PrimVBind _ => A.LetExp (b, exp env e)
       | A.PrimCodeBind _ => A.LetExp (b, exp env e)
       | A.FunBind lams => A.LetExp (A.FunBind ($(lambda env) lams), exp env e)
       | A.ValBind (p, rng as A.RangeExp (e1, e2, o3, t)) =>
           rangeBind (env, b, e, A.ValBind, p, e1, e2, o3, t)
       | A.ValBind (p, notRng) (* binding is not to a RangeExp *) =>
           A.LetExp (A.ValBind (p, exp env notRng), exp env e)
       | A.PValBind (p, rng as A.RangeExp (e1, e2, o3, t)) => 
           rangeBind (env, b, e, A.PValBind, p, e1, e2, o3, t)
       | A.PValBind (p, notRng) (* binding is not to a RangeExp *) =>
           A.LetExp (A.PValBind (p, exp env notRng), exp env e)
      (* end case *))
    and rangeBind (env, b, e, bindForm, p, e1, e2, o3, t) = let
      fun maybeBind (oldBinds, expBindCand, varName, k) = 
            if constOrVar expBindCand then
              (* don't introduce a new var for a const or var *)
              (oldBinds, k expBindCand)
	    else let
              val var = Var.new (varName, t)
	      val bind = bindForm (A.VarPat var, expBindCand)
	      val exp = A.VarExp (var, [])
              in
                (bind::oldBinds, k exp)
              end
      in (case p
        of A.VarPat x => let
             fun name suff = Var.nameOf x ^ suff
             val (binds3, o3') = (case o3 
	       of NONE => ([], NONE)
		| SOME e3 => maybeBind ([], e3, name "Step", SOME)
               (* end case *))
	     val (binds2, e2') = maybeBind (binds3, e2, name "Hi", fn e => e)
	     val (binds1, e1') = maybeBind (binds2, e1, name "Lo", fn e => e)
             val rng' = A.RangeExp (e1', e2', o3', t)
	     val binds = binds1 @ [A.ValBind (A.VarPat x, rng')]
	     val env' = extend (env, x, rng')
             in
               AU.mkLetExp (binds, exp env' e)
             end
	 | _ => (* _very_ unlikely not be a VarPat, but we'll soldier forth *)
             A.LetExp (b, exp env e)
        (* end case *))
      end
    in
      exp
    end

  val inlineRangesInternal : A.exp -> A.exp = let
  (* TODO: The env really should persist between runs of lp.
   * As it stands every binding 
   *   val x = <rngExp>
   * is put back into the env every time around.
   * Using a Var.Tbl instead of a Var.Map might be the easiest way to make this change.
   *)
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
