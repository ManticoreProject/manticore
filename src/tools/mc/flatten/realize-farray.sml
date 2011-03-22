(* realize-farray.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Convert abstract farrays to their AST realizations as
 *   FArray (data:tau rope, shape:nesting_tree)
 * where FArray is in path/to/src/lib/basis/parray/farray.pml.
 *)

structure RealizeFArray : sig

  val realize : AST.exp -> AST.exp

end = struct

  structure A = AST
  structure T = Types 
  structure B = Basis
  structure U = ASTUtil

  structure BEnv = BasisEnv
  structure MEnv = ModuleEnv

  fun assert (msg : string) (fact : bool) : unit = 
    if not fact then raise Fail ("assertion failure: " ^ msg) else ()

  local
    fun dcon c  = BEnv.getDConFromBasis ["FArray", c]
    fun tyc c   = BEnv.getTyConFromBasis ["FArray", c]
    fun const c = A.ConstExp (A.DConst (c, []))
  in
    fun mkTree (n : A.ntree) : A.exp = (case n
      of A.Lf (loExp, hiExp) => 
           U.mkApplyExp (const (dcon "Lf"), [loExp, hiExp])
       | A.Nd ts => let
           val ntreeTyc = tyc "nesting_tree"
	   val ntreeTy = A.ConTy ([], ntreeTyc)
	   val ndConConst = const (dcon "Nd")
           val ts' = List.map mkTree ts
           in
	     U.mkApplyExp (ndConConst, [U.mkList (ts', ntreeTy)])
	   end
      (* end case *))
    fun mkFArray (es, n, t) = let
      val data = ParrLitToRope.mkRope (es, t)
      val shape = mkTree n
      val farrayConConst = A.ConstExp (A.DConst (dcon "f_array", [t]))
      in
        U.mkApplyExp (farrayConConst, [data, shape])
      end
  end (* local *)

  structure FSet = FlattenOp.Set
  structure FMap = FlattenOp.Map

  datatype env = E of {operSet  : FSet.set ref, 
		       operCode : A.lambda FMap.map ref}

  fun initEnv () = E {operSet  = ref FSet.empty, operCode = ref FMap.empty}

  fun insOper (E {operSet, ...}, oper) = let
    val s  = !operSet
    val s' = FSet.add (s, oper)
    in
      operSet := s'
    end

  fun insLam (E {operCode, ...}, oper, lam) = let
    val m  = !operCode
    val m' = FMap.insert (m, oper, lam)
    in
      operCode := m'
    end

  fun lookupLam (E {operCode, ...}, oper) = let
    val m = !operCode
    in case FMap.find (m, oper)
      of NONE => raise Fail ("lookupLam " ^ FlattenOp.toString oper)
       | SOME lam => lam
    end

(* pass1: realize farrays and collect all flOps in a set *)
  fun pass1 (env : env) (e : A.exp) : A.exp = let
    fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
      | exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
      | exp (A.CaseExp (e, ms, t)) = A.CaseExp (exp e, List.map match ms, t)
      | exp (A.PCaseExp (es, ms, t)) = 
	  A.PCaseExp (List.map exp es, List.map pmatch ms, t)
      | exp (A.HandleExp (e, ms, t)) = A.HandleExp (exp e, List.map match ms, t)
      | exp (A.RaiseExp (e, t)) = A.RaiseExp (exp e, t)
      | exp (A.FunExp (x, e, t)) = A.FunExp (x, exp e, t)
      | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
      | exp (A.VarArityOpExp _) = raise Fail "todo"
      | exp (A.TupleExp es) = A.TupleExp (List.map exp es)
      | exp (A.RangeExp (e1, e2, optE, t)) =
          A.RangeExp (exp e1, exp e2, Option.map exp optE, t)
      | exp (A.PTupleExp es) = A.PTupleExp (List.map exp es)
      | exp (A.PArrayExp (es, t)) = A.PArrayExp (List.map exp es, t)
      | exp (A.PCompExp (e, pes, optE)) = let
          fun pe (p,e) = (p, exp e)
          in
	    A.PCompExp (exp e, List.map pe pes, Option.map exp optE)
	  end
      | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (List.map exp es, t)
      | exp (A.SpawnExp e) = A.SpawnExp (exp e)
      | exp (c as A.ConstExp _) = c
      | exp (x as A.VarExp _) = x
      | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
      | exp (x as A.OverloadExp _) = x
      | exp (A.ExpansionOptsExp (opts, e)) = A.ExpansionOptsExp (opts, exp e)
      | exp (A.FTupleExp es) = A.FTupleExp (List.map exp es)
      | exp (A.FArrayExp (es, n, t)) = 
(* **** one of two cases that does anything interesting: **** *)
          mkFArray (es, n, t)
      | exp (oper as A.FlOp o1) = 
(* **** one of two cases that does anything interesting: **** *)
          (insOper (env, o1); oper)
    and binding (A.ValBind (p, e)) = A.ValBind (p, exp e)
      | binding (A.PValBind (p, e)) = A.PValBind (p, exp e)
      | binding (A.FunBind lams) = A.FunBind (List.map lambda lams)
      | binding (p as A.PrimVBind _) = p
      | binding (c as A.PrimCodeBind _) = c
    and match (A.PatMatch (p, e)) = A.PatMatch (p, exp e)
      | match (A.CondMatch (p, e1, e2)) = A.CondMatch (p, exp e1, exp e2)
    and pmatch (A.PMatch (ps, e)) = A.PMatch (ps, exp e)
      | pmatch (A.Otherwise (ts, e)) = A.Otherwise (ts, exp e)
    and lambda (A.FB (f, x, b)) = A.FB (f, x, exp b)
    in
      exp e
    end

(* mkOps : env -> A.lambda list *)
(* Generate all operators' code, insert oper/code pairs into the env, 
 *   and prepend their definitions to the given exp. *)
  fun mkOps (env : env) (e : A.exp) : A.exp = let
    val (E {operSet, operCode}) = env
    val s = !operSet
    val ols = FlattenOpGen.gen s
    val _ = List.app (fn (oper,lam) => insLam (env, oper, lam)) ols
    val (opers, lams) = ListPair.unzip ols
    val e' = ASTUtil.mkLetExp ([A.FunBind lams], e)
    in
      e'
    end

(* pass2 : env -> A.exp -> A.exp *)
(* Replace all fl_ops with VarExps corresponding to their function names. *)
  fun pass2 (env : env) (e : A.exp) : A.exp = let
    fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
      | exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
      | exp (A.CaseExp (e, ms, t)) = A.CaseExp (exp e, List.map match ms, t)
      | exp (A.PCaseExp (es, ms, t)) = 
	  A.PCaseExp (List.map exp es, List.map pmatch ms, t)
      | exp (A.HandleExp (e, ms, t)) = A.HandleExp (exp e, List.map match ms, t)
      | exp (A.RaiseExp (e, t)) = A.RaiseExp (exp e, t)
      | exp (A.FunExp (x, e, t)) = A.FunExp (x, exp e, t)
      | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
      | exp (A.VarArityOpExp _) = raise Fail "todo"
      | exp (A.TupleExp es) = A.TupleExp (List.map exp es)
      | exp (A.RangeExp (e1, e2, optE, t)) =
          A.RangeExp (exp e1, exp e2, Option.map exp optE, t)
      | exp (A.PTupleExp es) = A.PTupleExp (List.map exp es)
      | exp (A.PArrayExp (es, t)) = A.PArrayExp (List.map exp es, t)
      | exp (A.PCompExp (e, pes, optE)) = let
          fun pe (p,e) = (p, exp e)
          in
	    A.PCompExp (exp e, List.map pe pes, Option.map exp optE)
	  end
      | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (List.map exp es, t)
      | exp (A.SpawnExp e) = A.SpawnExp (exp e)
      | exp (c as A.ConstExp _) = c
      | exp (x as A.VarExp _) = x
      | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
      | exp (x as A.OverloadExp _) = x
      | exp (A.ExpansionOptsExp (opts, e)) = A.ExpansionOptsExp (opts, exp e)
      | exp (A.FTupleExp es) = A.FTupleExp (List.map exp es)
      | exp (A.FArrayExp (es, n, t)) = raise Fail "these should have been expanded away by now"
      | exp (A.FlOp oper) = let 
(* **** the only case that does anything interesting: **** *)
          val A.FB (f, x, b) = lookupLam (env, oper)
          in
	    A.VarExp (f, []) (* FIXME type list might not be empty here *)
          end
    and binding (A.ValBind (p, e)) = A.ValBind (p, exp e)
      | binding (A.PValBind (p, e)) = A.PValBind (p, exp e)
      | binding (A.FunBind lams) = A.FunBind (List.map lambda lams)
      | binding (p as A.PrimVBind _) = p
      | binding (c as A.PrimCodeBind _) = c
    and match (A.PatMatch (p, e)) = A.PatMatch (p, exp e)
      | match (A.CondMatch (p, e1, e2)) = A.CondMatch (p, exp e1, exp e2)
    and pmatch (A.PMatch (ps, e)) = A.PMatch (ps, exp e)
      | pmatch (A.Otherwise (ts, e)) = A.Otherwise (ts, exp e)
    and lambda (A.FB (f, x, b)) = A.FB (f, x, exp b)
    in
      exp e
    end

(* realize : A.exp -> A.exp *)
  fun realize (e0 : A.exp) : A.exp = let
    val env = initEnv ()
    val e1 = pass1 env e0
    val e2 = mkOps env e1
    val e3 = pass2 env e2
    in
      e3
    end

end
