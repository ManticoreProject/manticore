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

  structure FSet = FlattenOp.Set
  structure FMap = FlattenOp.Map

  fun assert (msg : string) (fact : bool) : unit = 
    if not fact then raise Fail ("assertion failure: " ^ msg) else ()

  local
    val cell = ref NONE
    fun basisItems () = (case !cell
      of NONE => let
	   val ft = BEnv.getTyConFromBasis ["FArray", "f_array"]
           val nt = BEnv.getTyConFromBasis ["FArray", "nesting_tree"]
	   val fc = BEnv.getDConFromBasis ["FArray", "FArray"]
	   val lf = BEnv.getDConFromBasis ["FArray", "Lf"]
	   val nd = BEnv.getDConFromBasis ["FArray", "Nd"]
	   val record = {farrTyc=ft, ntreeTyc=nt, farrCon=fc, lfCon=lf, ndCon=nd}
           val _ = (cell := SOME record)
           in
             record
	   end
       | SOME r => r
      (* end case *))
    fun const c = A.ConstExp (A.DConst (c, []))
  in
    fun farrTyc () = #farrTyc (basisItems ())
    fun mkTree (n : A.ntree) : A.exp = let
      val {ntreeTyc, farrCon, lfCon, ndCon, ...} = basisItems ()
      val ntreeTy = A.ConTy ([], ntreeTyc)
      in case n
        of A.Lf (loExp, hiExp) => U.mkApplyExp (const lfCon, [loExp, hiExp])
	 | A.Nd ts => let
	     val ts' = List.map mkTree ts
             in
	       U.mkApplyExp (const ndCon, [U.mkList (ts', ntreeTy)])
	     end
      end
    fun mkFArray (es, n, t) = let
      val {farrCon, ...} = basisItems ()
      val data = ParrLitToRope.mkRope (es, t)
      val shape = mkTree n
      val farrConConst = A.ConstExp (A.DConst (farrCon, [t]))
      in
        U.mkApplyExp (farrConConst, [data, shape])
      end
  end (* local *)

  datatype env = E of {operSet  : FSet.set ref, 
		       operCode : A.lambda FMap.map ref}

  fun initEnv () = E {operSet = ref FSet.empty, operCode = ref FMap.empty}

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

  fun includesFArr (t : T.ty) : bool = let
    fun ty (T.ErrorTy) = false
      | ty (T.MetaTy (T.MVar {info, ...})) = (case !info
          of T.INSTANCE t' => ty t'
	   | _ => false
          (* end case *))
      | ty (T.VarTy a) = false
      | ty (T.ConTy (ts, c)) = List.exists ty ts
      | ty (T.FunTy (t1, t2)) = ty t1 orelse ty t2
      | ty (T.TupleTy ts) = List.exists ty ts
      | ty (T.FArrayTy _) = true
    in
      ty t
    end

(* anywhere we see the type t farray, change it to (t rope * ntree) *)
  fun realizeTy (t : T.ty) : T.ty = let
    fun ty (T.ErrorTy) = T.ErrorTy
      | ty (m as T.MetaTy (T.MVar {info, ...})) = (case !info
          of T.INSTANCE t' => let
               val t'' = ty t'
               in
                 info := T.INSTANCE t'';
	         m
	       end
	   | _ => m
          (* end case *))
      | ty (T.VarTy a) = T.VarTy a
      | ty (T.ConTy (ts, c)) = T.ConTy (List.map ty ts, c)
          (* note: the constructor c should have already been replaced 
	   * with its flat constructor by now *)
      | ty (T.FunTy (t1, t2)) = T.FunTy (ty t1, ty t2)
      | ty (T.TupleTy ts) = T.TupleTy ts
      | ty (T.FArrayTy (t, n)) = T.ConTy ([t], farrTyc ())
          (* we lose the shape tree type here *)
    in
      ty t
    end

  fun realizeScheme (T.TyScheme (xs, t)) = T.TyScheme (xs, realizeTy t)

  fun realizeTypesInOper (oper : A.fl_op) : A.fl_op = let
    val ty = realizeTy
    fun rt (A.ID t) = A.ID (ty t)
      | rt (A.Unzip t) = A.Unzip (ty t)
      | rt (A.Cat t) = A.Cat (ty t)
      | rt (A.Map (op1, n)) = A.Map (rt op1, n)
      | rt (A.Compose (op1, op2)) = A.Compose (rt op1, rt op2)
      | rt (A.CrossCompose ops) = A.CrossCompose (List.map rt ops)
    in
      rt oper
    end

  datatype var_bind 
    = Self
    | Other of A.var

(* pass1: realize farrays, change farray types, and collect all flOps in a set *)
  fun pass1 (env : env) (e : A.exp) : A.exp = let
    val vars : var_bind Var.Tbl.hash_table = let
      (* note: vars are all uniquely stamped, so one doesn't need to worry about scope *)
      val tbl = Var.Tbl.mkTable (128, Fail "vars")
      val _ = List.app (Var.Tbl.insert tbl) [(B.eq, Self), (B.neq, Self)]   
      in
	tbl
      end
    val ty = realizeTy
    fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
      | exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, ty t)
      | exp (A.CaseExp (e, ms, t)) = A.CaseExp (exp e, List.map match ms, ty t)
      | exp (A.PCaseExp (es, ms, t)) = 
	  A.PCaseExp (List.map exp es, List.map pmatch ms, ty t)
      | exp (A.HandleExp (e, ms, t)) = A.HandleExp (exp e, List.map match ms, ty t)
      | exp (A.RaiseExp (e, t)) = A.RaiseExp (exp e, ty t)
      | exp (A.FunExp (x, e, t)) = let
          val x' = var x
          in
	    A.FunExp (x', exp e, ty t)
	  end
      | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, ty t)
      | exp (A.VarArityOpExp _) = raise Fail "todo"
      | exp (A.TupleExp es) = A.TupleExp (List.map exp es)
      | exp (A.RangeExp (e1, e2, optE, t)) =
          A.RangeExp (exp e1, exp e2, Option.map exp optE, ty t)
      | exp (A.PTupleExp es) = A.PTupleExp (List.map exp es)
      | exp (A.PArrayExp (es, t)) = A.PArrayExp (List.map exp es, ty t)
      | exp (A.PCompExp (e, pes, optE)) = let
          fun pe (p,e) = (pat p, exp e)
          in
	    A.PCompExp (exp e, List.map pe pes, Option.map exp optE)
	  end
      | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (List.map exp es, ty t)
      | exp (A.SpawnExp e) = A.SpawnExp (exp e)
      | exp (A.ConstExp c) = A.ConstExp (const c)
      | exp (A.VarExp (x, ts)) = let
          val x' = (case Var.Tbl.find vars x
            of NONE => raise Fail ("unbound var " ^ Var.toString x)
	     | SOME Self => x
	     | SOME (Other y) => y)
	  val ts' = List.map ty ts
          in
            A.VarExp (x', ts')
	  end
      | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
      | exp (x as A.OverloadExp _) = raise Fail "pass1: overloading unresolved"
      | exp (A.ExpansionOptsExp (opts, e)) = A.ExpansionOptsExp (opts, exp e)
      | exp (A.FTupleExp es) = A.FTupleExp (List.map exp es)
      | exp (A.FArrayExp (es, n, t)) = mkFArray (es, n, ty t)
      | exp (A.FlOp oper) = let
          val oper' = realizeTypesInOper oper
          val _ = insOper (env, oper')
          in
	    A.FlOp oper'
	  end
    and binding (A.ValBind (p, e)) = A.ValBind (pat p, exp e)
      | binding (A.PValBind (p, e)) = A.PValBind (pat p, exp e)
      | binding (A.FunBind lams) = A.FunBind (List.map lambda lams)
      | binding (p as A.PrimVBind (x, _)) = (Var.Tbl.insert vars (x, Self); p)
      | binding (c as A.PrimCodeBind _) = c
    and match (A.PatMatch (p, e)) = A.PatMatch (pat p, exp e)
      | match (A.CondMatch (p, e1, e2)) = A.CondMatch (pat p, exp e1, exp e2)
    and pmatch (A.PMatch (ps, e)) = A.PMatch (List.map ppat ps, exp e)
      | pmatch (A.Otherwise (ts, e)) = A.Otherwise (List.map ty ts, exp e)
    and lambda (A.FB (f, x, b)) = A.FB (var f, var x, exp b)
    and pat (A.ConPat (c, ts, p)) = A.ConPat (c, List.map ty ts, pat p)
      | pat (A.TuplePat ps) = A.TuplePat (List.map pat ps)
      | pat (A.VarPat x) = A.VarPat (var x)
      | pat (A.WildPat t) = A.WildPat (ty t)
      | pat (A.ConstPat c) = A.ConstPat (const c)
    and ppat (A.NDWildPat t) = A.NDWildPat (ty t)
      | ppat (A.HandlePat (p, t)) = A.HandlePat (pat p, ty t)
      | ppat (A.Pat p) = A.Pat (pat p)
    and const (A.DConst (c, ts)) = A.DConst (c, List.map ty ts)
      | const (lit as A.LConst _) = lit
    and var x = (* binding sites *) let
      val tySch as T.TyScheme (_, t) = Var.typeOf x
(* (\* +debug *\) *)
(*       val _ = print ("inspecting " ^ Var.nameOf x ^ ": " ^ TypeUtil.schemeToString tySch ^ *)
(* 		     "\n  includesFarr=" ^ Bool.toString (includesFArr t) ^ "\n") *)
(* (\* -debug *\) *)
      in 
        if includesFArr t then let
          val tySch' = realizeScheme tySch
	  val x' = Var.newPoly (Var.nameOf x ^ "~", tySch')
          in
            Var.Tbl.insert vars (x, Other x');
	    x'
	  end
	else (Var.Tbl.insert vars (x, Self); x)
      end
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
      | exp (A.PArrayExp (es, t)) = raise Fail "these should have been expanded away by now"
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
