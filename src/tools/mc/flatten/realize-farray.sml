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

  structure AU = ASTUtil

  structure BEnv = BasisEnv
  structure MEnv = ModuleEnv

  structure FSet = FlattenOp.Set
  structure FMap = FlattenOp.Map

  fun ln () = TextIO.print "\n"
  fun println s = (TextIO.print s; ln ())

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
        of A.Lf (loExp, hiExp) => AU.mkApplyExp (const lfCon, [loExp, hiExp])
	 | A.Nd ts => let
	     val ts' = List.map mkTree ts
             in
	       AU.mkApplyExp (const ndCon, [AU.mkList (ts', ntreeTy)])
	     end
      end
    fun mkFArray (es, n, t) = let
      val {farrCon, ...} = basisItems ()
      val data = ParrLitToRope.mkRope (es, t)
      val shape = mkTree n
      val farrConConst = A.ConstExp (A.DConst (farrCon, [t]))
      in
        AU.mkApplyExp (farrConConst, [data, shape])
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
      | ty (T.TupleTy ts) = T.TupleTy (List.map ty ts)
      | ty (T.FArrayTy (t, n)) = T.ConTy ([ty t], farrTyc ())
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
 
  val realizeTypesInParrOper : A.parray_op -> A.parray_op = let
    fun ps (A.PSub_Nested t) = A.PSub_Nested (realizeTy t)
      | ps (A.PSub_Flat t) = A.PSub_Flat (realizeTy t)
      | ps (A.PSub_Tuple os) = A.PSub_Tuple (List.map ps os)
    fun pop (A.PA_Length t) = A.PA_Length (realizeTy t)
      | pop (A.PA_Sub s) = A.PA_Sub (ps s)
      | pop (A.PA_Tab t) = A.PA_Tab (realizeTy t)
      | pop (A.PA_Map t) = A.PA_Map (realizeTy t)
      | pop (A.PA_Reduce t) = A.PA_Reduce(realizeTy t)
    in
      pop
    end

  datatype var_bind 
    = Self
    | Other of A.var

(* pass1: realize farrays, change farray types, including parray opers's types,
 * and collect all flOps in a set *)
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
      | exp (A.FArrayExp (es, n, t)) = mkFArray (List.map exp es, n, ty t)
      | exp (A.FlOp oper) = let
          val oper' = realizeTypesInOper oper
          val _ = insOper (env, oper')
          in
	    A.FlOp oper'
	  end
      | exp (A.PArrayOp oper) = let
          val oper' = realizeTypesInParrOper oper
          in
            A.PArrayOp oper'
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

(* +debug *)

fun expressionForm (x : A.exp) : string = let
  fun e (A.LetExp _) = "LetExp"
    | e (A.IfExp _) = "IfExp"
    | e (A.CaseExp _) = "CaseExp"
    | e (A.PCaseExp _) = "PCaseExp"
    | e (A.HandleExp _) = "HandleExp"
    | e (A.RaiseExp _) = "RaiseExp"
    | e (A.FunExp _) = "FunExp"
    | e (A.ApplyExp _) = "ApplyExp"
    | e (A.VarArityOpExp _) = "VarArityOpExp"
    | e (A.TupleExp _) = "TupleExp"
    | e (A.RangeExp _) = "RangeExp"
    | e (A.PTupleExp _) = "PTupleExp"
    | e (A.PArrayExp _) = "PArrayExp"
    | e (A.PCompExp _) = "PCompExp"
    | e (A.PChoiceExp _) = "PChoiceExp"
    | e (A.SpawnExp _) = "SpawnExp"
    | e (A.ConstExp _) = "ConstExp"
    | e (A.VarExp _) = "VarExp"
    | e (A.SeqExp _) = "SeqExp"
    | e (A.OverloadExp _) = "OverloadExp"
    | e (A.ExpansionOptsExp _) = "ExpansionOptsExp"
    | e (A.FTupleExp _) = "FTupleExp"
    | e (A.FArrayExp _) = "FArrayExp"
    | e (A.FlOp _) = "FlOp"
    | e (A.PArrayOp _) = "PArrayOp"
  in
    e x
  end

fun look (e : A.exp) : unit = let
  fun ln () = print "\n"
  fun pr s = (print s; ln ())
  fun exp (A.LetExp (b, e)) = (pr ("let " ^ binding b ^ " in"); exp e)
    | exp (A.ExpansionOptsExp (opts, e)) = (pr "expansion"; exp e)
    | exp (A.TupleExp []) = pr "()"
    | exp (A.TupleExp es) = (pr "tuple("; app exp es; pr ")")
    | exp e = pr (expressionForm e) 
  and binding (A.ValBind (p, e)) = pat p ^ " = ..."
    | binding (A.PValBind (p, e)) = pat p ^ " = ..."
    | binding (A.FunBind lams) = String.concatWith "\n" (map lam lams)
    | binding (A.PrimVBind (x, _)) = var x ^ " = _prim(...)"
    | binding (A.PrimCodeBind c) = "_primcode(...)"
  and var x = Var.toString x
  and pat (A.ConPat (c, _, p)) = DataCon.toString c ^ "(" ^ pat p ^ ")"
    | pat (A.TuplePat ps) = "(" ^ String.concatWith "," (map pat ps) ^ ")"
    | pat (A.VarPat x) = var x
    | pat (A.WildPat _) = "_"
    | pat (A.ConstPat c) = "<const>"
  and lam (A.FB (f, x, b)) = "fun " ^ var f ^ "(" ^ var x ^ ") = ..."
  in
    pr "^^^^^ running LOOK"; exp e; raise Fail "HALT"
  end

(* -debug *)

(* mkOps : env -> A.lambda list *)
(* Generate all operators' code and insert oper/code pairs into the env. *)
  fun mkOps (env : env) (e : A.exp) : (A.fl_op * A.lambda) list = let
    val (E {operSet, operCode}) = env
    val s = !operSet
    val ols = FlattenOpGen.gen s
    val _ = List.app (fn (oper,lam) => insLam (env, oper, lam)) ols
(* +debug *)
    val _ = println "***** RAN mkOps *****"
    val _ = List.app (fn (oper, lam as A.FB (f, x, b)) =>
      (print (FlattenOp.toString oper);
       print " --> ";
       print (Var.toString f ^ " ...");
       ln ())) 
		     ols
(* -debug *)
    in
      ols
    end

(* pass2 : env -> A.exp -> A.exp *)
(* Replace all fl_ops with VarExps corresponding to their function names. *)
(* Insert code for each parray_op. *)
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
      | exp (A.FArrayExp (es, n, t)) = 
          raise Fail "FArrayExp: these should have been expanded away by now"
      | exp (A.FlOp oper) = let 
          val A.FB (f, x, b) = lookupLam (env, oper)
          in
	    A.VarExp (f, []) (* FIXME type list might not be empty here *)
          end
      | exp (pop as A.PArrayOp oper) = let
          val e = PArrayOpGen.gen oper 
	  val _ = print (concat ["+++++ for this: ", PArrayOp.toString oper, "\n",
				 "      generated this: "])
	  val _ = PrintAST.printExp e
	  val _ = print "\n"
          in
	    e
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

(* insertLams : A.lambda list -> A.exp -> A.exp *)
  fun insertLams (lams : A.lambda list) (e : A.exp) : A.exp = let
    fun isOneOfTheLams f = 
      List.exists (fn (A.FB (g, _, _)) => (Var.same (g, f))) lams
    fun lamsOccurIn b = let
      fun binding (A.ValBind (_, e)) = exp e
	| binding (A.PValBind (_, e)) = exp e
	| binding (A.FunBind lams') = List.exists lambda lams'
	| binding (A.PrimVBind _) = false
	| binding (A.PrimCodeBind _) = false
      and exp (A.LetExp (b, e)) = binding b orelse exp e
	| exp (A.IfExp (e1, e2, e3, _)) = List.exists exp [e1, e2, e3]
	| exp (A.CaseExp (e, ms, _)) = exp e orelse List.exists match ms
	| exp (A.PCaseExp (es, ms, _)) = List.exists exp es orelse List.exists pmatch ms
	| exp (A.HandleExp (e, ms, _)) = exp e orelse List.exists match ms
	| exp (A.RaiseExp (e, _)) = exp e
	| exp (A.FunExp (_, e, _)) = exp e
	| exp (A.ApplyExp (e1, e2, _)) = exp e1 orelse exp e2
	| exp (A.VarArityOpExp _) = false
	| exp (A.TupleExp es) = List.exists exp es
	| exp (A.RangeExp (e1, e2, optE, _)) = exp e1 orelse exp e2 orelse oexp optE
	| exp (A.PTupleExp es) = List.exists exp es
	| exp (A.PArrayExp (es, _)) = raise Fail "unexpected parray"
	| exp (A.PCompExp (e, pes, optE)) = 
            exp e orelse List.exists (exp o #2) pes orelse oexp optE
	| exp (A.PChoiceExp (es, _)) = List.exists exp es
	| exp (A.SpawnExp e) = exp e
	| exp (A.ConstExp _) = false
	| exp (A.VarExp (x, _)) = isOneOfTheLams x
	| exp (A.SeqExp (e1, e2)) = exp e1 orelse exp e2
	| exp (A.OverloadExp _) = raise Fail "unresolved overloading"
	| exp (A.ExpansionOptsExp (_, e)) = exp e
	| exp (A.FTupleExp _) = raise Fail "unexpected FTupleExp"
	| exp (A.FArrayExp _) = raise Fail "unexpected FArrayExp" 
	| exp (A.FlOp _) = raise Fail "unexpected FlOp"
	| exp (A.PArrayOp _) = raise Fail "unexpected PArrayOp"
      and oexp NONE = false
	| oexp (SOME e) = exp e
      and match (A.PatMatch (_, e)) = exp e
	| match (A.CondMatch (_, e1, e2)) = exp e1 orelse exp e2
      and pmatch (A.PMatch (_, e)) = exp e
	| pmatch (A.Otherwise (_, e)) = exp e
      and lambda (A.FB (_, _, b)) = exp b
      in
	binding b
      end
    fun topLevelExp (lexp as A.LetExp (b, e)) = 
	  if lamsOccurIn b then 
	    AU.mkLetExp ([A.FunBind lams], lexp)
	  else
            A.LetExp (b, topLevelExp e)
      | topLevelExp (A.ExpansionOptsExp (opts, e)) = 
	  A.ExpansionOptsExp (opts, topLevelExp e)
      | topLevelExp (theEnd as A.TupleExp []) = theEnd
      | topLevelExp e = raise Fail ("unexpected " ^ expressionForm e)
    in
      topLevelExp e
    end
					   
(* realize : A.exp -> A.exp *)
  fun realize (e0 : A.exp) : A.exp = let
    val env = initEnv ()
    val e1 = pass1 env e0
    val ols = mkOps env e1
    val e2 = pass2 env e1
    val e3 = let
      val (ops, lams) = ListPair.unzip ols
      in
	insertLams lams e2
      end
    in
      e3
    end

end
