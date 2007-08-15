(* translate.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Translate : sig

    val translate : AST.module -> BOM.module

    val test : int -> unit

  end = struct

    structure V = Var
    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy
    structure Lit = Literal

    (* fail : string -> 'a *)
    fun fail msg = raise Fail msg

    (* todo : string -> 'a *)
    fun todo msg = fail ("todo: " ^ msg)

    val trTy = TranslateTypes.tr
    val trScheme = TranslateTypes.trScheme

    datatype env = E of {
	exh : B.var,		(* current exception handler *)
	vmap : B.var V.Map.map	(* map from AST variables to BOM variables *)
      }

    fun mkEnv exh = E{exh = exh, vmap = V.Map.empty}

    fun find (E{vmap, ...}, x) = V.Map.find(vmap, x)

    fun insert (E{vmap, exh}, x, x') =
	  E{vmap=V.Map.insert(vmap, x, x'), exh=exh}

    fun newHandler (E{vmap, ...}) = let
	  val exh = BV.new("_exh", BTy.exhTy)
	  in
	    (exh, E{vmap = vmap, exh = exh})
	  end

    fun handlerOf (E{exh, ...}) = [exh]

  (* prune out overload nodes.
   * NOTE: we should probably have a pass that does this before
   * AST optimization.
   *)
    fun prune (AST.OverloadExp(ref(AST.Instance x))) = AST.VarExp(x, [])
      | prune (AST.OverloadExp _) = raise Fail "unresolved overloading"
      | prune e = e

  (* translate a binding occurrence of an AST variable to a BOM variable *)
    fun trVar (env, x) = let
	  val x' = BV.new(V.nameOf x, trScheme(V.typeOf x))
	  in
	    (x', insert(env, x, x'))
	  end

  (* the yield of translating an AST expression *)
    datatype bom_code
      = BIND of (B.var list * B.rhs)
      | EXP of B.exp

    fun toExp (BIND(xs, rhs)) = B.mkStmt(xs, rhs, B.mkRet xs)
      | toExp (EXP e) = e

    fun trExp (env, exp) : bom_code = (case prune exp
	   of AST.LetExp(b, e) =>
		EXP(trBind (env, b, fn env' => trExpToExp(env', e)))
	    | AST.IfExp(e1, e2, e3, ty) =>
		EXP(trExpToV (env, e1, fn x =>
		  B.mkIf(x, trExpToExp(env, e2), trExpToExp(env, e3))))
	    | AST.CaseExp(e, rules, ty) =>
		EXP(trExpToV (env, e, fn x => trCase(env, x, rules)))
	    | AST.ApplyExp(e1, e2, ty) => EXP(trExpToV (env, e1, fn f =>
		trExpToV (env, e2, fn arg =>
		  B.mkApply(f, [arg], handlerOf env))))
	    | AST.TupleExp[] => let
		val t = BV.new("_unit", BTy.unitTy)
		in
		  BIND([t], B.E_Const(Lit.unitLit, BTy.unitTy))
		end
	    | AST.TupleExp es =>
		EXP(trExpsToVs (env, es, fn xs => let
		  val ty = BTy.T_Tuple(false, List.map BV.typeOf xs)
		  val t = BV.new("_tpl", ty)
		  in
		    B.mkStmt([t], B.E_Alloc(ty, xs), B.mkRet [t])
		  end))
	    | AST.RangeExp(lo, hi, optStep, ty) => raise Fail "RangeExp"
	    | AST.PTupleExp exps => raise Fail "PTupleExp"
	    | AST.PArrayExp(exps, ty) => raise Fail "PArrayExp"
	    | AST.PCompExp _ => raise Fail "unexpected PCompExp"
	    | AST.PChoiceExp _ => raise Fail "unexpected PChoiceExp"
	    | AST.SpawnExp e => let
		val (exh, env') = newHandler env
		val e' = trExpToExp(env', e)
		val thd = BV.new("_thd", BTy.T_Fun([], [BTy.exhTy], [BTy.unitTy]))
		in
		  EXP(B.mkFun([B.FB{f=thd, params=[], exh=[exh], body=e'}],
		    B.mkHLOp(ManticoreOps.spawnOp, [thd], [])))
		end
	    | AST.ConstExp(AST.DConst dc) => raise Fail "DConst"
	    | AST.ConstExp(AST.LConst(lit, ty)) => (case trTy ty
		 of ty' as BTy.T_Tuple(false, [rty as BTy.T_Raw _]) => let
		      val t1 = BV.new("_lit", rty)
		      val t2 = BV.new("_wlit", ty')
		      in
			EXP(B.mkStmts([
			    ([t1], B.E_Const(lit, rty)),
			    ([t2], B.wrap t1)
			  ], B.mkRet [t2]))
		      end
		  | ty' => let
		      val t = BV.new("_lit", ty')
		      in
			BIND([t], B.E_Const(lit, ty'))
		      end
		(* end case *))
	    | AST.VarExp(x, _) => EXP(trVtoV(env, x, fn x' => B.mkRet[x']))
	    | AST.SeqExp _ => let
	      (* note: the typechecker puts sequences in right-recursive form *)
		fun tr (AST.SeqExp(e1, e2)) = (
		      case trExp(env, e1)
		       of BIND([], rhs) => B.mkStmt([], rhs, tr e2)
			| EXP e1' => B.mkLet([], e1', tr e2)
		      (* end case *))
		  | tr e = trExpToExp(env, e)
		in
		  EXP(tr exp)
		end
	    | AST.OverloadExp _ => raise Fail "unresolved overloading"
	  (* end case *))

    and trExpToExp (env, exp) = toExp(trExp(env, exp))

    and trBind (env, bind, k : env -> B.exp) = (case bind
	   of AST.ValBind(AST.TuplePat pats, exp) => let
		val (env', xs) = trVarPats (env, pats)
		fun sel (t, i, x::xs, p::ps) =
		      B.mkStmt([x], B.E_Select(i, t), sel(t, i+1, xs, ps))
		  | sel (t, _, [], []) = k env'
		in
		  case trExp(env, exp)
		   of BIND([y], rhs) => B.mkStmt([y], rhs, sel(y, 0, xs, pats))
		    | EXP e => let
			val ty = BTy.T_Tuple(false, List.map BV.typeOf xs)
			val t = BV.new("_tpl", ty)
			in
			  B.mkLet([t], e, sel(t, 0, xs, pats))
			end
		  (* end case *)
		end
	    | AST.ValBind(AST.VarPat x, exp) => (case trExp(env, exp)
		   of BIND([x'], rhs) =>
			B.mkStmt([x'], rhs, k(insert(env, x, x')))
		    | EXP e => let
			val (x', env') = trVar(env, x)
			in
			  B.mkLet([x'], trExpToExp(env, exp), k env')
			end
		(* end case *))
	    | AST.ValBind _ => raise Fail "unexpected complex pattern"
	    | AST.PValBind _ => raise Fail "PValBind"
	    | AST.FunBind fbs => let
		fun bindFun (AST.FB(f, x, e), (env, fs)) = let
		      val (f', env) = trVar(env, f)
		      in
			(env, (f', x, e) :: fs)
		      end
		val (env, fs) = List.foldr bindFun (env, []) fbs
		fun trFun (f', x, e) = let
		      val (x', env) = trVar(env, x)
		      val (exh', env) = newHandler env
		      val e' = trExpToExp (env, e)
		      in
			B.FB{f = f', params = [x'], exh = [exh'], body = e'}
		      end
		in
		  B.mkFun(List.map trFun fs, k env)
		end
	  (* end case *))

    and trCase (env, arg, rules) = let
	(* translate the type of a literal; if it is wrapped, then replace the wrapped
	 * type with a raw type.
	 *)
	  fun trLitTy ty = (case BV.typeOf arg
		 of BTy.T_Tuple(false, [ty as BTy.T_Raw _]) => ty
		  | ty => ty
		(* end case *))
	  fun trRules ([(pat, exp)], cases) = (case pat (* last rule *)
		 of AST.ConPat(dc, tyArgs, p) => raise Fail "ConPat"
		  | AST.TuplePat[] =>
		      ((B.P_Const B.unitConst, trExpToExp (env, exp))::cases, NONE)
		  | AST.TuplePat ps => let
		      val (env, xs) = trVarPats (env, ps)
		      fun bind (_, []) = trExpToExp (env, exp)
			| bind (i, x::xs) = B.mkStmt([x], B.E_Select(i, arg), bind(i+1, xs))
		      in
			(cases, SOME(bind(0, xs)))
		      end
		  | AST.VarPat x =>
		    (* default case: map x to the argument *)
		      (cases, SOME(trExpToExp(insert(env, x, arg), exp)))
		  | AST.ConstPat(AST.DConst(dc, tyArgs)) => raise Fail "DConst"
		  | AST.ConstPat(AST.LConst(lit, ty)) =>
		      ((B.P_Const(lit, trLitTy ty), trExpToExp (env, exp))::cases, NONE)
		(* end case *))
	    | trRules ((pat, exp)::rules, cases) = let
		val rule' = (case pat
		       of AST.ConPat(dc, tyArgs, p) => raise Fail "ConPat"
			| AST.ConstPat(AST.DConst(dc, tyArgs)) => raise Fail "DConst"
			| AST.ConstPat(AST.LConst(lit, ty)) =>
			    (B.P_Const(lit, trLitTy ty), trExpToExp (env, exp))
			| _ => raise Fail "exhaustive pattern in case"
		      (* end case *))
		in
		  trRules (rules, rule'::cases)
		end
	  fun mkCase arg (cases, dflt) = B.mkCase(arg, List.rev cases, dflt)
	  in
	    case BV.typeOf arg
	     of BTy.T_Tuple(false, [ty as BTy.T_Raw rty]) => let
		  val ty = BTy.T_Raw rty
		  val arg' = BV.new("_raw", ty)
		  in
		    B.mkStmt([arg'], B.unwrap arg, mkCase arg' (trRules (rules, [])))
		  end
	      | _ => mkCase arg (trRules (rules, []))
	  end

    and trVarPats (env, pats) = let
	  fun tr ([], env, xs) = (env, List.rev xs)
	    | tr (AST.VarPat x::pats, env, xs) = let
		val (x', env') = trVar(env, x)
		in
		  tr (pats, env', x'::xs)
		end
	    | tr _ = raise Fail "expected VarPat"
	  in
	    tr (pats, env, [])
	  end

  (* translate the expression exp to
   *
   *	let t = exp in cxt[t]
   *)
    and trExpToV (env, AST.VarExp(x, tys), cxt : B.var -> B.exp) =
	  trVtoV (env, x, cxt)
      | trExpToV (env, exp, cxt : B.var -> B.exp) = (case trExp(env, exp)
	   of BIND([x], rhs) => B.mkStmt([x], rhs, cxt x)
	    | EXP e => let
		val t = BV.new ("_t", trTy(TypeOf.exp exp))
		in
		  B.mkLet([t], e, cxt t)
		end
	  (* end case *))

    and trVtoV (env, x, cxt : B.var -> B.exp) = (
	  case find(env, x)
	   of SOME x' => cxt x' (* pass x' directly to the context *)
	    | NONE => let
		val lambda as B.FB{f, ...} = StdEnv.lookupVar x
		in
		  B.mkFun([lambda], cxt f)
		end
	  (* end case *))

  (* translate a list of expressions to a BOM let binding *)
    and trExpsToVs (env, exps, cxt : B.var list -> B.exp) : B.exp = let
	  fun tr ([], xs) = cxt(List.rev xs)
	    | tr (exp::exps, xs) =
		trExpToV (env, exp, fn x => tr(exps, x::xs))
	  in
	    tr (exps, [])
	  end

    fun translate exp = let
	  val exh = BV.new("_topExh", BTy.exhTy)
	  val mainFun = B.FB{
		  f = BV.new("main", BTy.T_Fun([], [BTy.exhTy], [trTy(TypeOf.exp exp)])),
		  params = [],
		  exh = [exh],
		  body = trExpToExp(mkEnv exh, exp)
		}
	  in
	    B.mkModule(Atom.atom "Main", [], mainFun)
	  end

    val translate =
       BasicControl.mkPass
       {preOutput = PrintAST.output,
        preExt = "ast",
        postOutput = PrintBOM.output,
        postExt = "bom",
        passName = "translate",
        pass = translate,
        registry = TranslateControls.registry}

    (**** tests ****)

    local

	structure U = TestUtils
	val (tup, ptup) = (U.tup, U.ptup)      
	val (int, fact, isZero) = (U.int, U.fact, U.isZero)

        (* testModule : AST.module -> BOM.module *)
	fun testModule a =
	    let val aflat = FlatParTup.flattenModule a
		val afut = FutParTup.futurize aflat
		val b = translate afut
		fun sep () = PrintAST.printComment "-->"
	    in
		PrintAST.print a;
		sep ();
		PrintAST.print aflat;
		sep ();
		PrintAST.print afut;
		sep ();
		PrintBOM.print b
	    end

	(* t0 = (| 10, 11 |) *)
	val t0 = ptup [int 10, int 11]

    in

        (* test : int -> unit *)
        val test =
	    let val testCases = [t0]
	    in
		U.mkTest testModule testCases
	    end

    end (* local *)

  end
