(* translate.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Translate : sig

    val translate : AST.module -> BOM.module

  end = struct

    structure A = AST
    structure V = Var
    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy
    structure Lit = Literal
    structure E = TranslateEnv

    val trTy = TranslateTypes.tr

  (* prune out overload nodes.
   * NOTE: we should probably have a pass that does this before
   * AST optimization.
   *)
    fun prune (AST.OverloadExp(ref(AST.Instance x))) = AST.VarExp(x, [])
      | prune (AST.OverloadExp _) = raise Fail "unresolved overloading"
      | prune e = e

  (* translate a binding occurrence of an AST variable to a BOM variable *)
    fun trVar (env, x) = let
	  val x' = BV.new(V.nameOf x, TranslateTypes.trScheme(env, V.typeOf x))
	  in
	    (x', E.insertVar(env, x, x'))
	  end

    local
      fun mkCasts (x::xs, ty::tys) = let
	    val (xs', casts) = mkCasts (xs, tys)
	    in
	      if BOMTyUtil.match (ty, BV.typeOf x)
		then (x::xs', casts)
		else let
		  val x' = BV.new("_t", ty)
		  in
		    (x'::xs', ([x], B.E_Cast(BV.typeOf x, x'))::casts)
		  end
	    end
	| mkCasts ([], []) = ([], [])
	| mkCasts _ = raise Fail "rhs/lhs arity mismatch"
    in
    fun mkStmt (lhs, rhs, e) = let
	  val (xs, casts) = mkCasts (lhs, BOMUtil.typeOfRHS rhs)
	  in
	    B.mkStmts ((xs, rhs)::casts, e)
	  end

    fun mkLet (lhs, rhs, e) = let
	  val (xs, casts) = mkCasts (lhs, BOMUtil.typeOfExp rhs)
	  in
	    B.mkLet (xs, rhs, B.mkStmts(casts, e))
	  end
    end (* local *)

  (* the yield of translating an AST expression *)
    datatype bom_code
      = BIND of (B.var list * B.rhs)
      | EXP of B.exp

    fun toExp (BIND(xs, rhs)) = mkStmt(xs, rhs, B.mkRet xs)
      | toExp (EXP e) = e

  (* translate datatype constructors and constants in expressions *)
    fun trDConExp (env, dc) : bom_code = (case TranslateTypes.trDataCon(env, dc)
	   of E.Const(w, ty) => let
		val t = BV.new("con_" ^ DataCon.nameOf dc, ty)
		in
		  BIND([t], B.E_Const(Lit.Enum w, ty))
		end
	    | E.DCon dc' => let
		val (exh, env) = E.newHandler env
		val dataTy = BOMTyCon.dconResTy dc'
		val (fb as B.FB{f, ...}) = (case BOMTyCon.dconArgTy dc'
		       of [ty] => let
			  (* constructor with a single argument *)
			    val arg = BV.new("arg", ty)
			    val res = BV.new("data", dataTy)
			    val f = BV.new(BOMTyCon.dconName dc',
				    BTy.T_Fun([ty], [BTy.exhTy], [dataTy]))
			    in
			      B.FB{
				  f = f, params = [arg], exh = [exh],
				  body = B.mkStmt([res], B.E_DCon(dc', [arg]), B.mkRet[res])
				}
			    end
			| tys => let
			  (* constructor with multiple arguments (or zero arguments); the
			   * lambda will take a tuple and deconstruct it to build the data value.
			   *)
			    val argTy = BTy.tupleTy tys
			    val arg = BV.new("arg", argTy)
			    val (tmps, binds) = let
				  fun f (ty, (i, xs, binds)) = let
					val x = BV.new("_t"^Int.toString i, ty)
					val b = ([x], B.E_Select(i, arg))
					in
					  (i+1, x::xs, b::binds)
					end
				  val (_, xs, binds) = List.foldl f (0, [], []) tys
				  in
				    (List.rev xs, List.rev binds)
				  end
			    val res = BV.new("data", dataTy)
			    val f = BV.new(BOMTyCon.dconName dc',
				    BTy.T_Fun([argTy], [BTy.exhTy], [dataTy]))
			    in
			      B.FB{
				  f = f, params = [arg], exh = [exh],
				  body = B.mkStmts(
				    binds @ [([res], B.E_DCon(dc', tmps))],
				    B.mkRet[res])
				}
			    end
		      (* end case *))
		in
		  EXP(B.mkFun([fb], B.mkRet[f]))
		end
	  (* end case *))

    fun trExp (env, exp) : bom_code = (case prune exp
	   of AST.LetExp(b, e) =>
		EXP(trBind (env, b, fn env' => trExpToExp(env', e)))
	    | AST.IfExp(e1, e2, e3, ty) =>
		EXP(trExpToV (env, e1, fn x =>
		  B.mkIf(x, trExpToExp(env, e2), trExpToExp(env, e3))))
	    | AST.CaseExp(e, rules, ty) =>
		EXP(trExpToV (env, e, fn x => trCase(env, x, rules)))
	    | f as AST.FunExp(x, e, ty) => let
		val fty = TypeOf.exp f
		val fvar = Var.new ("f", fty)
		val env' = E.insertVar (env, fvar, BV.new ("f", trTy(env, fty)))
		val fdef = AST.FB(fvar, x, e)
		val letExp = AST.LetExp (AST.FunBind[fdef], AST.VarExp (fvar, []))
		in
		  trExp (env', letExp)
		end
	    | AST.ApplyExp(e1, e2, ty) => 
	        EXP(trExpToV (env, e1, fn f =>
		  trExpToV (env, e2, fn arg =>
		    B.mkApply(f, [arg], [E.handlerOf env]))))
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
		val (exh, env') = E.newHandler env
		val e' = trExpToExp(env', e)
		val param = BV.new("unused", BTy.unitTy)
		val thd = BV.new("_thd", BTy.T_Fun([BTy.unitTy], [BTy.exhTy], [BTy.unitTy]))
		in
		  EXP(B.mkFun([B.FB{f=thd, params=[param], exh=[exh], body=e'}],
(* FIXME: should ManticoreOps be HLOpEnv??? *)
		    B.mkHLOp(ManticoreOps.spawnOp, [thd], [E.handlerOf env])))
		end
	    | AST.ConstExp(AST.DConst(dc, tys)) => trDConExp (env, dc)
	    | AST.ConstExp(AST.LConst(lit as Literal.String s, _)) => let
		val t1 = BV.new("_data", BTy.T_Any)
		val t2 = BV.new("_len", BTy.T_Raw BTy.T_Int)
		in
		  EXP(B.mkStmts([
		      ([t1], B.E_Const(lit, BTy.T_Any)),
		      ([t2], B.E_Const(Literal.Int(IntInf.fromInt(size s)), BTy.T_Raw BTy.T_Int))
		    ], B.mkHLOp(HLOpEnv.stringLitOp, [t1, t2], [])))
		end
	    | AST.ConstExp(AST.LConst(lit, ty)) => (case trTy(env, ty)
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
		       of BIND(lhs, rhs) => B.mkStmt(lhs, rhs, tr e2)
			| EXP e1' => B.mkLet([BV.new("unused", BTy.unitTy)], e1', tr e2)
		      (* end case *))
		  | tr e = trExpToExp(env, e)
		in
		  EXP(tr exp)
		end
	    | AST.OverloadExp _ => raise Fail "unresolved overloading"
	  (* end case *))

    and trExpToExp (env, exp) = toExp(trExp(env, exp))

    and trBind (env, bind, k : TranslateEnv.env -> B.exp) = (case bind
	   of AST.ValBind(AST.TuplePat pats, exp) => let
		val (env', xs) = trVarPats (env, pats)
		fun sel (t, i, x::xs, p::ps) =
		      mkStmt([x], B.E_Select(i, t), sel(t, i+1, xs, ps))
		  | sel (t, _, [], []) = k env'
		in
		  case trExp(env, exp)
		   of BIND([y], rhs) => mkStmt([y], rhs, sel(y, 0, xs, pats))
		    | EXP e => let
			val ty = BTy.T_Tuple(false, List.map BV.typeOf xs)
			val t = BV.new("_tpl", ty)
			in
			  mkLet([t], e, sel(t, 0, xs, pats))
			end
		  (* end case *)
		end
	    | AST.ValBind(AST.VarPat x, exp) => (case trExp(env, exp)
		   of BIND([x'], rhs) =>
			mkStmt([x'], rhs, k(E.insertVar(env, x, x')))
		    | EXP e => let
			val (x', env') = trVar(env, x)
			in
			  mkLet([x'], e, k env')
			end
		(* end case *))
	    | AST.ValBind(AST.WildPat ty, exp) => (case trExp(env, exp)
		   of BIND([x'], rhs) =>
			mkStmt([x'], rhs, k env)
		    | EXP e => let
			val x' = BV.new("_wild_", TranslateTypes.tr(env, ty))
			in
			  mkLet([x'], e, k env)
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
		      val (exh', env) = E.newHandler env
		      val e' = trExpToExp (env, e)
		      in
			B.FB{f = f', params = [x'], exh = [exh'], body = e'}
		      end
		in
		  B.mkFun(List.map trFun fs, k env)
		end
	  (* end case *))

    and trCase (env, arg, rules) = let
	  fun trConPat (dc, tyArgs, pat, exp) = (case TranslateTypes.trDataCon(env, dc)
		 of E.DCon dc' => let
		      val (env, args) = (case pat
			     of AST.TuplePat pats => trVarPats (env, pats)
			      | _ => trVarPats (env, [pat])
			    (* end case *))
		      in
			(B.P_DCon(dc', args), trExpToExp(env, exp))
		      end
		  | _ => raise Fail "unexpected constant"
		(* end case *))
	(* translate the type of a literal; if it is wrapped, then replace the wrapped
	 * type with a raw type.
	 *)
	  fun trLitTy ty = (case BV.typeOf arg
		 of BTy.T_Tuple(false, [ty as BTy.T_Raw _]) => ty
		  | ty => ty
		(* end case *))
	  fun trRules ([AST.PatMatch(pat, exp)], cases) = (case pat (* last rule *)
		 of AST.ConPat(dc, tyArgs, pat) =>
		      (trConPat (dc, tyArgs, pat, exp) :: cases, NONE)
		  | AST.TuplePat[] =>
		      ((B.P_Const B.unitConst, trExpToExp (env, exp))::cases, NONE)
		  | AST.TuplePat ps => let
		      val (env, xs) = trVarPats (env, ps)
		      fun bind (_, []) = trExpToExp (env, exp)
			| bind (i, x::xs) = B.mkStmt([x], B.E_Select(i, arg), bind(i+1, xs))
		      in
			(cases, SOME(bind(0, xs)))
		      end
		  | AST.VarPat x =>(* default case: map x to the argument *)
		      (cases, SOME(trExpToExp(E.insertVar(env, x, arg), exp)))
		  | AST.WildPat ty => (* default case *)
		      (cases, SOME(trExpToExp(env, exp)))
		  | AST.ConstPat(AST.DConst(dc, tyArgs)) => raise Fail "DConst"
		  | AST.ConstPat(AST.LConst(lit, ty)) =>
		      ((B.P_Const(lit, trLitTy ty), trExpToExp (env, exp))::cases, NONE)
		(* end case *))
	    | trRules (AST.PatMatch(pat, exp)::rules, cases) = let
		val rule' = (case pat
		       of AST.ConPat(dc, tyArgs, p) => trConPat (dc, tyArgs, p, exp)
			| AST.ConstPat(AST.DConst(dc, tyArgs)) => raise Fail "DConst"
			| AST.ConstPat(AST.LConst(lit, ty)) =>
			    (B.P_Const(lit, trLitTy ty), trExpToExp (env, exp))
			| _ => raise Fail "exhaustive pattern in case"
		      (* end case *))
		in
		  trRules (rules, rule'::cases)
		end
	    | trRules (AST.CondMatch _ :: _, _) = raise Fail "unexpected CondMatch"
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
	    | tr (AST.VarPat x :: pats, env, xs) = let
		val (x', env') = trVar(env, x)
		in
		  tr (pats, env', x'::xs)
		end
	    | tr (AST.WildPat ty :: pats, env, xs) = let
		val x' = BV.new("_wild_", TranslateTypes.tr(env, ty))
		in
		  tr (pats, env, x'::xs)
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
	   of BIND([x], rhs) => mkStmt([x], rhs, cxt x)
	    | EXP e => let
		val t = BV.new ("_t", trTy(env, TypeOf.exp exp))
		in
		  mkLet([t], e, cxt t)
		end
	  (* end case *))

    and trVtoV (env, x, cxt : B.var -> B.exp) = (
	  case E.lookupVar(env, x)
	   of E.Var x' => cxt x' (* pass x' directly to the context *)
	    | E.Lambda lambda => let
		val lambda as B.FB{f, ...} = BOMUtil.copyLambda lambda
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

  (* wrap the body of the program with code to initialize the scheduler. *)
    fun startup (env, exp) =
	  B.mkLet([], B.mkHLOp(HLOpEnv.defaultSchedulerStartupOp, [], [E.handlerOf env]),
	    exp)

    fun translate exp = let
          val argTy = BTy.T_Raw RawTypes.T_Int
          val arg = BV.new("_arg", argTy)
	  val (exh, env) = E.newHandler StdEnv.env0
	  val mainFun = B.FB{
		  f = BV.new("main", BTy.T_Fun([argTy], [BTy.exhTy], [trTy(env, TypeOf.exp exp)])),
		  params = [arg],
		  exh = [exh],
		  body = startup (env, trExpToExp(env, exp))
		}
	  val module = B.mkModule(Atom.atom "Main", [], mainFun)
	  in
	    Census.census module;
	    module
	  end

    val translate = BasicControl.mkKeepPass {
	    preOutput = PrintAST.output,
	    preExt = "ast",
	    postOutput = PrintBOM.output,
	    postExt = "bom",
	    passName = "translate",
	    pass = translate,
	    registry = TranslateControls.registry
	  }

  end
