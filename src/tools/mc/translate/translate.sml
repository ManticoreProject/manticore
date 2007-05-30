(* translate.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Translate : sig

    val translate : AST.module -> BOM.module

  end = struct

    structure Ty = Types;
    structure V = Var
    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy
    structure Lit = Literal

    datatype env = E of {
	exh : B.var,		(* current exception handler *)
	vmap : B.var V.Map.map	(* map from AST variables to BOM variables *)
      }

    fun mkEnv exh = E{exh = exh, vmap = V.Map.empty}

    fun lookup (E{vmap, ...}, x) = (case V.Map.find(vmap, x)
	   of SOME x' => x'
	    | NONE => raise Fail(concat["lookup(-, ", V.toString x, ")"])
	  (* end case *))

    fun insert (E{vmap, exh}, x, x') =
	  E{vmap=V.Map.insert(vmap, x, x'), exh=exh}

    fun newHandler (E{vmap, ...}) = let
	  val exh = BV.new("_exh", BTy.exhTy)
	  in
	    (exh, E{vmap = vmap, exh = exh})
	  end

    fun handlerOf (E{exh, ...}) = [exh]

  (* translate a binding occurrence of an AST variable to a BOM variable *)
    fun trVar (env, x) = let
	  val AST.TyScheme([], ty) = V.typeOf x
	  val x' = BV.new(V.nameOf x, trTy ty)
	  in
	    (x', insert(env, x, x'))
	  end

  (* the yield of translating an AST expression *)
    datatype bom_code
      = BIND of (B.var list * B.rhs)
      | EXP of B.exp

    fun toExp (BIND(xs, rhs)) = B.mkStmt(xs, rhs, B.mkRet xs)
      | toExp (EXP e) = e

    fun trExp (env, exp) = (case exp
	   of AST.LetExp(b, e) =>
		EXP(trBind (env, b, fn env' => trExpToExp(env', e)))
	    | AST.IfExp(e1, e2, e3) =>
		EXP(trExpToV (env, e1, fn x =>
		  B.mkIf(x, trExpToExp(env, e2), trExpToExp(env, e3))))
	    | AST.CaseExp(e, rules) => raise Fail "case"
	    | AST.ApplyExp(e1, e2, ty) =>
		EXP(trExpToV (env, e1, fn f =>
		  trExpToV (env, e2, fn arg =>
		    B.mkApply(f, [arg], handlerOf env))))
	    | AST.TupleExp[] => let
		val t = BV.new("_unit", BTy.unitTy)
		in
		  BIND([t], B.E_Const(Lit.unitLit, BTy.unitTy))
		end
	    | AST.TupleExp es =>
		EXP(trExpsToVs (env, es, fn xs => let
		  val ty = BTy.T_Tuple(List.map BV.typeOf xs)
		  val t = BV.new("_tpl", ty)
		  in
		    B.mkStmt([t], B.E_Alloc(ty, xs), B.mkRet [t])
		  end))
	    | AST.RangeExp(lo, hi, optStep, ty) =>
	    | AST.PTupleExp exps => raise Fail "PTupleExp"
	    | AST.PArrayExp(exps, ty) => raise Fail "PArrayExp"
	    | AST.ComprehendExp _ => raise Fail "unexpected ComprehendExp"
	    | AST.SpawnExp e => let
		val (exh, env') = newHandler env
		val e' = trExp(env', e)
		val thd = BV.newVar("_thd", BTy.T_Fun([], [BTy.exhTy], []))
		in
		  B.mkFun([B.FB{f=thd, params=[], exh=[exh], body=e'}],
		    B.mkHLOp(ManticoreOps.spawnOp, [f], []))
		end
	    | AST.ConstExp(AST.DConst dc) => raise Fail "DConst"
	    | AST.ConstExp(AST.LConst(lit, ty)) => let
		val ty' = trTy ty
		val t = BV.new("_lit", ty')
		in
		  BIND([t], B.E_Const(lit, ty'))
		end
	    | AST.VarExp(x, []) => EXP(B.mkRet[lookup(env, x)])
	    | AST.VarExp(x, tys) => raise Fail "poly var"
	    | AST.SeqExp _ => let
	      (* note: the typechecker puts sequences in right-recursive form *)
		fun tr (AST.SeqExp(e1, e2)) = (
		      case trExp(env, e1)
		       of BIND([], rhs) => B.mkStmt([], rhs, tr e2)
			| EXP e1' => B.mkLet([], e1', tr e2)
		      (* end case *))
		  | tr e = toExp(trExp(env, e))
		in
		  EXP(tr exp)
		end
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
			val ty = BTy.T_Tuple(List.map BV.typeOf xs)
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
	    | AST.FunBind fbs => raise Fail "fun bind"
	  (* end case *))

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
	(* if exp is a variable, we just pass it directly to the context *)
	  cxt (lookup(env, x))
      | trExpToV (env, exp, cxt : B.var -> B.exp) = (case trExp(env, exp)
	   of BIND([x], rhs) => B.mkStmt([x], rhs, cxt x)
	    | EXP e => let
		val t = BV.new ("_t", ?)
		in
		  B.mkLet([t], e, cxt t)
		end
	  (* end case *))

  (* translate a list of expressions to a BOM let binding *)
    and trExpsToVs (env, exps, cxt : B.var list -> B.exp) = let
	  fun tr ([], xs) = cxt xs
	    | tr (exp::exps, xs) =
		trExpToV (env, exp, fn x => tr(exps, x::xs))
	  in
	    tr (exps, [])
	  end

    fun translate exp = let
	  val exh = BV.new("_topExh", BTy.exhTy)
	  val mainFun = B.FB{
		  f = BV.new("main", BTy.T_Fun([], [BTy.exhTy], [])),
		  params = [],
		  exh = [exh],
		  body = trExpToExp(mkEnv exh, exp)
		}
	  in
	    B.mkModule(Atom.atom "Main", [], mainFun)
	  end

  end
