(* typechecker.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure Typechecker : sig

  (* check a compilation unit *)
    val check : (Error.err_stream * ParseTree.program) -> AST.exp

  (* check multiple compilation units *)
    val check' : (Error.err_stream * ParseTree.program) list -> AST.exp

  end = struct

    structure PT = ParseTree
    structure U = Unify
    structure TU = TypeUtil
    structure B = Basis
    structure Ty = Types

    val atos = Atom.toString
    fun qidToString path = Path.toString (Atom.toString, path)

  (* FIXME: the following is a hack to avoid threading the error stream through
   * all of the typechecking code.  Eventually, we should fix this, since otherwise
   * it is a space leak.
   *)
    val errStrm = ref(Error.mkErrStream "<bogus>")

    fun error (span, msg) = Error.errorAt (!errStrm, span, msg)

  (* "smart" tuple type constructor *)
    val mkTupleTy = TU.tupleTy

  (* create a single-parameter lambda from one that has a pattern parameter *)
    val mkLambda = ASTUtil.mkFunWithPat

  (* a type expression for when there is an error *)
    val bogusTy = AST.ErrorTy

  (* an expression/type pair for when there is an error *)
    val bogusExp = (AST.TupleExp[], bogusTy)

  (* a pattern for when there is an error *)
    val bogusPat = AST.TuplePat[]

  (* a utility *)
    fun unzip3 tups = let
      fun u ([], xs, ys, zs) = (xs, ys, zs)
	| u ((x,y,z)::t, xs, ys, zs) = u (t, x::xs, y::ys, z::zs)
      in
        u (List.rev tups, [], [], [])
      end
   
  (* typecheck type expressions as described in Section 6.4 *)
    fun chkTy (loc, env, tve, ty) = (case ty
	   of PT.MarkTy{span, tree} => chkTy(span, env, tve, tree)
	    | PT.NamedTy(tyArgs, id) => let
		val tyArgs' = List.map (fn ty => chkTy(loc, env, tve, ty)) tyArgs
		in
		  case Path.findTy(env, id)
		   of SOME(Env.TyDef(AST.TyScheme(tvs, ty))) =>
			if (List.length tvs <> List.length tyArgs')
			  then (
			    error(loc, ["arity mismatch for ", qidToString id]);
			    bogusTy)
			  else TU.substitute (ty, ListPair.zip(tvs, tyArgs'))
		    | SOME(Env.TyCon tyc') =>
			if (TyCon.arityOf tyc' <> List.length tyArgs')
			  then (
			    error(loc, ["arity mismatch for ", qidToString id]);
			    bogusTy)
			  else AST.ConTy(tyArgs', tyc')
		    | NONE => (
			error(loc, ["undefined type constructor ", qidToString id]);
			bogusTy)
		  (* end case *)
		end
	    | PT.VarTy tv => (case Env.find(tve, tv)
		 of SOME tv' => AST.VarTy tv'
		  | NONE => (error(loc, ["unbound type variable ", atos tv]); bogusTy)
		(* end case *))
	    | PT.TupleTy tys =>
		mkTupleTy(List.map (fn ty => chkTy(loc, env, tve, ty)) tys)
	    | PT.FunTy(ty1, ty2) =>
		AST.FunTy(chkTy(loc, env, tve, ty1), chkTy(loc, env, tve, ty2))
	  (* end case *))

  (* typecheck a literal *)
    fun chkLit (_, PT.IntLit i) = let
	  val ty = TypeClass.new Ty.Int
	  in
	    Overload.addLit (ty, Basis.IntClass);
	    (AST.LConst(Literal.Int i, ty), ty)
	  end
      | chkLit (_, PT.FltLit f) = let
	  val ty = TypeClass.new Ty.Float
	  in
	    Overload.addLit (ty, Basis.FloatClass);
	    (AST.LConst(Literal.Float f, ty), ty)
	  end
      | chkLit (_, PT.StrLit s) = (AST.LConst(Literal.String s, Basis.stringTy), Basis.stringTy)

  (* typecheck value declarations as described in Section 6.6 *)
    fun chkValDcl (loc, depth, env, decl) = (case decl
	   of PT.MarkVDecl{span, tree} => chkValDcl (span, depth, env, tree)
	    | PT.ValVDecl(pat, e) => let
		val (pat', env', lhsTy) = chkPat(loc, depth, env, pat)
		val (e', rhsTy) = chkExp (loc, depth, env, e)
		in
		  if not(U.unify(lhsTy, rhsTy))
		    then error (loc, [
	              	"type mismatch in val binding:\n\
                        \  lhs: ", TypeUtil.toString lhsTy, "\n\
                        \  rhs: ", TypeUtil.toString rhsTy, ".\n"
		      ])
		    else ();
		  (AST.ValBind(pat', e'), env')
		end
	    | PT.PValVDecl(pat, e) => let
		val (pat', env', lhsTy) = chkPat(loc, depth, env, pat)
		val (e', rhsTy) = chkExp (loc, depth, env, e)
		in
		  if not(U.unify(lhsTy, rhsTy))
		    then error (loc, ["type mismatch in pval binding"])
		    else ();
		  (AST.PValBind(pat', e'), env')
		end
	    | PT.FunVDecl fbs => let
		val depth' = depth+1
	      (* create variable bindings for the functions *)
		fun bindFun (fb, (fs, names)) = (case fb
		       of PT.MarkFunct{span, tree} => bindFun (tree, (fs, names))
			| PT.Funct(f, _, _) => let
			    val f' = Var.new(Atom.toString f,
				  AST.FunTy(
				    AST.MetaTy(MetaVar.new depth'),
				    AST.MetaTy(MetaVar.new depth')))
			    in
			      if AtomSet.member(names, f)
				then (
				  error(loc, [
				      "duplicate name ", Atom.toString f,
				      "in function binding"
				    ]);
				  ((f, f')::fs, names))
				else ((f, f')::fs, AtomSet.add(names, f))
			    end
		      (* end case *))
		val (fs, _) = List.foldr bindFun ([], AtomSet.empty) fbs
	      (* insert the function variables into an environment for checking
	       * the function bodies.
	       *)
	        val env' = List.foldl
		      (fn ((f, f'), env) => Env.insertVarEnv(env, f, Env.Var f'))
			env fs
	      (* typecheck the functions *)
		fun chkFun loc (fb, fbs) = (case fb
		       of PT.MarkFunct{span, tree} => chkFun span (tree, fbs)
			| PT.Funct(f, param, body) => let
			    val SOME(Env.Var f') = Env.findVarEnv(env', f)
			    val AST.TyScheme(_, funTy) = Var.typeOf f'
			    val (param', env'', paramTy) = chkPat (loc, depth', env', param)
			    val (body', bodyTy) = chkExp (loc, depth', env'', body)
			    in
			      if not(U.unify(funTy, AST.FunTy(paramTy, bodyTy)))
				then error(loc, ["type mismatch in function ", Atom.toString f])
				else ();
			      mkLambda(f', param', body') :: fbs
			    end
		      (* end case *))
		val fbs' = List.foldr (chkFun loc) [] fbs
	      (* close over the types of the functions and build an environment
	       * for checking the scope of the declaration.
	       *)
		fun close ((f, f'), env) = (
		      Var.closeTypeOf (depth, f');
		      Env.insertVarEnv(env, f, Env.Var f'))
	        val env' = List.foldl close env fs
		in
		  (AST.FunBind fbs', env')
		end
	  (* end case *))

  (* typecheck expressions as described in Section 6.8 *)
    and chkExp (loc, depth, env, exp) = (case exp
	   of PT.MarkExp{span, tree} => chkExp (span, depth, env, tree)
	    | PT.LetExp(valDcls, exp) => let
		  fun chkDcls ([], env) = chkExp (loc, depth, env, exp)
		    | chkDcls (vd::vds, env) = let
			  val (bind, env) = chkValDcl (loc, depth, env, vd)
			  val (e', ty) = chkDcls (vds, env)
		      in
		          (AST.LetExp(bind, e'), ty)
		      end
	      in
		  chkDcls (valDcls, env)
	      end
	    | PT.IfExp(e1, e2, e3) => let
		val (e1', ty1) = chkExp (loc, depth, env, e1)
		val (e2', ty2) = chkExp (loc, depth, env, e2)
		val (e3', ty3) = chkExp (loc, depth, env, e3)
		in
		  if not(U.unify(ty1, Basis.boolTy))
		    then error(loc, ["type of conditional not bool"])
		    else ();
		  if not(U.unify(ty2, ty3))
		    then (
		      error(loc, ["types of then and else clauses must match"]);
		      bogusExp)
		    else (AST.IfExp(e1', e2', e3', ty2), ty2)
		end
	    | PT.CaseExp(e, cases) => let
		val (e', argTy) = chkExp (loc, depth, env, e)
		val resTy = AST.MetaTy(MetaVar.new depth)
		val matches = List.map
		      (fn m => chkMatch(loc, depth, env, argTy, resTy, m))
			cases
		in
		  (AST.CaseExp(e', matches, resTy), resTy)
		end
	    | PT.PCaseExp(es, pms) => let
                val (es', tys) = let
                  fun chk e = chkExp (loc, depth, env, e)
                  in
                    ListPair.unzip (List.map chk es)
		  end
		val resTy = AST.MetaTy(MetaVar.new depth)
		val pms' = let
                  fun chk m = chkPMatch(loc, depth, env, tys, resTy, m)
                  in
                    List.map chk pms
                  end
                in
                  (AST.PCaseExp(es', pms', resTy), resTy)
                end
	    | PT.HandleExp(e, cases) => let
		val (e', resTy) = chkExp (loc, depth, env, e)
		val matches = List.map
		      (fn m => chkMatch(loc, depth, env, Basis.exnTy, resTy, m))
			cases
		in
		  (AST.HandleExp(e', matches, resTy), resTy)
		end
	    | PT.RaiseExp e => let
		val (e', ty) = chkExp(loc, depth, env, e)
		val resTy = AST.MetaTy(MetaVar.new depth)
		in
		  if not(U.unify(ty, Basis.exnTy))
		    then error(loc, ["argument of raise must be an exception"])
		    else ();
		  (AST.RaiseExp(e', resTy), resTy)
		end
	    | PT.PChoiceExp es => let
		fun chk (e, (es, ty)) = let
		      val (e', ty') = chkExp(loc, depth, env, e)
		      in
			if not(U.unify (ty, ty'))
			  then error(loc, ["type mismatch in parallel choice"])
			  else ();
			(e'::es, ty')
		      end
		val (es', ty) = List.foldr chk ([], Ty.MetaTy (MetaVar.new depth)) es
		in
		  (AST.PChoiceExp(es', ty), ty)
		end
	    | PT.OrElseExp(e1, e2) => let
		  val (e1', ty1) = chkExp (loc, depth, env, e1)
		  val (e2', ty2) = chkExp (loc, depth, env, e2)
	      in
		  if not(U.unify(ty1, Basis.boolTy) andalso U.unify(ty2, Basis.boolTy))
		  then error(loc, ["arguments of orelse must have type bool"])
		  else ();
		  (AST.IfExp(e1', AST.ConstExp(AST.DConst(Basis.boolTrue, [])), e2', Basis.boolTy), Basis.boolTy)
	      end
	    | PT.AndAlsoExp(e1, e2) => let
		  val (e1', ty1) = chkExp (loc, depth, env, e1)
		  val (e2', ty2) = chkExp (loc, depth, env, e2)
	      in
		  if not(U.unify(ty1, Basis.boolTy) andalso U.unify(ty2, Basis.boolTy))
		  then error(loc, ["arguments of andalso must have type bool"])
		  else ();
		  (AST.IfExp(e1', e2', AST.ConstExp(AST.DConst(Basis.boolFalse, [])), Basis.boolTy), Basis.boolTy)
	      end
	    | PT.BinaryExp(e1, bop, e2) => let
		val (e1', ty1) = chkExp (loc, depth, env, e1)
		val (e2', ty2) = chkExp (loc, depth, env, e2)
		fun mkApp (arg, resTy) = (AST.ApplyExp(arg, AST.TupleExp[e1', e2'], resTy), resTy)
		fun chkApp tyScheme = let
		      val (argTys, instTy as AST.FunTy(argTy, resTy)) = TU.instantiate (depth, tyScheme)
		      in
			if not(U.unify(argTy, AST.TupleTy[ty1, ty2]))
			  then error(loc, [
			      "type mismatch for operator ", Atom.toString bop, "\n",
			      "operator expects ", TypeUtil.toString argTy, "\n",
			      "argument has type ", TypeUtil.toString(AST.TupleTy[ty1, ty2])
			    ])
			  else ();
			(argTys, resTy, instTy)
		      end
		in
		  case BasisEnv.lookupOp bop
		   of Env.Con dc => let
			val (argTys, resTy, _) = chkApp (DataCon.typeOf dc)
			in
			  mkApp (AST.ConstExp(AST.DConst(dc, argTys)), resTy)
			end
		    | Env.Var x => let
			val (argTys, resTy, _) = chkApp (Var.typeOf x)
			in
			  mkApp (AST.VarExp(x, argTys), resTy)
			end
		    | Env.Overload(tysch, vars) => let
			val (argTys, resTy, instTy) = chkApp tysch
			val ovar = ref (AST.Unknown(instTy, vars))
			in
			  Overload.addVar ovar;
			  mkApp (AST.OverloadExp ovar, resTy)
			end
		    | Env.EqOp eqOp =>  let
			val ([ty], resTy, _) = chkApp (Var.typeOf eqOp)
			in
			  Overload.addEqTy ty;
			  mkApp (AST.VarExp(eqOp, [ty]), resTy)
			end
		  (* end case *)
		end
	    | PT.ApplyExp(e1, e2) => let
		val (e1', ty1) = chkExp (loc, depth, env, e1)
		val (e2', ty2) = chkExp (loc, depth, env, e2)
		val resTy = AST.MetaTy(MetaVar.new depth)
		in
		  if not(U.unify(ty1, AST.FunTy(ty2, resTy)))
		    then error(loc, ["type mismatch in application\n",
				     "* expected ", TypeUtil.toString ty1, "\n",
				     "* found    ", TypeUtil.toString ty2])
		    else ();
		  (AST.ApplyExp(e1', e2', resTy), resTy)
		end
	    | PT.ConstExp const => let
		val (const', ty) = chkLit (loc, const)
		in
		  (AST.ConstExp const', ty)
		end
	    | PT.TupleExp es => let
		fun chk (e, (es, tys)) = let
		      val (e', ty) = chkExp(loc, depth, env, e)
		      in
			(e'::es, ty::tys)
		      end
		val (es', tys) = List.foldr chk ([], []) es
		in
		  (AST.TupleExp es', mkTupleTy tys)
		end
	    | PT.ListExp es => let
		val elemTy = AST.MetaTy(MetaVar.new depth)
		val listTy = Ty.ConTy([elemTy], Basis.listTyc)
		fun chk (e, (es, tys)) = let
		      val (e', ty) = chkExp(loc, depth, env, e)
		      in
			if not(U.unify(elemTy, ty))
			  then error(loc, [
			      "type mismatch in list expression\n",
			      "* expected ", TypeUtil.toString elemTy, "\n",
			      "* found    ", TypeUtil.toString ty])
			  else ();
			(e'::es, ty::tys)
		      end
		val (es', tys) = List.foldr chk ([], []) es
		fun cons (e1, e2) = AST.ApplyExp(
			AST.ConstExp(AST.DConst(Basis.listCons, [elemTy])),
			AST.TupleExp[e1, e2],
			listTy)
		val exp = List.foldr cons
		      (AST.ConstExp(AST.DConst(Basis.listNil, [elemTy])))
			es'
		in
		  (exp, listTy)
		end
	    | PT.RangeExp (e1, e2, eo) => let
		  val (e1', ty1) = chkExp (loc, depth, env, e1)
		  val (e2', ty2) = chkExp (loc, depth, env, e2)
		  val _ = if not(U.unify (ty1, ty2))
			  then error (loc, ["type mismatch in range"])
			  else ()
		  val eo' = (case eo of
				 (SOME exp) => let
				     val (exp', ty) = chkExp (loc, depth, env, exp)
				 in
				     if not(U.unify (ty, ty1))
				     then error (loc, ["type mismatch in range"])
				     else ();
				     SOME exp'
				 end
			       | NONE => NONE
			    (* end case *))
	      in
		  if not(U.unify (ty1, TypeClass.new Types.Num))
		  then error (loc, ["range elements must have numeric type"])
		  else ();
		  (AST.RangeExp(e1', e2', eo', ty1), B.parrayTy ty1)
	      end
	    | PT.PTupleExp es => let
		  fun chk (e, (es, tys)) = let
		      val (e', ty) = chkExp(loc, depth, env, e)
		  in
		      (e'::es, ty::tys)
		  end
		  val (es', tys) = List.foldr chk ([], []) es
	      in
		  (AST.PTupleExp es', mkTupleTy tys)
	      end
	    | PT.PArrayExp es => let
		fun chk (e, (es, ty)) =
		    let
			val (e', ty') = chkExp(loc, depth, env, e)
		    in
			if not(U.unify (ty, ty'))
			then error(loc, ["type mismatch in parray"])
			else ();
			(e'::es, ty')
		    end
		val (es', ty) = List.foldr chk ([], Ty.MetaTy (MetaVar.new depth)) es
	      in
		  (AST.PArrayExp(es', ty), B.parrayTy ty)
	      end
	    | PT.PCompExp (e, pbs, eo) => let
		val (pes, env') = chkPBinds (loc, depth, env, pbs)
		val (e', resTy) = chkExp (loc, depth, env', e)
		val eo' = (case eo
			 of (SOME exp) => let
			      val (exp', ty) = chkExp (loc, depth, env', exp)
			      in
				if not(U.unify (ty, B.boolTy))
				  then error (loc, ["type mismatch in parray comprehension 'where' clause"])
				  else ();
				SOME exp'
			      end
			  | NONE => NONE
			(* end case *))
		in
		  (AST.PCompExp (e', pes, eo'), B.parrayTy resTy)
		end
	    | PT.SpawnExp e => let
		val (e', ty) = chkExp (loc, depth, env, e)
		in
		  if not(U.unify (ty, B.unitTy))
		    then error(loc, ["type mismatch in spawn"])
		    else ();
		  (AST.SpawnExp e', B.threadIdTy)
		end
	    | PT.SeqExp es => let
		fun chk [e] = chkExp(loc, depth, env, e)
		  | chk (e::r) = let
		      val (e', _) = chkExp (loc, depth, env, e)
		      val (e'', ty) = chk r
		      in
			(AST.SeqExp(e', e''), ty)
		      end
		in
		  chk es
		end
	    | PT.IdExp x => if Option.isSome (Path.unqualId x) andalso 
			       Atom.same (Option.valOf(Path.unqualId x), BasisNames.uMinus)
		then let
		(* Unary minus is being handled specially as
		 * an overloaded variable *)
		  val (tysch, vars) = BasisEnv.neg
		  val (_, instTy) = TU.instantiate (depth, tysch)
		  val ovar = ref (AST.Unknown (instTy, vars))
		  in
		    Overload.addVar ovar;
		    (AST.OverloadExp ovar, instTy)
		  end
		else (case Path.findVar(env, x)
		   of SOME(Env.Con dc) => let
			val (argTys, ty) = TU.instantiate (depth, DataCon.typeOf dc)
			in
			  (AST.ConstExp(AST.DConst(dc, argTys)), ty)
			end
		    | SOME(Env.Var x') => let
			val (argTys, ty) = TU.instantiate (depth, Var.typeOf x')
			in
			  (AST.VarExp(x', argTys), ty)
			end
		    | NONE => (
			error(loc, ["undefined identifier \"", qidToString x, "\""]);
			bogusExp)
		  (* end case *))
	    | PT.ConstraintExp(e, ty) => let
		val constraintTy = chkTy (loc, env, Env.empty, ty)
		val (e', ty') = chkExp (loc, depth, env, e)
		in
		   if not(U.unify(ty', constraintTy))
		     then error(loc, ["type mismatch in constraint pattern"])
		     else ();
		  (e', ty')
		end
	  (* end case *))

    and chkMatch (loc, depth, env, argTy, resTy, match) = (case match
	   of PT.MarkMatch{span, tree} => chkMatch(span, depth, env, argTy, resTy, tree)
	    | PT.Match(pat, exp) => let
		val (pat', env', argTy') = chkPat(loc, depth, env, pat)
		val (exp', resTy') = chkExp(loc, depth, env', exp)
		in
		  if not(U.unify(argTy, argTy'))
		    then error(loc, ["type mismatch in case pattern"])
		    else ();
		  if not(U.unify(resTy, resTy'))
		    then error(loc, ["type mismatch in case"])
		    else ();
		  AST.PatMatch(pat', exp')
		end
	  (* end case *))

    and chkPMatch (loc, depth, env, argTys, resTy, pmatch) = (case pmatch
          of PT.MarkPMatch{span, tree} => chkPMatch(span, depth, env, argTys, resTy, tree)
	   | PT.PMatch (ps, e) => let
	       fun chkPPats ([], ps', env, argTys) = (List.rev ps', env, List.rev argTys)
		 | chkPPats (p::ps, ps', env, argTys) = let
                     val (p', env', t') = chkPPat(loc, depth, env, p)
                     in
                       chkPPats(ps, p'::ps', env', t'::argTys)
		     end
               val (ps', env', argTys') = chkPPats(ps, [], env, [])
               val (e', resTy') = chkExp(loc, depth, env', e)
               fun u (argTy, argTy') = if not(U.unify(argTy, argTy'))
                                         then error(loc, ["type mismatch in pcase pattern"])
                                         else ();
               in
		 ListPair.app u (argTys, argTys');
                 if not(U.unify(resTy, resTy'))
                   then error(loc, ["type mismatch in pcase"])
                   else ();
                 AST.PMatch (ps', e')
               end
	   | PT.Otherwise e => let
               val (e', resTy') = chkExp(loc, depth, env, e)
               in
                 if not(U.unify(resTy, resTy'))
                   then error(loc, ["type mismatch in pcase"])
                   else ();
                 AST.Otherwise e'
               end
         (* end case *))

    and chkPBinds (loc, depth, env, pbs) = (case pbs
	   of [] => ([], env)
	    | pb::pbs => let
		val (pe, env1 as Env.ModEnv{varEnv=ve1, ...}) = chkPBind (loc, depth, env, pb)
		val (pes, env2  as Env.ModEnv{varEnv=ve2, ...}) = chkPBinds (loc, depth, env, pbs)
(* FIXME: the following code doesn't work when "pb" contains a shadowing definition *)
		val newve1 = List.filter (fn x => not (Env.inDomainVarEnv (env, x))) (AtomMap.listKeys ve1)
		in
		  if List.exists (fn x => Env.inDomainVarEnv(env2, x)) newve1
		    then error (loc, ["conflicting pattern bindings in parray comprehension"])
		    else ();
		  (pe::pes, Env.union(env1, env2))
		end
	  (* end case *))

    and chkPBind (loc, depth, env, pb) = (case pb
	   of PT.MarkPBind{span, tree} => chkPBind(span, depth, env, tree)
	    | PT.PBind (pat, exp) => let
		val (exp', resTy) = chkExp(loc, depth, env, exp)
		val (pat', env', resTy') = chkPat (loc, depth, env, pat)
		in
		 if not(U.unify(resTy, B.parrayTy resTy'))
		   then error(loc, ["type mismatch in pattern binding"])
		   else ();
		 ((pat', exp'), env')
		end
	(* end case *))

    and chkPPat (loc, depth, env, p) : (AST.ppat * Env.module_env * AST.ty) = (case p
          of PT.MarkPPat{span, tree} => chkPPat(span, depth, env, tree)
	   | PT.NDWildPat => let
               val ty = AST.MetaTy(MetaVar.new depth)
               in
                 (AST.NDWildPat ty, env, ty)
	       end
	   | PT.HandlePat p => raise Fail "todo: chkPPat HandlePat" (* FIXME *)
	   | PT.Pat p => let
               val (p', env', ty') = chkPat (loc, depth, env, p)
               in
                 (AST.Pat p', env', ty')
               end            
        (* end case *))
 
    and chkPat (loc, depth, env, pat) = (case pat
	   of PT.MarkPat{span, tree} => chkPat(span, depth, env, tree)
	    | PT.ConPat(conid, pat) => let
		val (pat, env', ty) = chkPat (loc, depth, env, pat)
		in
		  case Path.findVar(env, conid)
		   of SOME(Env.Con dc) => (case TU.instantiate (depth, DataCon.typeOf dc)
			 of (tyArgs, AST.FunTy(argTy, resTy)) => (
			      if not(U.unify(argTy, ty))
				then error(loc, ["type mismatch in constructor pattern"])
				else ();
			      (AST.ConPat(dc, tyArgs, pat), env', resTy))
			  | _ => (
			      error(loc, [
				  "application of nullary constructor ",
				  qidToString conid
				]);
			      (bogusPat, env', bogusTy))
			(* end case *))
		    | _ => (
			error(loc, ["unbound data constructor ", qidToString conid]);
			(bogusPat, env', bogusTy))
		  (* end case *)
		end
	    | PT.BinaryPat(p1, conid, p2) => chkPat (loc, depth, env, PT.ConPat (conid, PT.TuplePat [p1, p2]))
	    | PT.TuplePat pats => let
		val (pats, env', ty) = chkPats (loc, depth, env, pats)
		in
		  (AST.TuplePat pats, env', ty)
		end
	    | PT.ConstPat const => let
		val (const', ty) = chkLit (loc, const)
		in
		  (AST.ConstPat const', env, ty)
		end
	    | PT.WildPat => let
		val ty = AST.MetaTy(MetaVar.new depth)
		in
		  (AST.WildPat ty, env, ty)
		end
	    | PT.IdPat x => (case Env.findVarEnv(env, x)
		 of SOME(Env.Con dc) => (case DataCon.argTypeOf dc
		       of NONE => let
			    val (tyArgs, ty) = TU.instantiate (depth, DataCon.typeOf dc)
			    in
			      (AST.ConstPat(AST.DConst(dc, tyArgs)), env, ty)
			    end
			| _ => (
			    error(loc, [
				"data constructor ", Atom.toString x, " in variable pattern"
			      ]);
			    (bogusPat, env, bogusTy))
		      (* end case *))
		  | _ => let
		      val ty = AST.MetaTy(MetaVar.new depth)
		      val x' = Var.new(Atom.toString x, ty)
		      in
			(AST.VarPat x', Env.insertVarEnv(env, x, Env.Var x'), ty)
		      end
		(* end case *))
	    | PT.ConstraintPat(p, ty) => let
		val constraintTy = chkTy (loc, env, Env.empty, ty)
		val (p', env, ty') = chkPat (loc, depth, env, p)
		in
		   if not(U.unify(ty', constraintTy))
		     then error(loc, ["type mismatch in constraint pattern"])
		     else ();
		  (p', env, ty')
		end
	  (* end case *))

    and chkPats (loc, depth, env, pats : PT.pat list) = let
	  fun chk (pat, (env, ps, tys)) = let
		val (pat', env', ty) = chkPat (loc, depth, env, pat)
		in
		  (env', pat'::ps, ty::tys)
		end
	  val (env', pats', tys) = List.foldr chk (env, [], []) pats
	  in
	    case tys
	     of [ty] => (pats', env', ty)
	      | _ => (pats', env', mkTupleTy tys)
	    (* end case *)
	  end

  (* check a list of type variables *)
    fun chkTyVars (loc, tvs) = let
	  fun chk ([], tve, tvs) = (tve, List.rev tvs)
	    | chk (tv::rest, tve, tvs) = let
		val tv' = TyVar.new tv
		in
		  if Env.inDomain(tve, tv)
		    then (
		      error (loc, ["duplicate type variable ", Atom.toString tv]);
		      chk (rest, tve, tv'::tvs))
		    else chk (rest, Env.insert(tve, tv, tv'), tv'::tvs)
		end
	  in
	    chk (tvs, Env.empty, [])
	  end

  (* create an environment *)
    fun freshEnv (modRef, outerEnv) = Env.freshEnv(modRef, Env.empty, Env.empty, outerEnv)

  (* check type declarations *)
    fun chkTyDcl loc (ptTyDecl, env) = (case ptTyDecl
           of PT.MarkTyDecl {span, tree} => chkTyDcl loc (tree, env)
	    | PT.TypeTyDecl(tvs, id, ty) => let
		val (tve, tvs') = chkTyVars (loc, tvs)
		val ty' = chkTy (loc, env, tve, ty)
		in
		  Env.insertTyEnv(env, id, Env.TyDef(AST.TyScheme(tvs', ty')))
		end
	    | PT.AbsTyDecl (tvs, id) => let
                val (tve, tvs') = chkTyVars(loc, tvs)
		val tyc = TyCon.newAbsTyc(id, List.length tvs', false)
		val env' = Env.insertTyEnv(env, id, Env.TyCon tyc)
                in
		  env'
                end
	    | PT.DataTyDecl(tvs, id, cons) => let
		val (tve, tvs') = chkTyVars (loc, tvs)
		val tyc = TyCon.newDataTyc(id, tvs')
	      (* update the type environment before checking the constructors so that
	       * recursive types work.
	      *)
		val env = Env.insertTyEnv(env, id, Env.TyCon tyc)
		val newCon = DataCon.new tyc
		fun chkCons (_, ids, [], env, cons) = (env, List.rev cons)
		  | chkCons (loc', ids, con::rest, env, cons) = (case con
		       of PT.MarkConDecl{span, tree} =>
			    chkCons (span, ids, tree::rest, env, cons)
			| PT.ConDecl(conid, optTy) =>
			    if AtomSet.member(ids, conid)
			      then (
				error (loc', [
				    "duplicate constructor ", Atom.toString conid,
				    " in datatype ", Atom.toString id
				  ]);
				chkCons (loc, ids, rest, env, cons))
			      else let
				val optTy' = Option.map
				      (fn ty => chkTy(loc, env, tve, ty)) optTy
				val con' = newCon(conid, optTy')
				in
				  chkCons (loc,
				    AtomSet.add(ids, conid), rest,
				    Env.insertVarEnv (env, conid, Env.Con con'), con'::cons)
				end
		      (* end case *))
		val (env', cons') = chkCons (loc, AtomSet.empty, cons, env, [])
	       (* datatypes that have only nullary constructors are equality types *)
		val isEqTy = List.all (fn (Ty.DCon{argTy=NONE, ...}) => true | _ => false) cons'
		in
		  if isEqTy then TyCon.markEqTyc tyc else ();
		  env'
		end
          (* end case *))

    fun chkSpec loc (spec, env) = (case spec
        of PT.MarkSpec {tree, span} => chkSpec span (tree, env)
	 | PT.TypeSpec tyDecl => chkTyDcl loc (tyDecl, env)
	 | PT.ValSpec (x, tvs, ty) => let
	   val (tve, tvs') = chkTyVars (loc, tvs)
           val ty = chkTy(loc, env, tve, ty)
	   val x' = Var.newPoly(Atom.toString x, Ty.TyScheme(tvs', ty))
           in
	       Env.insertVarEnv(env, x, Env.Var x')
           end
        (* end case *))

    fun chkSpecs loc (specs, env) = List.foldl (chkSpec loc) env specs

  (* check that a signature is well formed and return the resulting environment *)
    fun chkSignature loc (id, sign, env) = (case sign
            of PT.MarkSig {tree, span} => chkSignature span (id, tree, env)
	     | PT.ExpSig specs => let
	       val modRef = AST.MOD{name=Option.getOpt(id, Atom.atom "sign"), id=Stamp.new(), formals=NONE}
               val sigEnv = Env.freshEnv(modRef, Env.empty, Env.empty, SOME env)
               in
		  chkSpecs loc (specs, sigEnv)
               end
             | PT.NameSig (id, tyRevls) => (case Env.findSigEnv(env, id)
               of NONE => (error (loc, ["cannot find signature ", Atom.toString id]);
			   env)
		| SOME sigEnv => let
	           val modRef = AST.MOD{name=id, id=Stamp.new(), formals=NONE}
		   val env' = Env.freshEnv(modRef, Env.empty, Env.empty, SOME env)
                   val env' as Env.ModEnv{tyEnv, ...} = List.foldl (chkTyDcl loc) env' tyRevls
                   in
		      MatchSig.reveal (sigEnv, tyEnv)
		   end
               (* end case *))
            (* end case *))

    fun pairEnvs (modEnv, residualEnv) = let
	fun f (id, xr, ps) = (case Env.Map.find(modEnv, id)
            of NONE => ps
	     | SOME xm => (xm, xr) :: ps
            (* end case *))
        in
	   Env.Map.foldli f [] residualEnv
        end

    fun getVars ((Env.Var x1, Env.Var x2), ps) = (x1, x2) :: ps
      | getVars (_, ps) = ps

  (* build a mapping from variable definitions in the module's var environment to
   * fresh variable definitions in the residual signature.
   *)
    fun buildVarSubst (modVarEnv, residualVarEnv) = let
	val valBindSubstPairs = pairEnvs (modVarEnv, residualVarEnv)
	val varBindSubstPairs = List.foldl getVars [] valBindSubstPairs
        in 
	   List.foldl VarSubst.add VarSubst.id varBindSubstPairs
        end

    fun chkModule loc (id, sign, module, (env, moduleEnv, astDecls)) = (case module
        of PT.MarkMod {span, tree} => chkModule span (id, sign, tree, (env, moduleEnv, astDecls))
	 | PT.DeclsMod decls => let
		val modRef = AST.MOD {name=id, id=Stamp.new(), formals=NONE}
		val (modEnv, moduleEnv, modAstDecls) = chkTopDcls(loc, decls, freshEnv(modRef, SOME env), moduleEnv)
		val (modEnv', modAstDecls') = (case sign
                    of SOME sign => let
                       val sigEnv = chkSignature loc (NONE, sign, env)
		       val env = MatchSig.match{err=(!errStrm), loc=loc, modEnv=modEnv, sigEnv=sigEnv}		       
		       val s = buildVarSubst (Env.varEnv modEnv, Env.varEnv env)
		       val modAstDecls' = VarSubst.topDecs s modAstDecls
                       in
			   (env, modAstDecls')
                       end
		     | NONE => (modEnv, modAstDecls)
                   (* end case *))
		val body = AST.M_Body(loc, modAstDecls')
                in
		  (Env.insertModEnv(env, id, modEnv'), 
		   Env.ModuleEnv.insert(moduleEnv, modRef, (modEnv, modEnv', body)),
		   AST.TD_Module(loc, modRef, NONE, body) :: astDecls)
	        end
	 | PT.NamedMod modName => (case Path.findMod(env, modName)
               of NONE => (error(loc, ["cannot find module ", qidToString modName]);
			   (env, moduleEnv, astDecls))
		| SOME modEnv => let
		      val modRefN = AST.MOD {name=id, id=Stamp.new(), formals=NONE}
		      val signEnv = (case sign
                          of SOME sign => let
				 val sigEnv = chkSignature loc (NONE, sign, env)
			     in
				 MatchSig.match{err=(!errStrm), loc=loc, modEnv=modEnv, sigEnv=sigEnv}
			     end
			   | NONE => modEnv
                         (* end case *))
		      val body = AST.M_Id(loc, modRefN)
		      in
	                 (Env.insertModEnv(env, id, signEnv),
			  Env.ModuleEnv.insert(moduleEnv, modRefN, (modEnv, signEnv, body)),
			  AST.TD_Module(loc, modRefN, NONE, body) :: astDecls)
                      end 
                 (* end case *))
       (* end case *))

    and chkTopDcl loc (ptDecl, (env, moduleEnv, astDecls)) = (case ptDecl
           of PT.MarkDecl{span, tree} => chkTopDcl span (tree, (env, moduleEnv, astDecls))
	    | PT.TyDecl tyDecl => (chkTyDcl loc (tyDecl, env), moduleEnv, astDecls)
	    | PT.ExnDecl(id, optTy) => let
		val optTy' = Option.map (fn ty => chkTy(loc, env, Env.empty, ty)) optTy
		val exnCon = Exn.new (id, optTy')
		in
		  (Env.insertVarEnv (env, id, Env.Con exnCon), moduleEnv, astDecls)
		end
	    | PT.ValueDecl valDcl => let
		val (bind, env) = chkValDcl(loc, 0, env, valDcl)
		in
		  (env, moduleEnv, AST.TD_Binding bind :: astDecls)
		end
	    | PT.ModuleDecl (id, sign, module) => chkModule loc (id, sign, module, (env, moduleEnv, astDecls))
	    | PT.LocalDecl (localDcls, dcls) => raise Fail "LocalDecl"
	    | PT.SignDecl (id, sign) => let
              val sigEnv = chkSignature loc (SOME id, sign, env)
              in
                 (Env.insertSigEnv(env, id, sigEnv), moduleEnv, astDecls)
              end
	  (* end case *))

    and chkTopDcls (loc, ptDecls, env, moduleEnv) = let
        val (env', moduleEnv, astDecls) = List.foldl (chkTopDcl loc) (env, moduleEnv, []) ptDecls
        in
	   (env', moduleEnv, List.rev astDecls)
        end

    fun bindSigIdVar (vSig, vMod, binds) =
	AST.ValBind(AST.VarPat vSig, ASTUtil.mkVarExp(vMod, [])) :: binds

    fun bindSigIdVars (sigVars, modVars, exp) = 
	ASTUtil.mkLetExp(ListPair.foldl bindSigIdVar [] (sigVars, modVars), exp)

    fun rebindSigVars (sigEnv, modEnv, exp) = let
	val sigVarEnv = Env.varEnv sigEnv
	val modVarEnv = Env.varEnv modEnv
	fun f (id, Env.Var sigVar, (sigVars, modVars)) = (case Env.Map.find(modVarEnv, id)
            of SOME (Env.Var modVar) => (sigVar :: sigVars, modVar :: modVars)
	     | _ => (sigVars, modVars)
            (* end case *))
	  | f (_, _, (sigVars, modVars)) = (sigVars, modVars)
        val (sigVars, modVars) = Env.Map.foldli f ([], []) sigVarEnv
	in
	    bindSigIdVars (sigVars, modVars, exp)
	end

  (* convert top-level declarations into expressions *)
    fun declsToExp moduleEnv (decls, exp) = List.foldl (declToExp moduleEnv) exp (List.rev decls)

  (* convert a top-level declaration into an expression *)
    and declToExp moduleEnv (decl, exp) = (case decl
	   of AST.TD_Module(info, modRef, sign, module) => 
	      moduleToExp moduleEnv (modRef, module, exp)
	    | AST.TD_DCon dcon => exp
	    | AST.TD_Binding binding => ASTUtil.mkLetExp([binding], exp)
	  (* end case *))   

  (* convert a module into an expression *)
    and moduleToExp moduleEnv (modRef, module, exp) = (case module
	   of AST.M_Id (info, modRef' as AST.MOD{name, ...}) => 
	      (case Env.ModuleEnv.find(moduleEnv, modRef)
		of NONE => raise Fail ("cannot find module "^Atom.toString name)
		 | SOME (modEnv as Env.ModEnv{modRef, ...}, sigEnv, module) => 
		   rebindSigVars(sigEnv, modEnv, exp)
              (* end case *))
	    | AST.M_Body (info, decls) => declsToExp moduleEnv (decls, exp)
	  (* end case *))

  (* call the entry-point function for the program *)
    fun makeEntryPoint (env, entryPoint) = (
	  case Env.findVarEnv(env, Atom.atom entryPoint)
	   of SOME (Env.Var entryPoint) => 
	      (* FIXME: pass command-line args *)
		AST.ApplyExp(AST.VarExp(entryPoint, []), AST.TupleExp[AST.TupleExp[], AST.TupleExp[]], Ty.TupleTy[Ty.TupleTy[], Ty.TupleTy[]])
	    | _ => raise Fail ("error: could not find entry point "^entryPoint)
	  (* end case *))

    fun check'' (es, env, moduleEnv, {span, tree=ptDecls}) = let
	  val _ = errStrm := es
	  val (env, moduleEnv, astDecls) = chkTopDcls (span, ptDecls, env, moduleEnv)
	  in
	    Overload.resolve ();
	    (env, moduleEnv, astDecls)
	  end

  (* check multiple compilation units *)
    fun check' programs = let
	val dummyModRef = AST.MOD{name=Atom.atom "dummy", id=Stamp.new(), formals=NONE}
	(* typecheck the compilation units individually *)
	  val env0 = Env.freshEnv(dummyModRef, BasisEnv.te0, BasisEnv.ve0, NONE)
	  fun f ((err, program), (env, moduleEnv, declss)) = let
		val (env', moduleEnv', decls) = check''(err, env, moduleEnv, program)
		in
		  (env', moduleEnv', decls :: declss)
		end
	  val (env, moduleEnv, declss) = List.foldl f (env0, Env.ModuleEnv.empty, []) programs
        (* flatten the top-level declarations of the compilation units *)
	  val decls = List.concat (List.rev declss)
	  in
	  (* convert the compilation units into a single AST expression *)
	    declsToExp moduleEnv (decls, makeEntryPoint(env, "main"))
	  end

  (* check a compilation unit *)
    fun check (err, program) = let
	val dummyModRef = AST.MOD{name=Atom.atom "dummy", id=Stamp.new(), formals=NONE}
	val env0 = Env.freshEnv(dummyModRef, BasisEnv.te0, BasisEnv.ve0, NONE)
	val (env, moduleEnv, decls) = check'' (err, env0, Env.ModuleEnv.empty, program)
	in
	    declsToExp moduleEnv (decls, makeEntryPoint(env, "main"))
	end

    val check = BasicControl.mkTracePassSimple {passName = "check", pass = check}

    val check' = BasicControl.mkTracePassSimple {passName = "check", pass = check'}

  end

