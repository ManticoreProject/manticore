(* typechecker.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure Typechecker : sig

    val check : Error.err_stream * ParseTree.program -> AST.module option

  end = struct

    structure PT = ParseTree
    structure E = Env
    structure U = Unify
    structure TU = TypeUtil
    structure B = Basis
    structure Ty = Types

    val atos = Atom.toString

  (* FIXME: the following is a hack to avoid threading the error stream through
   * all of the typechecking code.  Eventually, we should fix this, since otherwise
   * it is a space leak.
   *)
    val errStrm = ref(Error.mkErrStream "<bogus>")

    fun error (span, msg) = Error.errorAt (!errStrm, span, msg)

  (* "smart" tuple type constructor *)
    fun mkTupleTy [ty] = ty
      | mkTupleTy tys = AST.TupleTy tys

  (* create a single-parameter lambda from one that has a list of parameters *)
    fun mkLambda (f, AST.VarPat x, e) = AST.FB(f, x, e)
      | mkLambda (f, AST.TuplePat[], e) = let
	  val arg = Var.new ("arg", Basis.unitTy)
	  in
	    AST.FB(f, arg, e)
	  end
      | mkLambda (f, pat, e) = let
	  val AST.TyScheme(_, AST.FunTy(argTy, resTy)) = Var.typeOf f
	  val arg = Var.new ("arg", argTy)
	  in
	    AST.FB(f, arg, AST.CaseExp(AST.VarExp(arg, []), [(pat, e)], resTy))
	  end

  (* a type expression for when there is an error *)
    val bogusTy = AST.ErrorTy

  (* an expression/type pair for when there is an error *)
    val bogusExp = (AST.TupleExp[], bogusTy)

  (* a pattern for when there is an error *)
    val bogusPat = AST.TuplePat[]

  (* typecheck type expressions as described in Section 6.4 *)
    fun chkTy (loc, te, tve, ty) = (case ty
	   of PT.MarkTy{span, tree} => chkTy(span, te, tve, tree)
	    | PT.NamedTy(tyArgs, id) => let
		val tyArgs' = List.map (fn ty => chkTy(loc, te, tve, ty)) tyArgs
		in
		  case Env.find(te, id)
		   of SOME(E.TyDef(AST.TyScheme(tvs, ty))) =>
			if (List.length tvs <> List.length tyArgs')
			  then (
			    error(loc, ["arity mismatch for ", atos id]);
			    bogusTy)
			  else TU.substitute (ty, ListPair.zip(tvs, tyArgs'))
		    | SOME(E.TyCon tyc') =>
			if (TyCon.arityOf tyc' <> List.length tyArgs')
			  then (
			    error(loc, ["arity mismatch for ", atos id]);
			    bogusTy)
			  else AST.ConTy(tyArgs', tyc')
		    | NONE => (
			error(loc, ["undefined type constructor ", atos id]);
			bogusTy)
		  (* end case *)
		end
	    | PT.VarTy tv => (case Env.find(tve, tv)
		 of SOME tv' => AST.VarTy tv'
		  | NONE => (error(loc, ["unbound type variable ", atos tv]); bogusTy)
		(* end case *))
	    | PT.TupleTy tys =>
		mkTupleTy(List.map (fn ty => chkTy(loc, te, tve, ty)) tys)
	    | PT.FunTy(ty1, ty2) =>
		AST.FunTy(chkTy(loc, te, tve, ty1), chkTy(loc, te, tve, ty2))
	  (* end case *))

  (* typecheck a literal *)
    fun chkLit (_, PT.IntLit i) = let
	  val ty = TypeClass.new Ty.Int
	  val rc = ref (ty, Basis.IntClass)
	  in
	    Overload.add_lit rc;
	    (AST.LConst(Literal.Int i, ty), ty)
	  end
      | chkLit (_, PT.FltLit f) = let
	  val ty = TypeClass.new Ty.Float
	  val rc = ref (ty, Basis.FloatClass)
	  in
	    Overload.add_lit rc;
	    (AST.LConst(Literal.Float f, ty), ty)
	  end
      | chkLit (_, PT.StrLit s) = (AST.LConst(Literal.String s, Basis.stringTy), Basis.stringTy)

  (* typecheck value declarations as described in Section 6.6 *)
    fun chkValDcl (loc, depth, te, ve, decl) = (case decl
	   of PT.MarkVDecl{span, tree} => chkValDcl (span, depth, te, ve, tree)
	    | PT.ValVDecl(pat, e) => let
		val (pat', ve', lhsTy) = chkPat(loc, depth, te, ve, pat)
		val (e', rhsTy) = chkExp (loc, depth, te, ve, e)
		in
		  if not(U.unify(lhsTy, rhsTy))
		    then error (loc, ["type mismatch in val binding"])
		    else ();
		  (AST.ValBind(pat', e'), ve')
		end
	    | PT.PValVDecl(pat, e) => let
		val (pat', ve', lhsTy) = chkPat(loc, depth, te, ve, pat)
		val (e', rhsTy) = chkExp (loc, depth, te, ve, e)
		in
		  if not(U.unify(lhsTy, rhsTy))
		    then error (loc, ["type mismatch in pval binding"])
		    else ();
		  (AST.PValBind(pat', e'), ve')
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
	        val ve' = List.foldl
		      (fn ((f, f'), ve) => E.insert(ve, f, E.Var f'))
			ve fs
	      (* typecheck the functions *)
		fun chkFun loc (fb, fbs) = (case fb
		       of PT.MarkFunct{span, tree} => chkFun span (tree, fbs)
			| PT.Funct(f, param, body) => let
			    val SOME(E.Var f') = E.find(ve', f)
			    val AST.TyScheme(_, funTy) = Var.typeOf f'
			    val (param', ve'', paramTy) = chkPat (loc, depth', te, ve', param)
			    val (body', bodyTy) = chkExp (loc, depth', te, ve'', body)
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
		fun close ((f, f'), ve) = (
		      Var.closeTypeOf (depth, f');
		      E.insert(ve, f, E.Var f'))
	        val ve' = List.foldl close ve fs
		in
		  (AST.FunBind fbs', ve')
		end
	  (* end case *))

  (* typecheck expressions as described in Section 6.8 *)
    and chkExp (loc, depth, te, ve, exp) = (case exp
	   of PT.MarkExp{span, tree} => chkExp (span, depth, te, ve, tree)
	    | PT.LetExp(valDcls, exp) => let
		  fun chkDcls ([], ve) = chkExp (loc, depth, te, ve, exp)
		    | chkDcls (vd::vds, ve) = let
			  val (bind, ve) = chkValDcl (loc, depth, te, ve, vd)
			  val (e', ty) = chkDcls (vds, ve)
		      in
		          (AST.LetExp(bind, e'), ty)
		      end
	      in
		  chkDcls (valDcls, ve)
	      end
	    | PT.IfExp(e1, e2, e3) => let
		  val (e1', ty1) = chkExp (loc, depth, te, ve, e1)
		  val (e2', ty2) = chkExp (loc, depth, te, ve, e2)
		  val (e3', ty3) = chkExp (loc, depth, te, ve, e3)
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
		  val (e', argTy) = chkExp (loc, depth, te, ve, e)
		  val resTy = AST.MetaTy(MetaVar.new depth)
		  val matches = List.map
				    (fn m => chkMatch(loc, depth, te, ve, argTy, resTy, m))
				    cases
	      in
		  (AST.CaseExp(e', matches, resTy), resTy)
	      end
	    | PT.PChoiceExp es => let
		fun chk (e, (es, ty)) = let
		      val (e', ty') = chkExp(loc, depth, te, ve, e)
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
		  val (e1', ty1) = chkExp (loc, depth, te, ve, e1)
		  val (e2', ty2) = chkExp (loc, depth, te, ve, e2)
	      in
		  if not(U.unify(ty1, Basis.boolTy) andalso U.unify(ty2, Basis.boolTy))
		  then error(loc, ["arguments of orelse must have type bool"])
		  else ();
		  (AST.IfExp(e1', AST.ConstExp(AST.DConst(Basis.boolTrue, [])), e2', Basis.boolTy), Basis.boolTy)
	      end
	    | PT.AndAlsoExp(e1, e2) => let
		  val (e1', ty1) = chkExp (loc, depth, te, ve, e1)
		  val (e2', ty2) = chkExp (loc, depth, te, ve, e2)
	      in
		  if not(U.unify(ty1, Basis.boolTy) andalso U.unify(ty2, Basis.boolTy))
		  then error(loc, ["arguments of andalso must have type bool"])
		  else ();
		  (AST.IfExp(e1', e2', AST.ConstExp(AST.DConst(Basis.boolFalse, [])), Basis.boolTy), Basis.boolTy)
	      end
	    | PT.BinaryExp(e1, bop, e2) => let
		val (e1', ty1) = chkExp (loc, depth, te, ve, e1)
		val (e2', ty2) = chkExp (loc, depth, te, ve, e2)
		fun mkApp (arg, resTy) = AST.ApplyExp(arg, AST.TupleExp[e1', e2'], resTy)
		fun chkApp tyScheme = let
		      val (argTys, instTy as AST.FunTy(argTy, resTy)) = TU.instantiate (depth, tyScheme)
		      in
			if not(U.unify(argTy, AST.TupleTy[ty1, ty2]))
			  then error(loc, [
			      "type mismatch for operator ", Atom.toString bop,
			      ", argument has type ", TypeUtil.toString(AST.TupleTy[ty1, ty2])
			    ])
			  else ();
			(argTys, resTy, instTy)
		      end
		in
		  if Atom.same(bop, BasisNames.listCons)
		    then let
		      val dc = Basis.listCons
		      val (argTys, resTy, _) = chkApp (DataCon.typeOf dc)
		      in
			 (mkApp (AST.ConstExp(AST.DConst(dc, argTys)), resTy), resTy)
		     end
		    else let
		      val (tysch, vars) = Basis.lookupOp(bop)
		      val (argTys, resTy, instTy) = chkApp tysch
		      val ovar = ref (AST.Unknown (instTy, vars))
		      in
			Overload.add_var ovar;
			(mkApp (AST.OverloadExp ovar, resTy), resTy)
		      end
		end
	    | PT.ApplyExp(e1, e2) => let
		  val (e1', ty1) = chkExp (loc, depth, te, ve, e1)
		  val (e2', ty2) = chkExp (loc, depth, te, ve, e2)
		  val resTy = AST.MetaTy(MetaVar.new depth)
	      in
		  if not(U.unify(ty1, AST.FunTy(ty2, resTy)))
		  then error(loc, ["type mismatch in application"])
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
		      val (e', ty) = chkExp(loc, depth, te, ve, e)
		  in
		      (e'::es, ty::tys)
		  end
		  val (es', tys) = List.foldr chk ([], []) es
	      in
		  (AST.TupleExp es', mkTupleTy tys)
	      end
	    | PT.RangeExp (e1, e2, eo) => let
		  val (e1', ty1) = chkExp (loc, depth, te, ve, e1)
		  val (e2', ty2) = chkExp (loc, depth, te, ve, e2)
		  val _ = if not(U.unify (ty1, ty2))
			  then error (loc, ["type mismatch in range"])
			  else ()
		  val eo' = (case eo of
				 (SOME exp) => let
				     val (exp', ty) = chkExp (loc, depth, te, ve, exp)
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
		      val (e', ty) = chkExp(loc, depth, te, ve, e)
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
			val (e', ty') = chkExp(loc, depth, te, ve, e)
		    in
			if not(U.unify (ty, ty'))
			then error(loc, ["type mismatch in parray"])
			else ();
			(e'::es, ty')
		    end
		val (es', ty) = List.foldr chk ([], Ty.MetaTy (MetaVar.new depth)) es
	      in
		  (AST.PArrayExp(es', ty), ty)
	      end
	    | PT.PCompExp (e, pbs, eo) => let
		  val (pes, ve') = chkPBinds (loc, depth, te, ve, pbs)
		  val (e', resTy) = chkExp (loc, depth, te, ve', e)
		  val eo' = (case eo of
				 (SOME exp) => let
				      val (exp', ty) = chkExp (loc, depth, te, ve', exp)
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
		  val (e', ty) = chkExp (loc, depth, te, ve, e)
	      in
		if not(U.unify (ty, B.unitTy))
		  then error(loc, ["type mismatch in spawn"])
		  else ();
		(AST.SpawnExp e', B.threadIdTy)
	      end
	    | PT.SeqExp es => let
		  fun chk [e] = chkExp(loc, depth, te, ve, e)
		    | chk (e::r) = let
			  val (e', _) = chkExp (loc, depth, te, ve, e)
			  val (e'', ty) = chk r
		      in
			  (AST.SeqExp(e', e''), ty)
		      end
	      in
		  chk es
	      end
	    | PT.IdExp x =>
	      if Atom.same (x, BasisNames.uMinus)
	      then
		  (* Unary minus is being handled specially as
		   * an overloaded variable *)
		  let
		      val (tysch, vars) = B.neg
		      val (_, instTy) = TU.instantiate (depth, tysch)
		      val ovar = ref (AST.Unknown (instTy, vars))
		  in
		      (Overload.add_var ovar;
		       (AST.OverloadExp ovar, instTy))
		  end
	      else
		  (case E.find(ve, x)
		    of SOME(E.Con dc) => let
			   val (argTys, ty) = TU.instantiate (depth, DataCon.typeOf dc)
		       in
			   (AST.ConstExp(AST.DConst(dc, argTys)), ty)
		       end
		     | SOME(E.Var x') => let
			   val (argTys, ty) = TU.instantiate (depth, Var.typeOf x')
		       in
			   (AST.VarExp(x', argTys), ty)
		       end
		     | NONE => (
		       error(loc, ["undefined identifier \"", Atom.toString x, "\""]);
		       bogusExp)
		  (* end case *))
	    | PT.ConstraintExp(e, ty) => let
		val constraintTy = chkTy (loc, te, E.empty, ty)
		val (e', ty') = chkExp (loc, depth, te, ve, e)
		in
		   if not(U.unify(ty', constraintTy))
		     then error(loc, ["type mismatch in constraint pattern"])
		     else ();
		  (e', ty')
		end
	  (* end case *))

    and chkMatch (loc, depth, te, ve, argTy, resTy, match) = (case match
	   of PT.MarkMatch{span, tree} => chkMatch(span, depth, te, ve, argTy, resTy, tree)
	    | PT.Match(pat, exp) => let
		val (pat', ve', argTy') = chkPat(loc, depth, te, ve, pat)
		val (exp', resTy') = chkExp(loc, depth, te, ve', exp)
		in
		  if not(U.unify(argTy, argTy'))
		    then error(loc, ["type mismatch in case pattern"])
		    else ();
		  if not(U.unify(resTy, resTy'))
		    then error(loc, ["type mismatch in case"])
		    else ();
		  (pat', exp')
		end
	  (* end case *))

    and chkPBinds (loc, depth, te, ve, pbs) =
	(case pbs of
	     [] => ([], ve)
	   | pb::pbs => let
		 val (pe, ve1) = chkPBind (loc, depth, te, ve, pb)
		 val (pes, ve2) = chkPBinds (loc, depth, te, ve, pbs)
		 val newve1 = List.filter (fn x => not (E.inDomain (ve, x)))
					  (AtomMap.listKeys ve1)

		 fun notInVE2 [] = true
		   | notInVE2 (atom::atoms) =
		     if E.inDomain (ve2, atom)
		     then false
		     else notInVE2 atoms
	     in
	       if notInVE2 newve1
		 then ()
		 else error (loc, ["conflicting pattern bindings in parray comprehension"]);
	       (pe::pes, AtomMap.unionWith #1 (ve1, ve2))
	     end
	(* end case *))

    and chkPBind (loc, depth, te, ve, pb) =
	(case pb of
	     PT.MarkPBind{span, tree} => chkPBind(span, depth, te, ve, tree)
	   | PT.PBind (pat, exp) => let
		 val (exp', resTy) = chkExp(loc, depth, te, ve, exp)
		 val (pat', ve', resTy') = chkPat (loc, depth, te, ve, pat)
	     in
		 if not(U.unify(resTy, B.parrayTy resTy'))
		 then error(loc, ["type mismatch in pattern binding"])
		 else ();
		 ((pat', exp'), ve')
	     end
	(* end case *))

    and chkPat (loc, depth, te, ve, pat) = (case pat
	   of PT.MarkPat{span, tree} => chkPat(span, depth, te, ve, tree)
	    | PT.ConPat(conid, pat) => let
		val (pat, ve', ty) = chkPat (loc, depth, te, ve, pat)
		in
		  case E.find(ve, conid)
		   of SOME(E.Con dc) => (case TU.instantiate (depth, DataCon.typeOf dc)
			 of (tyArgs, AST.FunTy(argTy, resTy)) => (
			      if not(U.unify(argTy, ty))
				then error(loc, ["type mismatch in constructor pattern"])
				else ();
			      (AST.ConPat(dc, tyArgs, pat), ve', resTy))
			  | _ => (
			      error(loc, [
				  "application of nullary constructor ",
				  Atom.toString conid
				]);
			      (bogusPat, ve', bogusTy))
			(* end case *))
		    | _ => (
			error(loc, ["unbound data constructor ", Atom.toString conid]);
			(bogusPat, ve', bogusTy))
		  (* end case *)
		end
	    | PT.BinaryPat(p1, conid, p2) => chkPat (loc, depth, te, ve, PT.ConPat (conid, PT.TuplePat [p1, p2]))
	    | PT.TuplePat pats => let
		val (pats, ve', ty) = chkPats (loc, depth, te, ve, pats)
		in
		  (AST.TuplePat pats, ve', ty)
		end
	    | PT.ConstPat const => let
		val (const', ty) = chkLit (loc, const)
		in
		  (AST.ConstPat const', ve, ty)
		end
	    | PT.WildPat => let
		val ty = AST.MetaTy(MetaVar.new depth)
		in
		  (AST.WildPat ty, ve, ty)
		end
	    | PT.IdPat x => (case E.find(ve, x)
		 of SOME(E.Con dc) => (case DataCon.argTypeOf dc
		       of NONE => let
			    val (tyArgs, ty) = TU.instantiate (depth, DataCon.typeOf dc)
			    in
			      (AST.ConstPat(AST.DConst(dc, tyArgs)), ve, ty)
			    end
			| _ => (
			    error(loc, [
				"data constructor ", Atom.toString x, " in variable pattern"
			      ]);
			    (bogusPat, ve, bogusTy))
		      (* end case *))
		  | _ => let
		      val ty = AST.MetaTy(MetaVar.new depth)
		      val x' = Var.new(Atom.toString x, ty)
		      in
			(AST.VarPat x', E.insert(ve, x, E.Var x'), ty)
		      end
		(* end case *))
	    | PT.ConstraintPat(p, ty) => let
		val constraintTy = chkTy (loc, te, E.empty, ty)
		val (p', ve, ty') = chkPat (loc, depth, te, ve, p)
		in
		   if not(U.unify(ty', constraintTy))
		     then error(loc, ["type mismatch in constraint pattern"])
		     else ();
		  (p', ve, ty')
		end
	  (* end case *))

    and chkPats (loc, depth, te, ve, pats : PT.pat list) = let
	  fun chk (pat, (ve, ps, tys)) = let
		val (pat', ve', ty) = chkPat (loc, depth, te, ve, pat)
		in
		  (ve', pat'::ps, ty::tys)
		end
	  val (ve', pats', tys) = List.foldr chk (ve, [], []) pats
	  in
	    case tys
	     of [ty] => (pats', ve', ty)
	      | _ => (pats', ve', mkTupleTy tys)
	    (* end case *)
	  end

  (* check a list of type variables *)
    fun chkTyVars (loc, tvs) = let
	  fun chk ([], tve, tvs) = (tve, List.rev tvs)
	    | chk (tv::rest, tve, tvs) = let
		val tv' = TyVar.new tv
		in
		  if E.inDomain(tve, tv)
		    then (
		      error (loc, ["duplicate type variable ", Atom.toString tv]);
		      chk (rest, tve, tv'::tvs))
		    else chk (rest, E.insert(tve, tv, tv'), tv'::tvs)
		end
	  in
	    chk (tvs, E.empty, [])
	  end

  (* typecheck top-level declarations as described in Sections 6.2 and 6.3 *)
    fun chkTopDcl (loc, te, ve, decl, next) = (case decl
	   of PT.MarkDecl{span, tree} => chkTopDcl (span, te, ve, tree, next)
	    | PT.TyDecl(tvs, id, ty) => let
		val (tve, tvs') = chkTyVars (loc, tvs)
		val ty' = chkTy (loc, te, tve, ty)
		in
		  next(E.insert(te, id, E.TyDef(AST.TyScheme(tvs', ty'))), ve)
		end
	    | PT.DataDecl(tvs, id, cons) => let
		val (tve, tvs') = chkTyVars (loc, tvs)
		val tyc = TyCon.newDataTyc(id, tvs')
	      (* update the type environment before checking the constructors so that
	       * recursive types work.
	      *)
		val te = E.insert(te, id, E.TyCon tyc)
		val newCon = DataCon.new tyc
		fun chkCons (_, ids, [], ve, cons) = (ve, List.rev cons)
		  | chkCons (loc', ids, con::rest, ve, cons) = (case con
		       of PT.MarkConDecl{span, tree} =>
			    chkCons (span, ids, tree::rest, ve, cons)
			| PT.ConDecl(conid, optTy) =>
			    if AtomSet.member(ids, conid)
			      then (
				error (loc', [
				    "duplicate constructor ", Atom.toString conid,
				    " in datatype ", Atom.toString id
				  ]);
				chkCons (loc, ids, rest, ve, cons))
			      else let
				val optTy' = Option.map
				      (fn ty => chkTy(loc, te, tve, ty)) optTy
				val con' = newCon(conid, optTy')
				in
				  chkCons (loc,
				    AtomSet.add(ids, conid), rest,
				    E.insert (ve, conid, E.Con con'), con'::cons)
				end
		      (* end case *))
		val (ve', cons') = chkCons (loc, AtomSet.empty, cons, ve, [])
		in
		  next(te, ve')
		end
	    | PT.ValueDecl valDcl => let
		val (bind, ve) = chkValDcl(loc, 0, te, ve, valDcl)
		val (e, ty) = next(te, ve)
		in
		  (AST.LetExp(bind, e), ty)
		end
	  (* end case *))

    fun check (es, {span, tree=(dcls, exp)}) = let
	  val _ = errStrm := es
	  fun chkDcls (te, ve, []) = chkExp(span, 0, te, ve, exp)
	    | chkDcls (te, ve, d::ds) =
	        chkTopDcl (span, te, ve, d, fn (te, ve) => chkDcls (te, ve, ds))
	  val ret = SOME (#1 (chkDcls (Basis.te0, Basis.ve0, dcls)))
	  in
	    print "resolve overloading\n";
	    Overload.resolve ();
	    ret
	  end

  end
