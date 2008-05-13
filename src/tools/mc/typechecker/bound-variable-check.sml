(* bound-variable-check.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check the program for unbound variables.
 *)

structure BoundVariableCheck :> sig

  (* check for unbound variables *)
    val check : (Error.err_stream * ProgramParseTree.PML1.program) -> ProgramParseTree.PML2.program

  end = struct

    structure PT1 = ProgramParseTree.PML1
    structure PT2 = ProgramParseTree.PML2
    structure Var = ProgramParseTree.Var
    structure BEnv = BindingEnv

    val atos = Atom.toString
    fun qidToString path = QualifiedId.toString (Atom.toString, path)

  (* FIXME: the following is a hack to avoid threading the error stream through
   * all of the typechecking code.  Eventually, we should fix this, since otherwise
   * it is a space leak.
   *)
    val errStrm = ref(Error.mkErrStream "<bogus>")

    fun error (span, msg) = Error.errorAt (!errStrm, span, msg)

    fun chkList loc (chkX, xs, env) = let
	   fun f (x, (xs, env)) = let
	          val (x, env) = chkX loc (x, env)
                  in
	              (x :: xs, env)
	          end
	   val (xs', env) = List.foldl f ([], env) xs
           in
	      (List.rev xs', env)
           end

    fun chkTy loc (ty, env) = (case ty
           of PT1.MarkTy {span, tree} => let
		  val (tree, env) = chkTy span (tree, env)
	          in
		     (PT2.MarkTy {span=span, tree=tree}, env)
		  end
	    | PT1.NamedTy (tys, id) => let
		  val (tys, env) = chkTys loc (tys, env)
	          in
		     case QualifiedId.findTy(env, id)
		      of NONE => (PT2.NamedTy (tys, Var.new("dummy", ())), env)
		       | SOME id' => (PT2.NamedTy (tys, id'), env)
		  end
	    | PT1.VarTy tv => (PT2.VarTy tv, env)
	    | PT1.TupleTy tys => let
		  val (tys, env) = chkTys loc (tys, env)
	          in
		    (PT2.TupleTy tys, env)
		  end
	    | PT1.FunTy (ty1, ty2) => let
		  val (ty1, env) = chkTy loc (ty1, env)
		  val (ty2, env) = chkTy loc (ty2, env)
	          in
		     (PT2.FunTy (ty1, ty2), env)
		  end
           (* end case *))

    and chkTys loc (tys, env) = chkList loc (chkTy, tys, env)

    fun chkPat loc (pat, env) = (case pat
           of PT1.MarkPat {span, tree} => let
		  val (tree, env) = chkPat span (tree, env)
	          in
		     (PT2.MarkPat{span=span, tree=tree}, env)
		  end
	    | PT1.IdPat vb => let
		  val vb' = Var.new(Atom.toString vb, ())
		  val env = BEnv.insertVal(env, vb, vb')
	          in
		     (PT2.IdPat vb', env)
		  end
	    | PT1.WildPat => (PT2.WildPat, env)
           (* end case *))

    fun chkPPat loc (pat, env) = (case pat
           of PT1.MarkPPat {span, tree} => let
		  val (tree, env) = chkPPat span (tree, env)
	          in
		     (PT2.MarkPPat{span=span, tree=tree}, env)
		  end
	    | PT1.NDWildPat => (PT2.NDWildPat, env)
           (* end case *))

    and chkPPats loc (ppats, env) = chkList loc (chkPPat, ppats, env)

    and chkValDecl loc (valDecl, env) = (case valDecl
           of PT1.MarkVDecl {span, tree} => let
		  val (tree, env) = chkValDecl span (tree, env)
	          in
		     (PT2.MarkVDecl{span=span, tree=tree}, env)
		  end
	    | PT1.ValVDecl (pat, exp) => let
		  val (pat, env) = chkPat loc (pat, env)
		  val (exp, env) = chkExp loc (exp, env)
	          in
		     (PT2.ValVDecl(pat, exp), env)
		  end
	    | PT1.PValVDecl (pat, exp) => let
		  val (pat, env) = chkPat loc (pat, env)
		  val (exp, env) = chkExp loc (exp, env)
	          in
		     (PT2.PValVDecl(pat, exp), env)
		  end
	    | PT1.FunVDecl functs => let
(* FIXME: support mutually recursive functions *)
		  fun chkFunct loc (PT1.MarkFunct {span, tree}, env) = let
		          val (tree, env) = chkFunct span (tree, env)
		          in
		            (PT2.MarkFunct{span=span, tree=tree}, env)
		          end
		    | chkFunct loc (PT1.Funct (f, pat, exp), env) = let
			  val f' = Var.new(Atom.toString f, ())
			  val env = BEnv.insertVal(env, f, f')
			  val (pat, env) = chkPat loc (pat, env)
			  val (exp, env) = chkExp loc (exp, env)
			  in
			     (PT2.Funct (f', pat, exp), env)
			  end
		  val (functs, env) = chkList loc (chkFunct, functs, env)
	          in
		     (PT2.FunVDecl functs, env)
		  end
           (* end case *))

    and chkValDecls loc (valDecls, env) = chkList loc (chkValDecl, valDecls, env)

    and chkMatch loc (match, env) = (case match
           of PT1.MarkMatch {span, tree} => let
		  val (tree, env) = chkMatch span (tree, env)
	          in
                    (PT2.MarkMatch{span=span, tree=tree}, env)
	          end
	    | PT1.Match (pat, exp) => let
		  val (pat, env) = chkPat loc (pat, env)
		  val (exp, env) = chkExp loc (exp, env)
	          in
		     (PT2.Match(pat, exp), env)
		  end
           (* end case *))

    and chkMatches loc (matches, env) = chkList loc (chkMatch, matches, env)

    and chkPMatch loc (pmatch, env) = (case pmatch
           of PT1.MarkPMatch {span, tree} => let
		  val (tree, env) = chkPMatch span (tree, env)
	          in
                    (PT2.MarkPMatch{span=span, tree=tree}, env)
	          end
	    | PT1.PMatch (ppats, exp) => let
		  val (ppats, env) = chkPPats loc (ppats, env)
		  val (exp, env) = chkExp loc (exp, env)
	          in
		     (PT2.PMatch(ppats, exp), env)
		  end
	    | PT1.Otherwise exp => let
		  val (exp, env) = chkExp loc (exp, env)
	          in
		     (PT2.Otherwise exp, env)
		  end
           (* end case *))

    and chkPMatches loc (pmatches, env) = chkList loc (chkPMatch, pmatches, env)

    and chkConst (PT1.IntLit x) = PT2.IntLit x
      | chkConst (PT1.FltLit x) = PT2.FltLit x
      | chkConst (PT1.StrLit x) = PT2.StrLit x

    and chkExp loc (exp, env) = (case exp
           of PT1.MarkExp {span, tree} => let
		  val (tree, env) = chkExp span (tree, env)
	          in
                    (PT2.MarkExp{span=span, tree=tree}, env)
	          end
	    | PT1.LetExp(valDecls, exp) => let
		  val (valDecls', env) = chkValDecls loc (valDecls, env)
		  val (exp, env) = chkExp loc (exp, env)
	          in
		     (PT2.LetExp(valDecls', exp), env)
		  end
	    | PT1.IfExp (e1, e2, e3) => let
		  val (e1, env) = chkExp loc (e1, env)
		  val (e2, env) = chkExp loc (e2, env)
		  val (e3, env) = chkExp loc (e3, env)
	          in
		     (PT2.IfExp (e1, e2, e3), env)
		  end
	    | PT1.CaseExp (exp, matches) => let
		  val (exp, env) = chkExp loc (exp, env)
		  val (matches, env) = chkMatches loc (matches, env)
	          in
		     (PT2.CaseExp(exp, matches), env)
		  end
	    | PT1.PCaseExp (exps, pmatches) => let
		  val (exps, env) = chkExps loc (exps, env)
		  val (pmatches, env) = chkPMatches loc (pmatches, env)
	          in
		     (PT2.PCaseExp(exps, pmatches), env)
		  end
	    | PT1.HandleExp (exp, matches) => let
		  val (exp, env) = chkExp loc (exp, env)
		  val (matches, env) = chkMatches loc (matches, env)
	          in
		     (PT2.HandleExp(exp, matches), env)
		  end
	    | PT1.RaiseExp exp => let
		  val (exp, env) = chkExp loc (exp, env)
	          in
		     (PT2.RaiseExp exp, env)
		  end
	    | PT1.AndAlsoExp (exp1, exp2) => let
		  val (exp1, env) = chkExp loc (exp1, env)
		  val (exp2, env) = chkExp loc (exp2, env)
	          in
		     (PT2.AndAlsoExp (exp1, exp2), env)
		  end
	    | PT1.OrElseExp (exp1, exp2) => let
		  val (exp1, env) = chkExp loc (exp1, env)
		  val (exp2, env) = chkExp loc (exp2, env)
	          in
		     (PT2.OrElseExp (exp1, exp2), env)
		  end
	    | PT1.BinaryExp (exp1, id, exp2) => let
		  val (exp1, env) = chkExp loc (exp1, env)
		  val (exp2, env) = chkExp loc (exp2, env)
	          in
		     (PT2.BinaryExp (exp1, BasisEnv.lookupOpPT id, exp2), env)
		  end
	    | PT1.PChoiceExp exps => let
		  val (exps, env) = chkExps loc (exps, env)
	          in
		     (PT2.PChoiceExp exps, env)
		  end
	    | PT1.ApplyExp (exp1, exp2) => let
		  val (exp1, env) = chkExp loc (exp1, env)
		  val (exp2, env) = chkExp loc (exp2, env)
	          in
		     (PT2.ApplyExp(exp1, exp2), env)
		  end
	    | PT1.IdExp qId => (case QualifiedId.findVar(env, qId)
                  of NONE => 
		       (error(loc, ["unbound identifier ", qidToString qId]);
			(PT2.TupleExp [], env))
		   | SOME var => 
		       (PT2.IdExp var, env)
	          (* end case *))
	    | PT1.ConstExp const => (PT2.ConstExp (chkConst const), env)
	    | PT1.TupleExp exps => let
		  val (exps, env) = chkExps loc (exps, env)
	          in
		     (PT2.TupleExp exps, env)
	          end
	    | PT1.ListExp exps => let
		  val (exps, env) = chkExps loc (exps, env)
	          in
		     (PT2.ListExp exps, env)
	          end
	    | PT1.RangeExp (exp1, exp2, expOpt) => let
		  val (exp1, env) = chkExp loc (exp1, env)
		  val (exp2, env) = chkExp loc (exp2, env)
		  val (expOpt, env) = (case expOpt
					of NONE => (NONE, env)
					 | SOME exp => let
					       val (exp, env) = chkExp loc (exp, env)
					       in
					         (SOME exp, env)
					       end
				      (* end case *))
	          in
		     (PT2.RangeExp (exp1, exp2, expOpt), env)
		  end
	    | PT1.SeqExp exps => let
		  val (exps, env) = chkExps loc (exps, env)
	          in
		     (PT2.SeqExp exps, env)
	          end
	    | PT1.ConstraintExp (exp, ty) => let
		  val (exp, env) = chkExp loc (exp, env)
		  val (ty, env) = chkTy loc (ty, env)
	          in
		     (PT2.ConstraintExp (exp, ty), env)
	          end
            (* end case *))

    and chkExps loc (exps, env) = chkList loc (chkExp, exps, env)

    fun chkTyDecl loc (tyDecl, env) = (case tyDecl
           of PT1.MarkTyDecl {span, tree} => let
		  val (tree, env) = chkTyDecl span (tree, env)
	          in
                    (PT2.MarkTyDecl{span=span, tree=tree}, env)
	          end
	    | PT1.TypeTyDecl (tvs, id, ty) => let
		  val (ty, env) = chkTy loc (ty, env)
		  val id' = Var.new(Atom.toString id, ())
		  val env = BEnv.insertTy(env, id, id')
	          in
		     (PT2.TypeTyDecl (tvs, id', ty), env)
		  end
	    | PT1.DataTyDecl (tvs, id, conDecls) => let
		  val (conDecls, env) = chkConDecls loc (conDecls, env)
		  val id' = Var.new(Atom.toString id, ())
		  val env = BEnv.insertDataTy(env, id, id')
	          in
		     (PT2.DataTyDecl (tvs, id', conDecls), env)
		  end
	    | PT1.AbsTyDecl (tvs, id) => let
		  val id' = Var.new(Atom.toString id, ())
		  val env = BEnv.insertDataTy(env, id, id')
	          in
		     (PT2.AbsTyDecl (tvs, id'), env)
		  end
           (* end case *))

    and chkTyDecls loc (tyDecls, env) = chkList loc (chkTyDecl, tyDecls, env)

    and chkConDecl loc (conDecl, env) = (case conDecl
           of PT1.MarkConDecl {span, tree} => let
		  val (tree, env) = chkConDecl span (tree, env)
	          in
                    (PT2.MarkConDecl{span=span, tree=tree}, env)
	          end
	    | PT1.ConDecl (id, tyOpt) => let
		  val id' = Var.new(Atom.toString id, ())
		  val env = BEnv.insertCon(env, id, id')
		  val (tyOpt, env) = (case tyOpt
                        of NONE => (NONE, env)
			 | SOME ty => let
			       val (ty, env) = chkTy loc (ty, env)
			       in
			          (SOME ty, env)
			       end
                        (* end case *))
	          in
		     (PT2.ConDecl(id', tyOpt), env)
		  end
           (* end case *))

    and chkConDecls loc (conDecls, env) = chkList loc (chkConDecl, conDecls, env)

    fun chkSpec loc (spec, env) = (case spec
           of PT1.MarkSpec {span, tree} => let
		  val (tree, env) = chkSpec span (tree, env)
	          in
		     (PT2.MarkSpec {span=span, tree=tree}, env)
		  end
	    | PT1.IncludeSpec sign => let
		  val (sign, env) = chkSign loc (sign, env)
	          in
		      (PT2.IncludeSpec sign, env)
		  end
	    | PT1.ModuleSpec (mb, sign) => let
		  val (sign, env) = chkSign loc (sign, env)
		  val mb' = Var.new(Atom.toString mb, ())
		  val env = BEnv.insertMod(env, mb, (mb', env))
	          in
		      (PT2.ModuleSpec (mb', sign), env)
		  end
	    | PT1.TypeSpec tyDecl => let
		  val (tyDecl, env) = chkTyDecl loc (tyDecl, env)
	          in
		     (PT2.TypeSpec tyDecl, env)
		  end
	    | PT1.ConstSpec (cb, tvs) => let
		  val cb' = Var.new(Atom.toString cb, ())
		  val env = BEnv.insertVal(env, cb, cb')
	          in
		     (PT2.ConstSpec (cb', tvs), env)
		  end
	    | PT1.ValSpec (vb, tvs, ty) => let
		  val (ty, env) = chkTy loc (ty, env)
		  val vb' = Var.new(Atom.toString vb, ())
		  val env = BEnv.insertVal(env, vb, vb')
		  in
		     (PT2.ValSpec (vb', tvs, ty), env)
		  end
          (* end case *))

    and chkSpecs loc (specs, env) = chkList loc (chkSpec, specs, env)

    and chkSign loc (sign, env) = (case sign
           of PT1.MarkSig {span, tree} => let
		  val (tree, env) = chkSign span (tree, env)
	          in
		     (PT2.MarkSig {span=span, tree=tree}, env)
		  end
	    | PT1.NameSig (id, tyDecls) => let
		  val (tyDecls, env) = chkTyDecls loc (tyDecls, env)
                  in
		     case BEnv.findSig(env, id)
                      of NONE => (error(loc, ["unbound signature ", Atom.toString id]);
				  (PT2.NameSig(Var.new("dummy", ()), tyDecls), env))
		       | SOME id' => (PT2.NameSig(id', tyDecls), env)
	          end
	    | PT1.ExpSig specs => let
		  val (specs, env) = chkSpecs loc (specs, env)
	          in
		     (PT2.ExpSig specs, env)
		  end
          (* end case *))

    fun chkModule loc (sign, env) = (case sign
           of PT1.MarkMod {span, tree} => let
		  val (tree, env) = chkModule span (tree, env)
	          in
		     (PT2.MarkMod {span=span, tree=tree}, env)
		  end
	    | PT1.DeclsMod decls => let
		  val (decls, env) = chkDecls loc (decls, env)
	          in
		     (PT2.DeclsMod decls, env)
		  end
	    | PT1.NamedMod id => (case QualifiedId.findMod(env, id)
                    of NONE => (error(loc, ["unbound module ", qidToString id]);
			       (PT2.NamedMod (Var.new("dummy", ())), env))
		     | SOME id' => (PT2.NamedMod id', env)
                    (* end case *))
           (* end case *))

    and chkDecl loc (decl, env) = (case decl
           of PT1.MarkDecl {span, tree} => let
		  val (tree, env) = chkDecl span (tree, env)
	          in
	             (PT2.MarkDecl{span=span, tree=tree}, env)
	          end
	    | PT1.ModuleDecl (mb, sign, module) => let
	          val (sign, env) = (case sign
		         of NONE => (NONE, env)
			  | SOME sign => let
				val (sign, env) = chkSign loc (sign, env)
			        in
				   (SOME sign, env)
				end
	                 (* end case *))
		  val mb' = Var.new(Atom.toString mb, ())
		  val (module, modEnv) = chkModule loc (module, BEnv.empty (SOME env))
		  val env = BEnv.insertMod(env, mb, (mb', modEnv))
	          in
	             (PT2.ModuleDecl(mb', sign, module), env)
	          end
	    | PT1.TyDecl tyDecl => let
		  val (tyDecl, env) = chkTyDecl loc (tyDecl, env)
	          in
		     (PT2.TyDecl tyDecl, env)
		  end
	    | PT1.ExnDecl (con, tyOpt) => let
		  val con' = Var.new(Atom.toString con, ())
		  val env = BEnv.insertCon(env, con, con')
		  val (tyOpt, env) = (case tyOpt
                        of NONE => (NONE, env)
			 | SOME ty => let
			       val (ty, env) = chkTy loc (ty, env)
			       in
			         (SOME ty, env)
			       end
                        (* end case *))
	          in
		     (PT2.ExnDecl (con', tyOpt), env)
		  end
	    | PT1.ValueDecl valDecl => let
		  val (valDecl, env) = chkValDecl loc (valDecl, env)
	          in
		    (PT2.ValueDecl valDecl, env)
		  end
	    | PT1.LocalDecl (locals, decls) => let
		  val (locals, env) = chkDecls loc (locals, env)
		  val (decls, env) = chkDecls loc (decls, env)
	          in
		     (PT2.LocalDecl (locals, decls), env)
		  end
	    | PT1.SignDecl (id, sign) => let
		  val id' = Var.new(Atom.toString id, ())
		  val (sign, sigEnv) = chkSign loc (sign, BEnv.empty (SOME env))
		  val env = BEnv.insertSig(env, id, id')
		  in
		     (PT2.SignDecl (id', sign), env)
		  end
           (* end case *))

    and chkDecls loc (decls, env) = chkList loc (chkDecl, decls, env)

    fun check (es, {span, tree}) = let
	val _ = errStrm := es
	val (tree', env) = chkDecls span (tree, BasisEnv.bEnv0)
        in		  
	   {span=span, tree=tree'}
        end

  end (* BoundVariableCheck *)
