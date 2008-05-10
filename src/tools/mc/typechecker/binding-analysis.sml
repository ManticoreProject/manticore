structure BindingAnalysis :> sig

  (* check for unbound variables *)
    val check : ProgramParseTree.PML1.program -> (ProgramParseTree.PML2.program * BindingEnv.env)

  end = struct

    structure PT1 = ProgramParseTree.PML1
    structure PT2 = ProgramParseTree.PML2
    structure Var = ProgramParseTree.Var
    structure BEnv = BindingEnv

    val atos = Atom.toString
    fun qidToString path = Path.toString (Atom.toString, path)

    fun error _ = raise Fail ""

    val bogusExp = PT2.TupleExp []

    fun foldChks loc (chkX, xs, env) = let
	   fun f (x, (xs, env)) = let
	          val (x, env) = chkX loc (x, env)
                  in
	              (x :: xs, env)
	          end
	   val (xs', env) = List.foldl f ([], env) xs
           in
	      (List.rev xs', env)
           end

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
           (* end case *))

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
           (* end case *))

    and chkValDecls loc (valDecls, env) = foldChks loc (chkValDecl, valDecls, env)

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
	    | PT1.IdExp qId => (case QualifiedId.findVar(env, qId)
                  of NONE => 
		       (error(loc, ["unbound identifier ", qidToString qId]);
			(bogusExp, env))
		   | SOME var => 
		       (PT2.IdExp var, env)
	          (* end case *))
            (* end case *))

    and chkExps loc (exps, env) = foldChks loc (chkExp, exps, env)

    fun chkDecl loc (decl, env) = (case decl
        of PT1.MarkDecl {span, tree} => let
	       val (tree, env) = chkDecl span (tree, env)
	       in
	          (PT2.MarkDecl{span=span, tree=tree}, env)
	       end
        (* end case *))

    fun chkDecls loc (decls, env) = foldChks loc (chkDecl, decls, env)

    fun check {span, tree} = let
	val env0 = BEnv.topLevelEnv NONE
	val (tree', env) = chkDecls span (tree, env0)
        in
	   ({span=span, tree=tree'}, env)
        end

  end (* BindingAnalysis *)
