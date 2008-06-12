(* bom-bound-variable-check.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check BOM code for unbound variables.
 *)

structure BOMBoundVariableCheck :> sig

  (* check for unbound variables *)
    val checkCode : Error.span -> (ProgramParseTree.PML1.BOMParseTree.code * BindingEnv.env) 
		        -> (ProgramParseTree.PML2.BOMParseTree.code * BindingEnv.env)

  end = struct

    structure PT1 = ProgramParseTree.PML1.BOMParseTree
    structure PT2 = ProgramParseTree.PML2.BOMParseTree
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

  (* attempt to find the binding site of a qualified identifier, reporting an error if none exists *)
    fun findQid (find, kind, dummy) (loc, env, qId) = (case find(env, qId)
           of NONE => (
	      error(loc, ["unbound ", kind, " ", qidToString qId]);
	      dummy)
	    | SOME x => x
           (* end case *))

    val dummyVar = Var.new("dummyVar", ())
    val findBOMVarQid = findQid (QualifiedId.findBOMVar, "bom variable", dummyVar)

    fun freshVar v = Var.new(Atom.toString v, ())

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
           of PT1.T_Mark {tree, span} => let
		  val tree = chkTy loc (tree, env)
	          in
		      PT2.T_Mark {tree=tree, span=span}
		  end
           (* end case *))

    fun chkTys loc (tys, env) = List.map (fn ty => chkTy loc (ty, env)) tys

    fun chkRhs loc (rhs, env) = (case rhs
            of PT1.RHS_Mark {tree, span} => let
		   val tree = chkRhs loc (rhs, env)
	           in
		       PT2.RHS_Mark {tree=tree, span=span}
		   end
            (* end case *))

  (* check the lambda where the variable v is already bound *)
    and chkLambda' loc ((v, params, exns, returnTys, exp), env) = let	    
	    val returnTys' = chkTys loc (returnTys, env)
	    val (params', env') = chkVarPats loc (params, env)
	    val (exns', env') = chkVarPats loc (exns, env')
	    val exp' = chkExp loc (exp, env')
            in
	       (v, params', exns', returnTys', exp')
	    end

  (* bind the lambda in the environment add the fresh lambda to the list *)
    and bindLambda ((v, params, exns, returnTys, exp), (lambdas, env)) = let
	    val v' = freshVar v
	    val env' = BEnv.insertBOMVar(env, v, v')
	    val lambda' = (v', params, exns, returnTys, exp)
            in
	       (lambda' :: lambdas, env')
	    end

    and chkLambdas loc (lambdas, env) = let
	    val (lambdas, env') = List.foldl bindLambda ([], env) lambdas
            in
	        (List.map (fn lambda => chkLambda' loc (lambda, env)) lambdas, env')
	    end

    and chkExp loc (exp, env) = (case exp
            of PT1.E_Mark {tree, span} => let
		   val tree = chkExp loc (tree, env)
	           in
		       PT2.E_Mark {tree=tree, span=span}
		   end
	     | PT1.E_Let (vps, rhs, exp) => let
		   val rhs = chkRhs loc (rhs, env)
		   val (vps, env) = chkVarPats loc (vps, env)
		   val exp = chkExp loc (exp, env)
	           in
		       PT2.E_Let(vps, rhs, exp)
		   end
	     | PT1.E_Fun (lambdas, exp) => let
		   val (lambdas, env) = chkLambdas loc (lambdas, env)
		   val exp = chkExp loc (exp, env)
	           in
		       PT2.E_Fun (lambdas, exp)
		   end
	     | PT1.E_Cont (lambda, exp) => let
		   val ([lambda], env) = chkLambdas loc ([lambda], env)
		   val exp = chkExp loc (exp, env)
	           in
		       PT2.E_Cont (lambda, exp)
		   end
	     | PT1.E_If (sexp, exp1, exp2) => let
		   val sexp = chkSexp loc(sexp, env)
		   val exp1 = chkExp loc (exp1, env)
		   val exp2 = chkExp loc (exp2, env)
	           in
		       PT2.E_If (sexp, exp1, exp2)
		   end
	     | PT1.E_Case (sexp, cases, def) => let
		   val sexp = chkSexp loc(sexp, env)
		   val cases = chkCases loc (cases, env)
		   val def = (case def
			       of NONE => NONE
				| SOME def => SOME (chkCase loc (def,  env))
			     (* end *))
	           in
		       PT2.E_Case (sexp, cases, def)
		   end
	     | PT1.E_Apply (f, args, exns) => let
		   val f = findBOMVarQid(loc, env, f)
		   val args = chkSexps loc (args, env)
		   val exns = chkSexps loc (exns, env)
	           in
		       PT2.E_Apply (f, args, exns)
		   end
            (* end case *))

    and chkSexp loc (sexp, env) = raise Fail ""

    and chkSexps loc (sexp, env) = raise Fail ""

    and chkCase loc (c, env) = raise Fail ""

    and chkCases loc (cases, env) = raise Fail ""

    and chkVarPat loc (vp, env) = (case vp
            of PT1.P_VPMark {tree, span} => let
		   val (tree, env) = chkVarPat loc (tree, env)
	           in
	              (PT2.P_VPMark {tree=tree, span=span}, env)
		   end
	     | PT1.P_Wild tyOpt => 
	           (PT2.P_Wild (Option.map (fn ty => chkTy loc (ty, env)) tyOpt), env)
	     | PT1.P_Var (vb, ty) => let
		   val vb' = freshVar vb
		   val env' = BEnv.insertBOMVar(env, vb, vb')
	           in
		       (PT2.P_Var (vb', chkTy loc (ty, env)), env')
	           end
            (* end case *))

    and chkVarPats loc (vps, env) = chkList loc (chkVarPat, vps, env)

    fun chkDefn loc (defn, env) = (case defn
	    of PT1.D_Mark {tree, span} => let
		   val (tree, env) = chkDefn span (tree, env)
	           in
		      (PT2.D_Mark {tree=tree, span=span}, env)
	           end
	     | PT1.D_Define (b, v, params, exns, returnTys, exp) => let
		   val v' = freshVar v
		   val env' = BEnv.insertBOMVar(env, v, v')
		   val returnTys' = (case returnTys
				      of NONE => NONE 
				       | SOME returnTys => SOME (chkTys loc (returnTys, env))
				    (* end case *))
		   val (params', env'') = chkVarPats loc (params, env')
		   val (exns', env'') = chkVarPats loc (exns, env')
		   val exp' = (case exp
				of NONE => NONE 
				 | SOME exp => SOME (chkExp loc (exp, env))
			      (* end case *))
	           in
		       (PT2.D_Define (b, v', params', exns', returnTys', exp'), env')
		   end
             (* end case *))

    fun checkCode loc (defs, env) = chkList loc (chkDefn, defs, env)

  end (* BOMBoundVariableCheck *)
