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

    fun chkTy loc (ty, env) = raise Fail "todo"

    fun chkExp loc (exp, env) = raise Fail "todo"

    fun chkTys loc (tys, env) = List.map (fn ty => chkTy loc (tys, env)) tys

    fun chkVarPat loc (vp, env) = (case vp
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

    fun chkVarPats loc (vps, env) = chkList loc (chkVarPat, vps, env)

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
