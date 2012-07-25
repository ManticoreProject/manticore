(* inline.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Flattening transformation in BOM.
 *
 *)

structure Flatten : sig

  type env = BOMUtil.subst

  val transform : BOM.module -> BOM.module

  val trLambda  : env -> BOM.lambda -> (env * BOM.lambda)
  val trLambdas : env -> BOM.lambda list -> (env * BOM.lambda list)
  val trExp     : env -> BOM.exp -> BOM.exp
  val trRHS     : env -> BOM.rhs -> BOM.rhs
  val trVar     : env -> BOM.var -> (env * BOM.var)
  val trVars    : env -> BOM.var list -> (env * BOM.var list)

end = struct

  structure B  = BOM
  structure BV = BOM.Var
  structure U  = BOMUtil

  type env = U.subst
  val vSub = U.subst : env -> B.var -> B.var
  (* curry this for consistency *)
  fun vsSub (env : env) (vs : B.var list) =
    U.subst' (env, vs)

(* needNew : var -> bool *)
(* checks if we should be creating a new var as subst. for given var *)
  fun needNew v = (* bogus implementation *) true
        
  local
    fun trVar' (env : env) (v : B.var) = let
      val sub = vSub env v
      in
        if not(BV.same(sub,v)) then SOME(env, sub)
        else
          if needNew v then let
            val v' = BV.newWithKind (BV.nameOf v ^ "FLAT", BV.kindOf v, BV.typeOf v)
            val _ = TextIO.print (BV.nameOf v' ^ "\n")
            val env' = U.extend (env, v, v')
            in
              SOME (env', v')
            end 
          else NONE
      end
  in

  fun trVar (env : env) (v : B.var) =
    Option.getOpt(trVar' env v, (env, v))

  fun trVars (env : env) (vs : B.var list) = let
    fun lp (env, [], acc) = (env, List.rev acc)
      | lp (env, v::vs, acc) = 
         (case (trVar' env v)
            of SOME (env', v') => lp (env', vs, v'::acc)
             | NONE => lp (env, vs, v::acc)
           (* end case *))
    in
      lp (env, vs, [])
    end
  end (* local *)

  fun trRHS (env : env) (r) = U.substRHS (env,r)

  fun trExp (env : env) (exp : B.exp) = let
    val B.E_Pt(_, term) = exp
    in
      (case term
         of B.E_Let (vs, e1, e2) => let
            val (env', vs') = trVars env vs
            val e2' = trExp env' e2
            in
              B.mkLet(vs',
                      trExp env e1,
                      e2')
            end 
          | B.E_Stmt(vs, rhs, e) => let
            val (env', vs') = trVars env vs
            in
              B.mkStmt(vs,
                       trRHS env rhs,
                       trExp env e)
            end
          | B.E_Fun(lams, e) => U.substExp (env, exp) (* let
              val (env', lams') = trLambdas env lams
	      val e' = trExp env' e
              in
		B.mkFun (lams', e')
	      end *)
          | B.E_Cont(lam, e) => let
              val (env', lam') = trLambda env lam
              in
                B.mkCont(lam', trExp env' e)
              end
          | B.E_If(cond, e1, e2) =>
            B.mkIf(CondUtil.map (vSub env) cond,
                   trExp env e1,
                   trExp env e2)
          | B.E_Case(v, pelist, eopt) => let
            val (env', v') = trVar env v
            in
              B.mkCase(vSub env v,
                       map (fn (p, e) => (p, trExp env e)) pelist,
                       Option.map (trExp env) eopt)
            end
          | B.E_Apply(v, vs1, vs2) => let
            val (_, v') = trVar env v
            val (_, vs1') = trVars env vs1
            val (_, vs2') = trVars env vs2
            in
              B.mkApply(vSub env v,
                        vsSub env vs1,
                        vsSub env vs2)
            end
          | B.E_Throw(v, vs) => let
            val (env', v') = trVar env v
            val (_, vs') = trVars env' vs
            in
              B.mkThrow(vSub env v,
                        vsSub env vs)
            end
          | B.E_Ret(vs) => let
            val (_, vs') = trVars env vs
            in
              B.mkRet(vsSub env vs)
            end
          | B.E_HLOp(oper, vs1, vs2) => let
            val (_, vs1') = trVars env vs1
            val (_, vs2') = trVars env vs2
            in
              B.mkHLOp(oper,
                       vsSub env vs1,
                       vsSub env vs2)
            end
        (*end case*))
    end
        
  and trLambda (env : env) (lam as B.FB {f, params, exh, body}) = let
    val (env1, f') = trVar env f
    val (env2, params') = trVars env1 params
    val (env3, exh') = trVars env2 exh
    val body' = trExp env3 body
    in
      (env3, B.mkLambda {f=f', params=params', exh=exh', body=body'})
    end

  and trLambdas (env : env) lams = let
    fun trBody (env : env) (B.FB {f, params, exh, body}) =
      let
        val (env1, params') = trVars env params
        val (env2, exh') = trVars env1 exh
        val body' = trExp env2 body
      in
        B.mkLambda {f=f, params=params', exh=exh', body=body'}
      end
    fun lp ([], acc, env) = (env, List.rev acc)
      | lp (lam::lams, acc, env) =
          let
            val B.FB {f, params, exh, body} = lam
            val (env', f') = trVar env f
            val lam' = B.FB {f=f', params=params, exh=exh, body=body}
          in 
            lp (lams, lam'::acc, env')
          end
    val (env', lams') = lp (lams, [], env)
    val lams'' = List.map (trBody env') lams'
    in
      (env', lams'')
    end

  fun module (B.MODULE {name, externs, hlops, rewrites, body}) = let
    val (_, body') = trLambda U.empty body
    in
      B.MODULE {name=name,
                externs=externs,
                hlops=hlops,
                rewrites=rewrites,
                body=body'}
    end
      
  fun transform m = 
      if not(!BOMOptControls.flattenFlg) then m 
      else let
        val _ = TextIO.print "The compiler *would* be flattening now.\n"
        in
          module m
        end

end
