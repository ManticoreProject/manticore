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

  val trLambda : env -> BOM.lambda -> BOM.lambda
  val trExp : env -> BOM.exp -> BOM.exp
  val trRHS : env -> BOM.rhs -> BOM.rhs
  val trVar : env -> BOM.var -> (env * BOM.var)
  val trVars : env -> BOM.var list -> (env * BOM.var list)

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

(* Begin Local *) local
    fun trVar' (env : env) (v : B.var) = 
      if needNew v then let
        val v' = BV.newWithKind (BV.nameOf v ^ "FLAT", BV.kindOf v, BV.typeOf v)
        val env' = U.extend (env, v, v') (* TODO: check whether order of v, v' is right *)
        in
          SOME (env', v')
        end
      else NONE
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

  end (* End Local *)


  fun trRHS (env : env) (r) = U.substRHS (env,r)

  fun trExp (env : env) (exp : B.exp) = let
    val B.E_Pt(_, term) = exp (* U.substExp (env, exp) *)
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
          | B.E_Fun(lamlist, e) => U.substExp (env, exp) (*
            B.mkFun(map (trLambda env) lamlist,
                    trExp env e) *)
          | B.E_Cont(lam, e) =>
            B.mkCont(trLambda env lam,
                     trExp env e)
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
        
  and trLambda (env : env) (lam as B.FB {f, params, exh, body}) =
        B.FB {f=f,
              params=params,
              exh=exh,
              body=(trExp env body)}
        
  fun module (B.MODULE {name, externs, hlops, rewrites, body}) =
      B.MODULE {name=name,
                externs=externs,
                hlops=hlops,
                rewrites=rewrites,
                body=(trLambda U.empty body)}
      
  fun transform m = 
      if not(!BOMOptControls.flattenFlg) then m 
      else let
        val _ = TextIO.print "The compiler *would* be flattening now.\n"
        in
          module m
        end

end
