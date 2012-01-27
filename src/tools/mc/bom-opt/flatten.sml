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
  val trVar : env -> BOM.var -> (env * BOM.var) option

end = struct

  type env = BOMUtil.subst

  structure B  = BOM
  structure BV = BOM.Var
  structure U  = BOMUtil

(* needNew : var -> bool *)
(* checks if we should be creating a new var as subst. for given var *)
  fun needNew v = (* bogus implementation *) true

  fun trVar (env : env) (v : B.var) = 
    if needNew v then let
      val v' = BV.newWithKind (BV.nameOf v ^ "FLAT", BV.kindOf v, BV.typeOf v)
      val env' = U.extend (env, v, v') (* TODO: check whether order of v, v' is right *)
      in
        SOME (env', v')
      end
    else NONE

  fun trVars (env : env) (vs : B.var list) = let
    fun lp (env, [], acc) = (env, List.rev acc)
      | lp (env, v::vs, acc) = 
         (case (trVar env v)
            of SOME (env', v') => lp (env', vs, v'::acc)
             | NONE => lp (env, vs, v::acc)
           (* end case *))
    in
      lp (env, vs, [])
    end

  fun trRHS (env : env) (r) = U.substRHS (env,r)

  fun trExp (env : env) (exp : B.exp) = let
    val B.E_Pt(_, term) = U.substExp (env, exp)
    in
      (case term
         of B.E_Let (vs, e1, e2) => let
            val (env', vs') = trVars env vs
            val e2' = trExp env' e2
            in
              B.mkLet(vs', e1, e2')
            end 
          | B.E_Stmt(vs, rhs, e) => let
            val (env', vs') = trVars env vs
            in
            B.mkStmt(vs,
                     rhs,
                     e)
            end
          | B.E_Fun(lamlist, e) => B.mkFun(lamlist, e)
          | B.E_Cont(lam, e) => B.mkCont(lam, e)
          | B.E_If(cond, e1, e2) =>
            B.mkIf(cond,
                   e1,
                   e2)
          | B.E_Case(v, pelist, eopt) => let
            val (env', v') = Option.getOpt(trVar env v, (env,v))
            in
              B.mkCase(v, pelist, eopt)
            end
          | B.E_Apply(v, vs1, vs2) => let
            val (_, v') = Option.getOpt(trVar env v, (env,v))
            val (_, vs1') = trVars env vs1
            val (_, vs2') = trVars env vs2
            in
              B.mkApply(v, vs1, vs2)
            end
          | B.E_Throw(v, vs) => let
            val (env', v') = Option.getOpt(trVar env v, (env,v))
            val (_, vs') = trVars env' vs
            in
              B.mkThrow(v, vs)
            end
          | B.E_Ret(vs) => let
            val (_, vs') = trVars env vs
            in
              B.mkRet(vs)
            end
          | B.E_HLOp(oper, vs1, vs2) => let
            val (_, vs1') = trVars env vs1
            val (_, vs2') = trVars env vs2
            in
              B.mkHLOp(oper, vs1, vs2)
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
