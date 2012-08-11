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

  val trLambda  : env -> BOM.lambda -> BOM.lambda
  val trLambdas : env -> BOM.lambda list -> BOM.lambda list
  val trExp     : env -> BOM.exp -> BOM.exp
  val trRHS     : env -> BOM.rhs -> BOM.rhs
  val trVar     : env -> BOM.var -> BOM.var
  val trVars    : env -> BOM.var list -> BOM.var list

  val replaceVar  : env -> BOM.var -> (env * BOM.var)
  val replaceVars : env -> BOM.var list -> (env * BOM.var list)

end = struct

  structure B  = BOM
  structure BV = BOM.Var
  structure U  = BOMUtil

  type env = U.subst
  val trVar = U.subst : env -> B.var -> B.var
  (* curry this for consistency *)
  fun trVars (env : env) (vs : B.var list) = U.subst' (env, vs)
  fun trRHS (env : env) (r) = U.substRHS (env,r)

  local
(* needNew : var -> bool *)
(* checks if we should be creating a new var as subst. for given var *)
    fun needNew v = (* bogus implementation *) true

    fun replaceVar' (env : env) (v : B.var) = let
      val sub = trVar env v
      in
        if not(BV.same(sub,v)) then SOME(env, sub)
        else
          if needNew v then let
            val v' = BV.newWithKind (BV.nameOf v ^ "FLAT", BV.kindOf v, BV.typeOf v)
            val env' = U.extend (env, v, v')
            in
              SOME (env', v')
            end 
          else NONE
      end
  in

    fun replaceVar (env : env) (v : B.var) =
      Option.getOpt(replaceVar' env v, (env, v))

    fun replaceVars (env : env) (vs : B.var list) = let
      fun lp (env, [], acc) = (env, List.rev acc)
        | lp (env, v::vs, acc) = 
           (case (replaceVar' env v)
              of SOME (env', v') => lp (env', vs, v'::acc)
               | NONE => lp (env, vs, v::acc)
             (* end case *))
      in
        lp (env, vs, [])
      end
  end (* local *)

  fun trExp (env : env) (exp : B.exp) = let
    val B.E_Pt(_, term) = exp
    in
      (case term
         of B.E_Let (vs, e1, e2) => let
            val (env', vs') = replaceVars env vs
            val e1' = trExp env e1
            val e2' = trExp env' e2
            in
              B.mkLet(vs', e1', e2')
            end 
          | B.E_Stmt(vs, rhs, e) =>
              B.mkStmt(trVars env vs, trRHS env rhs, trExp env e)
          | B.E_Fun(lams, e) =>
              B.mkFun (trLambdas env lams, trExp env e)
          | B.E_Cont(lam, e) =>
              B.mkCont(trLambda env lam, trExp env e)
          | B.E_If(cond, e1, e2) =>
              B.mkIf(CondUtil.map (trVar env) cond, trExp env e1, trExp env e2)
          | B.E_Case(v, pelist, eopt) =>
              B.mkCase(trVar env v, map (fn (p, e) => (p, trExp env e)) pelist, Option.map (trExp env) eopt)
          | B.E_Apply(v, vs1, vs2) =>
              B.mkApply(trVar env v, trVars env vs1, trVars env vs2)
          | B.E_Throw(v, vs) =>
              B.mkThrow(trVar env v, trVars env vs)
          | B.E_Ret(vs) =>
              B.mkRet(trVars env vs)
          | B.E_HLOp(oper, vs1, vs2) =>
              B.mkHLOp(oper, trVars env vs1, trVars env vs2)
        (*end case*))
    end
        
  and trLambda (env : env) (lam as B.FB {f, params, exh, body}) = let
    val body' = trExp env body
    in
      B.mkLambda {f=f, params=params, exh=exh, body=body'}
    end

  and trLambdas (env : env) lams = List.map (trLambda env) lams

  fun module (B.MODULE {name, externs, hlops, rewrites, body}) = let
    val body' = trLambda U.empty body
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
        val m = module m
        val _ = Census.census m
        in
            m
        end

end
