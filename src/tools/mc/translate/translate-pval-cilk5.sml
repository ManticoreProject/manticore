(* translate-pval-cilk5.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translate pval bindings of the form 
 *   let pval x = e1 in e2 end
 *)

structure TranslatePValCilk5  : sig

  (* An AST to BOM translation of parrays to ropes. *)
    val tr : {
	  env : TranslateEnv.env,
	  trVar : (TranslateEnv.env * AST.var) -> (BOM.Var.var * TranslateEnv.env),
	  trExp : TranslateEnv.env * AST.exp * (BOM.Var.var -> BOM.exp) -> BOM.exp,
	  x : AST.var,
	  e1 : AST.exp,
	  e2 : AST.exp
	} -> BOM.exp

  end  = struct

    structure B   = BOM
    structure BB  = BOMBasis
    structure BTy = BOMTy
    structure BU  = BOMUtil
    structure BTy = BOMTy
    structure BV = BOM.Var
    structure E = TranslateEnv

    val getTyc = BasisEnv.getBOMTyFromBasis
    val findHLOp = #name o Option.valOf o HLOpEnv.findDefByPath

    fun iVarTy () = getTyc ["WorkStealingIVar", "ivar"]
    fun iGet () = findHLOp ["WorkStealingIVar", "get"]
    fun iPut () = findHLOp ["WorkStealingIVar", "put"]
    fun iVar () = findHLOp ["WorkStealingIVar", "ivar"]
    fun mkIVar exh = 
	  B.mkHLOp(iVar(), [], [exh])
    fun mkIPut (exh, iv, x) =
	  B.mkHLOp(iPut(), [iv, x], [exh])
    fun mkIGet (exh, iv) =
	  B.mkHLOp(iGet(), [iv], [exh])

    fun wsPush () = findHLOp ["Cilk5WorkStealing", "push-tl"]
    fun wsPop () = findHLOp ["Cilk5WorkStealing", "pop-tl"]
    fun mkWsPush (exh, kLocal) =
	  B.mkHLOp(wsPush(), [kLocal], [exh])
    fun mkWsPop exh =
	  B.mkHLOp(wsPop(), [], [exh])

    fun mkStop exh = 
	  B.mkHLOp(findHLOp ["Control", "stop"], [], [exh])

    fun unitVar () = BV.new("_unit", BTy.unitTy)

    fun tr {env, trExp, trVar, x, e1, e2} = let
	  val exh = E.handlerOf env
	  val ty1 = TranslateTypes.tr(env, TypeOf.exp e1)
	  val ty2 = TranslateTypes.tr(env, TypeOf.exp e2)
	  val ivar = BV.new("ivar", TranslateTypes.cvtPrimTy env (iVarTy()))
	  val (x', env) = trVar(env, x)
	  val selFnTy = BTy.T_Fun([], [BTy.exhTy], [ty1])
	  val selFn = BV.new("selFn", selFnTy)
	  val bodyFn = BV.new("bodyFn", BTy.T_Fun([selFnTy], [BTy.exhTy], [ty2]))
	  val (bodyExh, env) = E.newHandler env
	  val bodyFnL = 
	      B.mkLambda{f=bodyFn, params=[selFn], exh=[bodyExh], body=
                 B.mkLet([x'], B.mkApply(selFn, [], [exh]),
		    trExp(env, e2, fn v2 => B.mkRet[v2]))}
	  val selFromIVar = BV.new("selFromIVar", BTy.T_Fun([], [BTy.exhTy], [ty1]))
	  val (selFromIVarExh, env) = E.newHandler env
	  val selFromIVarL = B.mkLambda{f=selFromIVar, params=[], exh=[selFromIVarExh], body=mkIGet(exh, ivar)}
	  val slowPath = BV.new("slowPath", BTy.T_Cont[BTy.unitTy])
	  val slowPathL = 
	      B.mkLambda{f=slowPath, params=[unitVar()], exh=[], body=
                 B.mkApply(bodyFn, [selFromIVar], [exh])}
	  val goLocal = BV.new("goLocal", BTy.boolTy)
	  val selLocally = BV.new("selLocally", BTy.T_Fun([], [BTy.exhTy], [ty1]))
	  val (selLocallyExh, env) = E.newHandler env
          in
	     B.mkLet([ivar], mkIVar exh,
             B.mkFun([bodyFnL, selFromIVarL],
             B.mkCont(slowPathL,
             B.mkLet([], mkWsPush(exh, slowPath),
	     trExp(env, e1, fn x1 =>
             B.mkFun([B.mkLambda{f=selLocally, params=[], exh=[selLocallyExh], body=B.mkRet[x1]}],
             B.mkLet([goLocal], mkWsPop exh,
		      B.mkIf(goLocal,
			     B.mkApply(bodyFn, [selLocally], [exh]),
			     B.mkLet([], mkIPut(exh, ivar, x1),
				     mkStop exh)))))))))
	  end

  end
