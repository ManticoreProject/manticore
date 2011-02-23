(* ft-translate-pval-cilk5.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module performs the translation [| e |] given below.

    [| let pval x = e1 in e2 end |]

 * =

   let
   val ivar = ImplicitThreadIVar.empty-ivar()
   fun bodyFn selFn = [| e2 |][x -> selFn()]
   cont slowPathK () = bodyFn(fn () => ImplicitThreadIVar.get ivar)
   val _ = SwpWorkStealing.push-tl(ImplicitThread.thread slowPathK)
   val x = [| e1 |]
   in
      if (SwpWorkStealing.pop-tl())
	 then bodyFn(fn () => x)
      else ( ImplicitThreadIVar.put(ivar, x); SchedulerAction.stop() )
   
 * 
 * For the full technical discussion, see our Section 5 of our 2008 ICFP paper,
 * A scheduling framework for general-purpose parallel languages.
 *)

structure FTTranslatePValCilk5  : sig

  (* A FLAST to BOM translation of parrays to ropes. *)
    val tr : {
	  env : FTTranslateEnv.env,
	  trVar : (FTTranslateEnv.env * FLAST.var) -> (BOM.Var.var * FTTranslateEnv.env),
	  trExp : FTTranslateEnv.env * FLAST.exp * (BOM.Var.var -> BOM.exp) -> BOM.exp,
	  x : FLAST.var,
	  e1 : FLAST.exp,
	  e2 : FLAST.exp
	} -> BOM.exp

  end  = struct

    structure B   = BOM
    structure BTy = BOMTy
    structure BU  = BOMUtil
    structure BTy = BOMTy
    structure BV = BOM.Var
    structure E = FLTranslateEnv

    fun unitVar () = BV.new("_unit", BTy.unitTy)

    val findBOMTy = E.findBOMTyByPath
    val findHLOp = E.findBOMHLOpByPath

  (* ivar support *)
    fun mkIVar exh = 
	  B.mkHLOp(findHLOp["ImplicitThreadIVar", "empty-ivar"], [], [exh])
    fun mkIPut (exh, iv, x) =
	  B.mkHLOp(findHLOp["ImplicitThreadIVar", "put"], [iv, x], [exh])
    fun mkIGet (exh, iv) =
	  B.mkHLOp(findHLOp["ImplicitThreadIVar", "get"], [iv], [exh])
  (* implicit-thread creation *)
    fun mkThread (exh, k) =
	  B.mkHLOp(findHLOp["ImplicitThread", "thread-no-cancelable"], [k], [exh])
  (* deque support *)
    fun mkWsPush (exh, kLocal) =
	  B.mkHLOp(findHLOp["SwpWorkStealing", "push-tl"], [kLocal], [exh])
    fun mkWsPop exh =
	  B.mkHLOp(findHLOp["SwpWorkStealing", "pop-tl"], [], [exh])

    fun mkRaiseExn (env, exh) = let
	  val matchExnDCon = (
	        case E.findDCon(env, Basis.exnMatch)
		 of SOME(E.ExnConst matchExnDCon) => matchExnDCon
		  | _ => raise Fail "compiler bug: missing match exception"
 	        (* end case *))
	  val matchExn = BV.new("matchExn", BTy.T_TyCon(BOMTyCon.dconTyc matchExnDCon))
	  in
	    B.mkStmt([matchExn], B.E_DCon(matchExnDCon, []),
		    B.mkThrow(exh, [matchExn]))
	  end

    fun mkStop (env, exh) = 
	  B.mkLet([unitVar()], 
		    B.mkHLOp(findHLOp["SchedulerAction", "stop"], [], []),
	    mkRaiseExn(env, exh))

    fun tr {env, trExp, trVar, x, e1, e2} = let
	  val exh = E.handlerOf env
	  val ty1 = TranslateTypes.tr(env, TypeOf.exp e1)
	  val ty2 = TranslateTypes.tr(env, TypeOf.exp e2)
	  val ivar = BV.new("ivar", findBOMTy["ImplicitThreadIVar", "ivar"])
	  val (x', env) = trVar(env, x)
	  val selFnAST = Var.new("selFn", AST.FunTy(Basis.unitTy, TypeOf.exp e1))
	  val (selFn, env) = trVar(env, selFnAST)
        (* e2' = e2[x -> selFn()] *)
	  val e2' = VarSubst.substForExp (VarSubst.idSubst x) 
					 (ASTUtil.mkApplyExp(AST.VarExp(selFnAST, []), [AST.TupleExp[]]))
					 e2
	  val bodyFn = BV.new("bodyFn", BTy.T_Fun([BV.typeOf selFn], [BTy.exhTy], [ty2]))
	  val (bodyExh, _) = E.newHandler env
	  val bodyFnL = 
	      B.mkLambda{f=bodyFn, params=[selFn], exh=[bodyExh], body=trExp(env, e2', fn v2 => B.mkRet[v2])}
	  val selFromIVar = BV.new("selFromIVar", BTy.T_Fun([BTy.unitTy], [BTy.exhTy], [ty1]))
	  val (selFromIVarExh, _) = E.newHandler env
	  val selFromIVarL = B.mkLambda{f=selFromIVar, params=[unitVar()], exh=[selFromIVarExh], body=mkIGet(exh, ivar)}
	  val slowPathK = BV.new("slowPathK", BTy.T_Cont[BTy.unitTy])
	  val slowPathL = 
	      B.mkLambda{f=slowPathK, params=[unitVar()], exh=[], body=
                 B.mkApply(bodyFn, [selFromIVar], [exh])}
	  val slowPath = BV.new("slowPath", findBOMTy["ImplicitThread", "thread"])
	  val goLocal = BV.new("goLocal", BOMTyUtil.boolTy)
	  val selLocally = BV.new("selLocally", BTy.T_Fun([BTy.unitTy], [BTy.exhTy], [ty1]))
	  val (selLocallyExh, _) = E.newHandler env
          in
	    B.mkLet([ivar], mkIVar exh,
	    B.mkFun([bodyFnL, selFromIVarL],
	    B.mkCont(slowPathL,
	    B.mkLet([slowPath], mkThread(exh, slowPathK),
	    B.mkLet([], mkWsPush(exh, slowPath),
	    trExp(env, e1, fn x1 =>
	    B.mkFun([B.mkLambda{f=selLocally, params=[unitVar()], exh=[selLocallyExh], body=B.mkRet[x1]}],
	    B.mkLet([goLocal], mkWsPop exh,
	      BOMUtil.mkBoolCase (goLocal,
		B.mkApply(bodyFn, [selLocally], [exh]),
		B.mkLet([], mkIPut(exh, ivar, x1), mkStop(env, exh)))))))))))
	  end

  end
