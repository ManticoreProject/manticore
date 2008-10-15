(* translate-pval-cilk5.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module performs the translation [| e |] given below.

    [| let pval x = e1 in e2 end |]

 * =

   let 
   val ivar = WorkStealingIVar.ivar()
   fun bodyFn selFn = [| e2 |][x -> selFn()]
   cont slowPathK () = bodyFn(fn () => WorkStealingIVar.get ivar)
   val _ = Cilk5WorkStealing.push slowPathK
   val x = [| e1 |]
   in
      if (Cilk5WorkStealing.pop())
	 then bodyFn(fn () => x)
      else ( WorkStealingIVar.put(ivar, x); Control.stop() )
   
 * 
 * For the full technical discussion, see our Section 5 of our 2008 ICFP paper,
 * A scheduling framework for general-purpose parallel languages.
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
    structure BTy = BOMTy
    structure BU  = BOMUtil
    structure BTy = BOMTy
    structure BV = BOM.Var
    structure E = TranslateEnv

(* FIXME: we should be looking in the Basis environment for HLOps *)
    val findHLOp = #name o Option.valOf o HLOpEnv.findDefByPath
    fun getBOMTy (env, path) = (case TranslateEnv.findBOMTyDef(BasisEnv.getBOMTyFromBasis path)
	   of SOME ty => ty
	    | NONE => raise Fail("unable to find type " ^ String.concatWith "." path)
	  (* end case *))

    fun getTy (env, path) = (
	case BasisEnv.getTyFromBasis path
	 of ModuleEnv.TyCon tyc => (
	    case E.findTyc(env, tyc)
	     of SOME ty => ty
	      | NONE => raise Fail "unbound tyc"
	    (* end case *))
	  | _ => raise Fail "todo"
        (* end case *))

    fun iVarTy env = getBOMTy (env, ["WorkStealingIVar", "ivar2"])
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
	  val ivar = BV.new("ivar", iVarTy env)
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
	  val slowPath = BV.new("slowPath", BTy.T_Cont[BTy.unitTy])
	  val slowPathL = 
	      B.mkLambda{f=slowPath, params=[unitVar()], exh=[], body=
                 B.mkApply(bodyFn, [selFromIVar], [exh])}
	  val goLocal = BV.new("goLocal", BOMTy.boolTy)
	  val selLocally = BV.new("selLocally", BTy.T_Fun([BTy.unitTy], [BTy.exhTy], [ty1]))
	  val (selLocallyExh, _) = E.newHandler env
          in
	     B.mkLet([ivar], mkIVar exh,
             B.mkFun([bodyFnL, selFromIVarL],
             B.mkCont(slowPathL,
             B.mkLet([], mkWsPush(exh, slowPath),
	     trExp(env, e1, fn x1 =>
             B.mkFun([B.mkLambda{f=selLocally, params=[unitVar()], exh=[selLocallyExh], body=B.mkRet[x1]}],
             B.mkLet([goLocal], mkWsPop exh,
		      B.mkIf(goLocal,
			     B.mkApply(bodyFn, [selLocally], [exh]),
			     B.mkLet([], mkIPut(exh, ivar, x1),
				     mkStop exh)))))))))
	  end

  end
