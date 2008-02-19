structure RuntimeBasis 
(*: sig

   (* first-class continuations *)
    val contTyc : Types.tycon
    val contTy : Types.ty -> Types.ty

    val callcc : AST.var
    val throwcc : AST.var

   (* I-variables *)
    val ivarTyc : Types.tycon
    val ivarTy : Types.ty -> Types.ty

    val ivarNew : AST.var
    val ivarPut : AST.var
    val ivarGet : AST.var

   (* lazy task creation *)
    val ltcPush : AST.var
    val ltcPop  : AST.var

    val mkLtcPush : AST.var -> AST.exp
    val mkLtcPop  : AST.exp

  end *) = struct

    structure A = AST
    structure AU = ASTUtil
    structure B = Basis
    structure T = Types

    val --> = T.FunTy
    infixr 8 -->

    fun ** (t1, t2) = T.TupleTy [t1, t2]
    infixr 8 **

    fun forall mkTy =
	let val tv = TyVar.new (Atom.atom "'a")
	in
	    T.TyScheme ([tv], mkTy (A.VarTy tv))
	end

    val voidTy = T.VarTy (TyVar.new (Atom.atom "'a"))

   (* first-class continuations *)
    val contTyc = TyCon.newAbsTyc (Atom.atom "cont", 1, false)
    fun contTy t = T.ConTy([t], contTyc)

    val callcc = BasisUtils.polyVar ("callcc",
			  fn tv => (contTy(tv) --> tv) --> tv)

    val throwcc = BasisUtils.polyVar ("throwcc",
			fn tv => AST.TupleTy [contTy(tv), tv] --> Basis.unitTy)

    fun mkCallcc (ty, e) =
	AU.mkApplyExp(AU.mkVarExp(callcc, [ty]), [e])

    fun mkThrowcc (ty, k, e) =
	AU.mkApplyExp(AU.mkVarExp(throwcc, [ty]), [A.VarExp(k, []), e])

   (* ivars *)
    fun mkIVarNew (ty) =
	AU.mkApplyExp(AU.mkVarExp(Basis.iVar, [ty]), [])

    fun mkIVarGet (ty, ivar) = 
	AU.mkApplyExp(AU.mkVarExp(Basis.iGet, [ty]),
		      [A.VarExp(ivar, [])])

    fun mkIVarPut (ty, ivar, e) = 
	AU.mkApplyExp(AU.mkVarExp(Basis.iPut, [ty]),
		      [A.VarExp(ivar, []), e])

   (* lazy task creation *)
    val ltcPush = BasisUtils.monoVar ("ltcPush", 
			   (Basis.unitTy --> voidTy) --> voidTy)

    val ltcPop  = BasisUtils.monoVar ("ltcPop", Basis.unitTy --> Basis.boolTy)

    fun mkLtcPush (ctx) = 
	AU.mkApplyExp (A.VarExp(ltcPush, []), [A.VarExp(ctx, [])])

    val mkLtcPop =
	AU.mkApplyExp (A.VarExp(ltcPop, []), [])

    val threadExit = BasisUtils.monoVar("threadExit", Basis.unitTy --> voidTy)
    val mkThreadExit = AU.mkApplyExp(A.VarExp(threadExit, []), [])

   (* set once cells *)
    val setOnceCellTyc = TyCon.newAbsTyc(Atom.atom "set_once_cell", 0, false)
    val setOnceCellTy = T.ConTy([], setOnceCellTyc)
			
    val setOnceCell = BasisUtils.monoVar ("setOnceCell", 
			       Basis.unitTy --> setOnceCellTy)
		      
    val setCell  = BasisUtils.monoVar ("setCell", 
			    setOnceCellTy --> Basis.boolTy)
		   
    val setOnceCellVar = BasisUtils.monoVar("doneCell", setOnceCellTy)
			 
    val mkSetOnceCell = 
	AU.mkApplyExp(A.VarExp(setOnceCell, []) , [])
	
    fun mkSetCell (cell) =
	AU.mkApplyExp(A.VarExp(setCell, []), [A.VarExp(cell, [])])

  end
