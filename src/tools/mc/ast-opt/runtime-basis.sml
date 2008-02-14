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

    fun monoVar (name, ty) = Var.new (name, ty)

    fun polyVar (name, mkTy) = Var.newPoly (name, forall mkTy)

   (* first-class continuations *)
    val contTyc = TyCon.newAbsTyc (Atom.atom "cont", 1, false)
    fun contTy t = T.ConTy([t], contTyc)

    val callcc = polyVar ("callcc",
			  fn tv => (contTy(tv) --> tv) --> tv)

    val throwcc = polyVar ("throwcc",
			fn tv => AST.TupleTy [contTy(tv), tv] --> voidTy)

    fun mkContVar (ty, k) = 
	monoVar(k, contTy(ty))

    fun mkCallcc (ty, e) =
	AU.mkApplyExp(AU.mkVarExp(callcc, [ty]), [e])

    fun mkThrowcc (ty, k, e) =
	AU.mkApplyExp(AU.mkVarExp(throwcc, [ty]), [A.VarExp(k, [contTy(ty)]), e])

   (* ivars *)
(*    val ivarTyc = TyCon.newAbsTyc (Atom.atom "ivar", 1, false)
    fun ivarTy t = T.ConTy([t], ivarTyc)

    val ivarNew = polyVar("ivarNew",
		       fn tv => Basis.unitTy --> ivarTy(tv))

    val ivarPut = polyVar("ivarPut",
			  fn tv => (ivarTy(tv) ** tv) --> Basis.unitTy)

    val ivarGet = polyVar("ivarGet",
			  fn tv => ivarTy(tv) --> tv)
*)

    val ivarTyc = Basis.ivarTyc
    fun ivarTy t = T.ConTy([t], ivarTyc)
    val ivarNew = Basis.iVar
    val ivarGet = Basis.iGet
    val ivarPut = Basis.iPut

    fun ivarVar (ty, name) = let
	val ty = ivarTy(ty)
        in
           (monoVar(name, ty), ty)
        end

    fun mkIvar (ty) =
	AU.mkApplyExp(AU.mkVarExp(ivarNew, [ty]), [])

    fun mkIvarGet (ivar, ty) = 
	AU.mkApplyExp(AU.mkVarExp(ivarGet, [ty]),
		      [A.VarExp(ivar, [ivarTy(ty)])])

    fun mkIvarPut ((ivar, ty), e) = 
	AU.mkApplyExp(AU.mkVarExp(ivarPut, [ty]),
		      [A.VarExp(ivar, [ivarTy(ty)]), e])

   (* lazy task creation *)
    val ltcPush = monoVar ("ltcPush", 
			   (Basis.unitTy --> voidTy) --> voidTy)

    val ltcPop  = monoVar ("ltcPop", Basis.unitTy --> voidTy)

    fun mkLtcPush (ctx) = 
	AU.mkApplyExp (A.VarExp(ltcPush, []), [A.VarExp(ctx, [])])

    val mkLtcPop =
	AU.mkApplyExp (A.VarExp(ltcPop, []), [])

  (* set once cells*)
    val setOnceCellTyc = TyCon.newAbsTyc(Atom.atom "set_once_cell", 0, false)
    val setOnceCellTy = T.ConTy([], setOnceCellTyc)

    val setOnceCell = monoVar ("setOnceCell", 
			   Basis.unitTy --> setOnceCellTy)

    val setCell  = monoVar ("setCell", 
			    setOnceCellTy --> Basis.boolTy)

    val setOnceCellVar = monoVar("doneCell", setOnceCellTy)

    val mkSetOnceCell = 
	AU.mkApplyExp(A.VarExp(setOnceCell, []) , [])

    fun mkSetCell (cell) =
	AU.mkApplyExp(A.VarExp(setCell, []), [A.VarExp(cell, [])])
			   
  end
