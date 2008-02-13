structure RuntimeBasis : sig

    val contTyc : Types.tycon
    val contTy : Types.ty -> Types.ty

    val callcc : AST.var
    val throwcc : AST.var

  end = struct

    structure A = AST
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

    fun monoVar (name, ty) = Var.new (name, ty)

    fun polyVar (name, mkTy) = Var.newPoly (name, forall mkTy)

    val contTyc = TyCon.newAbsTyc (Atom.atom "cont", 1, false)
    fun contTy t = T.ConTy([t], contTyc)

    val callcc = polyVar ("callcc",
			  fn tv => (contTy(tv) --> tv) --> tv)

    val throwcc = let 
	fun mkTy ([a,b]) = 
	    AST.TupleTy [contTy(a), a] --> b
	  | mkTy _ = raise Fail "BUG: bad instantiation for throwcc"
	in
	    BasisUtils.polyVarMulti ("throwcc", 2, mkTy)
	end

  end
