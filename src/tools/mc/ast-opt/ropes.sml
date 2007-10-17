(* ropes.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Ropes : sig

    val ropeTyc      : Types.tycon
    val ropeTy       : Types.ty -> Types.ty
    val rope         : Atom.atom
    val ropeFromList : AST.var

  end = struct

    val rope = Atom.atom "rope"
	       
    local
	val tv = TyVar.new(Atom.atom "'a")
	val tv' = AST.VarTy tv
    in
        val ropeTyc = TyCon.newDataTyc (rope, [tv])
    end

    (* ropeTy : Types.ty -> Types.ty *)
    fun ropeTy ty = AST.ConTy ([ty], ropeTyc)
			     
    local

	(* forall : (Types.ty -> Types.ty) -> AST.ty_scheme *)
	fun forall mkTy = 
	    let val tv = TyVar.new (Atom.atom "'a")
	    in
		AST.TyScheme ([tv], mkTy (AST.VarTy tv))
	    end

	(* polyVar : string * (Types.ty -> Types.ty) -> AST.var *)
	fun polyVar (name, mkTy) = Var.newPoly (name, forall mkTy)

    in
        val ropeFromList = polyVar ("ropeFromList", 
				    fn tv => AST.FunTy (Basis.listTy tv, ropeTy tv))
    end (* local *)
  
  end (* structure Ropes *)
