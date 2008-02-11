(* ropes.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Ropes : sig

    val maxLeafSize  : int
    val ropeTyc      : Types.tycon
    val ropeTy       : Types.ty -> Types.ty
    val rope         : Atom.atom
    val ropeLeaf     : AST.dcon
    val ropeCat      : AST.dcon
    val ropeLeafExp  : Types.ty -> AST.exp
    val ropeCatExp   : Types.ty -> AST.exp

  end = struct

    structure A = AST
    structure B = Basis
    structure T = Types

    exception VariableArityType

    val maxLeafSize = 4 (* This is currently small, for testing purposes. *)

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
	fun newRopeDCon (name:string, argTys) = 
	    DataCon.new ropeTyc (Atom.atom name, SOME (T.TupleTy argTys))
	val tv = AST.VarTy (TyVar.new (Atom.atom "'a"))
	val intTy = B.intTy
    in
        val ropeLeaf = newRopeDCon ("LEAF", [intTy, B.listTy tv])
	val ropeCat  = newRopeDCon ("CAT", [intTy, intTy, ropeTy tv, ropeTy tv])
    end (* local *)

    local
	fun dconExp dcon = (fn t => A.ConstExp (A.DConst (dcon, [t])))
    in
        val ropeLeafExp = dconExp ropeLeaf
	val ropeCatExp  = dconExp ropeCat
    end (* local *)

  end (* structure Ropes *)
