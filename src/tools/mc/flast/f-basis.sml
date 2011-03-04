(* f-basis.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Basis library for FLAST representation.
 *)

(* FIXME all elements here need to be included in the flatten env
 * before translation
 *)

structure FBasis : sig

(* type constructors *)
  val threadIdTyc : FTTypes.tycon
  val intTyc      : FTTypes.tycon
  val parrayTyc   : FTTypes.tycon
  val listTyc     : FTTypes.tycon

(* types *)
  val threadIdTy : FTTypes.ty
  val intTy      : FTTypes.ty
  val parrayTy   : FTTypes.ty -> FTTypes.ty
  val listTy     : FTTypes.ty -> FTTypes.ty

(* constructors *)
  val listNil    : FTTypes.dcon
  val listCons   : FTTypes.dcon

end = struct

  structure F = FLAST
  structure T = FTTypes
  structure N = BasisNames

(* copyAbs : AST.tycon -> FLAST.tycon *)
(* utility to copy abstract tycons from one IL to another *)
  fun copyAbs (original: Types.tycon) : FTTypes.tycon = let
    val name = TyCon.nameOf original
    val arity = TyCon.arityOf original
    val eq = TyCon.isEqTyc original
    in
      FTTyCon.newAbsTyc (name, arity, eq, original)
    end

(* type constructors *)
  val threadIdTyc = copyAbs Basis.threadIdTyc

  val parrayTyc = let
    val alpha = TyVar.new (Atom.atom "'a")
    in
      FTTyCon.newDataTyc (N.parray, [alpha], Basis.parrayTyc)
    end

  val intTyc = copyAbs Basis.intTyc

  local
    val alpha = TyVar.new (Atom.atom "'a")
    val alphaTy = FTTypes.VarTy alpha
  in
    val listTyc = FTTyCon.newDataTyc (N.list, [alpha], Basis.listTyc)
    val dc = FTDataCon.new listTyc
    val listNil  = dc (N.listNil, NONE, Basis.listNil)
    val listCons = let
          val interfaceTy = valOf (DataCon.argTypeOf Basis.listCons)
	  val tupTy = FTTypes.TupleTy (interfaceTy,
				       [alphaTy, FTTypes.ConTy ([alphaTy],
								listTyc)])
	  in
	    dc (N.listCons, SOME tupTy, Basis.listCons)
          end
  end

(* types *)
  val threadIdTy = FLAST.ConTy ([], threadIdTyc)
  val intTy      = FLAST.ConTy ([], intTyc)
  fun listTy t   = FLAST.ConTy ([t], listTyc)
  fun parrayTy t = FLAST.ConTy ([t], parrayTyc)


end
