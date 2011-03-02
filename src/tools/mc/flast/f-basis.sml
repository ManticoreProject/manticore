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
  val parrayTyc   : FTTypes.tycon

(* types *)
  val threadIdTy : FTTypes.ty
  val parrayTy   : FTTypes.ty -> FTTypes.ty

end = struct

  structure F = FLAST
  structure T = FTTypes
  structure N = BasisNames

(* type constructors *)
  val threadIdTyc = FTTyCon.newAbsTyc (N.thread_id, 0, true, Basis.threadIdTyc)

  val parrayTyc = let
    val alpha = TyVar.new (Atom.atom "'a")
    in
      FTTyCon.newDataTyc (N.parray, [alpha], Basis.parrayTyc)
    end

(* types *)
  val threadIdTy = FLAST.ConTy ([], threadIdTyc)
  fun parrayTy t = FLAST.ConTy ([t], parrayTyc)

end
