(* bom-exn.sml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMExn : sig

  (* the unique datatype for all exceptions.  The translation from SXML should map the
   * the datatype created by the ImplementExceptions functor (mlton/xml/implement-exceptions.fun)
   * to this BOM datatype.
   *)
    val tyc : BOMTy.tyc

  (* new exception datacon *)
    val addCon : string * BOMTy.ty list -> BOMTy.data_con

  (* is a BOM data constructor an exception constructor? *)
    val isExnCon : BOMTy.data_con -> bool

  end = struct

    val tyc = BOMTyCon.newDataTyc ("exn", 0)

    val addCon = BOMTyCon.newDataCon tyc

  (* is a BOM data constructor an exception constructor *)
    fun isExnCon (BOMTy.DCon{myTyc, ...}) = BOMTyCon.sameTyc(myTyc, tyc)

  end

