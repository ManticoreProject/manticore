(* bom-basis.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Predefined datatypes.
 *)

signature BOM_BASIS =
  sig

    val boolTy : BOMTy.ty

  end

structure BOMBasis : BOM_BASIS =
  struct

    structure BTy = BOMTy

    val boolTyc = let
	  val boolTyc as BTy.DataTyc{rep, ...} = BOMTyCon.newDataTyc ("bool", 2)
	  in
	    rep := BTy.T_Enum 0w1;
	    boolTyc
	  end
    val boolTy = BTy.T_TyCon boolTyc
    val boolFalse = BOMTyCon.newDataCon boolTyc ("FALSE", BTy.Enum 0w0, [])
    val boolTrue = BOMTyCon.newDataCon boolTyc ("TRUE", BTy.Enum 0w1, [])

  end
