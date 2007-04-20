(* bom-tycon.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utility code for datatypes in BOM.
 *)

structure BOMTyCon =
  struct

    datatype tyc = datatype BOMTy.tyc
    datatype data_con = datatype BOMTy.data_con
    datatype dcon_rep = datatype BOMTy.dcon_rep

    fun toString (DataTyc{name, ...}) = name

  (* convert datatypes to their representation types *)
    fun toRepTy (DataTyc{nNullary=0, cons=[DCon{argTy, ...}], ...}) = argTy
      | toRepTy (DataTyc{nNullary, cons=[], ...}) = BOMTy.T_Enum(Word.fromInt nNullary - 0w1)
(* FIXME: we need a union type in BOM for this situation *)
      | toRepTy _ = BOMTy.T_Any

  end
