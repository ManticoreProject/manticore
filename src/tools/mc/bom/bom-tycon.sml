(* bom-tycon.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utility code for datatypes in BOM.
 *)

structure BOMTyCon : sig

    val newDataTyc : (string * int) -> BOMTy.tyc
    val tycName : BOMTy.tyc -> string
    val nCons : BOMTy.tyc -> int

    val newDataCon : BOMTy.tyc -> (string * BOMTy.dcon_rep * BOMTy.ty list) -> BOMTy.data_con
    val dconName : BOMTy.data_con -> string
    val dconArgTy : BOMTy.data_con -> BOMTy.ty list
    val dconTyc : BOMTy.data_con -> BOMTy.tyc
    val dconResTy : BOMTy.data_con -> BOMTy.ty	(* the tyc as a ty *)

  (* new exception datacon *)
    val newExnCon : (string * BOMTy.ty list) -> BOMTy.data_con

  end = struct

    datatype tyc = datatype BOMTy.tyc
    datatype data_con = datatype BOMTy.data_con
    datatype dcon_rep = datatype BOMTy.dcon_rep

  (* create a new datatype tycon *)
    fun newDataTyc (name, nNullary) = DataTyc{
	    name = name,
	    stamp = Stamp.new(),
	    nNullary = nNullary,
	    cons = ref[],
	    rep = ref BOMTy.T_Any,	(* first approximation *)
	    kind = ref BOMTy.K_UNIFORM	(* first approximation *)
	  }

  (* add a data constructor to a datatype tycon *)
    fun newDataCon (tyc as DataTyc{cons, ...}) (name, rep, argTy) = let
	  val dc = DCon{
		  name = name,
		  stamp = Stamp.new(),
		  rep = rep,
		  argTy = argTy,
		  myTyc = tyc
		}
	  in
	    cons := !cons @ [dc];
	    dc
	  end

    fun tycName (DataTyc{name, ...}) = name
      | tycName (AbsTyc{name, ...}) = name
    fun nCons (DataTyc{cons, ...}) = List.length(!cons)

    fun dconName (DCon{name, ...}) = name
    fun dconArgTy (DCon{argTy, ...}) = argTy
    fun dconTyc (DCon{myTyc, ...}) = myTyc
    fun dconResTy (DCon{myTyc, ...}) = BOMTy.T_TyCon myTyc

    fun newExnCon (name, tys) = DCon{
	    name = name,
	    stamp = Stamp.new(),
	    rep = Tuple,
	    argTy = tys,
	    myTyc = BOMTy.exnTyc
	  }

  end
