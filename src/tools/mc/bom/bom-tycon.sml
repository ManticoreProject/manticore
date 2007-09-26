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
    fun nCons (DataTyc{cons, ...}) = List.length(!cons)

    fun dconName (DCon{name, ...}) = name
    fun dconArgTy (DCon{argTy, ...}) = argTy
    fun dconTyc (DCon{myTyc, ...}) = myTyc
    fun dconResTy (DCon{myTyc, ...}) = BOMTy.T_TyCon myTyc

  end
