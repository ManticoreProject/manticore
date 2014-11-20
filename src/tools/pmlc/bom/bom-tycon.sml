(* bom-tycon.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utility code for datatypes in BOM.
 *)

structure BOMTyCon : sig

    type tyc
    type data_con

    val newDataTyc : (string * int) -> tyc
    val tycName : tyc -> string
    val nCons : tyc -> int
    val sameTyc : (tyc * tyc) -> bool
  (* per-tyc properties *)
    val newTycProp : ( tyc -> 'a) -> {
	    clrFn : tyc -> unit,
	    getFn : tyc -> 'a,
	    peekFn : tyc -> 'a option,
	    setFn : (tyc * 'a) -> unit
	  }
    val newTycFlag : unit -> {
	    getFn : tyc -> bool,
	    setFn : tyc * bool -> unit
	  }

  (* create a new data constructor for the tyc *)
    val newDataCon : tyc -> (string * BOMTy.ty list) -> data_con

    val dconName : data_con -> string
    val dconArgTy : data_con -> BOMTy.ty list
    val dconTyc : data_con -> tyc
    val dconResTy : data_con -> BOMTy.ty	(* the tyc as a ty *)
    val dconSame : data_con * data_con -> bool
  (* per-con properties *)
    val newConProp : (data_con -> 'a) -> {
	    clrFn : data_con -> unit,
	    getFn : data_con -> 'a,
	    peekFn : data_con -> 'a option,
	    setFn : (data_con * 'a) -> unit
	  }
    val newConFlag : unit -> {
	    getFn : data_con -> bool,
	    setFn : data_con * bool -> unit
	  }

  end = struct

    datatype tyc = datatype BOMTy.tyc
    datatype data_con = datatype BOMTy.data_con

  (* create a new datatype tycon *)
    fun newDataTyc (name, nNullary) = DataTyc{
	    name = name,
	    stamp = Stamp.new(),
	    nNullary = nNullary,
	    cons = ref[],
	    props = PropList.newHolder()
	  }

    local
      fun propsOf (DataTyc{props, ...}) = props
    in
    fun newTycProp mkProp = PropList.newProp (propsOf, mkProp)
    fun newTycFlag () = PropList.newFlag propsOf
    end (* local *)

  (* add a data constructor to a datatype tycon *)
    fun newDataCon (tyc as DataTyc{cons, ...}) (name, argTy) = let
	  val dc = DCon{
		  name = name,
		  stamp = Stamp.new(),
		  argTy = argTy,
		  myTyc = tyc,
		  props = PropList.newHolder()
		}
	  in
	    cons := !cons @ [dc];
	    dc
	  end

    fun tycName (DataTyc{name, ...}) = name
    fun nCons (DataTyc{cons, ...}) = List.length(!cons)

    fun sameTyc (DataTyc{stamp = a, ...}, DataTyc{stamp = b, ...}) = Stamp.same (a, b)

    fun dconName (DCon{name, ...}) = name
    fun dconArgTy (DCon{argTy, ...}) = argTy
    fun dconTyc (DCon{myTyc, ...}) = myTyc
    fun dconResTy (DCon{myTyc, ...}) = BOMTy.T_Con(myTyc, [])

    fun dconSame (DCon{stamp=a, ...}, DCon{stamp=b, ...}) = Stamp.same(a, b)

    local
      fun propsOf (DCon{props, ...}) = props
    in
    fun newConProp mkProp = PropList.newProp (propsOf, mkProp)
    fun newConFlag () = PropList.newFlag propsOf
    end (* local *)

  end
