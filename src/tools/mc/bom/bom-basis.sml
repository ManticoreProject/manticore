(* bom-basis.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Predefined datatypes.
 *)

signature BOM_BASIS =
  sig

  (* predefined types *)
    val intTy : BOMTy.ty
    val longTy : BOMTy.ty
    val floatTy : BOMTy.ty
    val doubleTy : BOMTy.ty
    val stringLenTy : BOMTy.ty	(* the length field in the ML string rep *)
    val stringTy : BOMTy.ty

  (* predefined datatypes *)
    val boolTy : BOMTy.ty
    val listTy : BOMTy.ty
    val optionTy : BOMTy.ty
(*
    val parrayTy : BOMTy.ty
    val ropeTy : BOMTy.ty
    val signalTy : BOMTy.ty
    val rdyqItemTy : BOMTy.ty
    val workQueueTy : BOMTy.ty
    val ivarTy : BOMTy.ty
    val evtTy : BOMTy.ty
    val chanTy : BOMTy.ty
*)

  (* predefined data constructors *)
    val boolFalse : BOMTy.data_con
    val boolTrue : BOMTy.data_con
    val listNil : BOMTy.data_con
    val listCons : BOMTy.data_con
    val optionNONE : BOMTy.data_con
    val optionSOME : BOMTy.data_con

  (* predefined exception constructors *)
    val exnFail : BOMTy.data_con

    val findTyc : Atom.atom -> BOMTy.tyc option
    val findDCon : Atom.atom -> BOMTy.data_con option

  end

structure BOMBasis : BOM_BASIS =
  struct

    structure BTy = BOMTy
    structure H = HLOp

    fun new (name, params, res, attrs) =
	  H.new(Atom.atom name, {params= List.map HLOp.PARAM params, exh=[], results=res}, attrs)

  (* some standard parameter types *)
    val unitTy = BTy.unitTy
    val exnTy = BTy.exnTy
    val exhTy = BTy.exhTy

  (* predefined types *)
    local
      fun wrap ty = BOMTyUtil.wrap(BTy.T_Raw ty)
    in
    val rawIntTy = BTy.T_Raw BTy.T_Int
    val intTy = wrap BTy.T_Int
    val longTy = wrap BTy.T_Long
    val floatTy = wrap BTy.T_Float
    val doubleTy = wrap BTy.T_Double
(* FIXME: the length type should be architecture dependent! *)
    val stringLenTy = rawIntTy
    val stringTy = BTy.T_Tuple(false, [BTy.T_Any, stringLenTy])
    end

    val boolTyc = let
	  val boolTyc as BTy.DataTyc{rep, ...} = BOMTyCon.newDataTyc ("bool", 2)
	  in
	    rep := BTy.T_Enum 0w1;
	    boolTyc
	  end
    val boolTy = BTy.T_TyCon boolTyc
    val boolFalse = BOMTyCon.newDataCon boolTyc ("FALSE", BTy.Enum 0w0, [])
    val boolTrue = BOMTyCon.newDataCon boolTyc ("TRUE", BTy.Enum 0w1, [])

    val listTyc = BOMTyCon.newDataTyc ("list", 1)
    val listTy = BTy.T_TyCon listTyc
    val listNil = BOMTyCon.newDataCon listTyc
	  ("NIL", BTy.Enum 0w0, [])
    val listCons = BOMTyCon.newDataCon listTyc
	  ("CONS", BTy.Tuple, [BTy.T_Any, listTy])

    val optionTyc = BOMTyCon.newDataTyc ("option", 1)
    val optionTy = BTy.T_TyCon optionTyc
    val optionNONE= BOMTyCon.newDataCon optionTyc
	  ("NONE", BTy.Enum 0w0, [])
    val optionSOME = BOMTyCon.newDataCon optionTyc
	  ("SOME", BTy.Tuple, [BTy.T_Any])

  (* predefined exception constructors *)
    val exnFail = BOMTyCon.newExnCon ("Fail", [stringTy])

    fun mkTbl nameOf bindings = let
	  val tbl = AtomTable.mkTable (List.length bindings, Fail "table")
	  fun ins v = AtomTable.insert tbl (nameOf v, v)
	  in
	    List.app ins bindings;
	    AtomTable.find tbl
	  end

  (* Type-constructor table *)
    val findTyc : Atom.atom -> BOMTy.tyc option = mkTbl (Atom.atom o BOMTyCon.tycName) [
	    boolTyc,
	    listTyc,
            optionTyc
	  ]

  (* Data-constructor table *)
    val findDCon : Atom.atom -> BOMTy.data_con option = mkTbl (Atom.atom o BOMTyCon.dconName) [
	    boolFalse,
	    boolTrue,
	    listNil,
	    listCons,
	    optionNONE,
            optionSOME,
	    exnFail
	  ]

  end
