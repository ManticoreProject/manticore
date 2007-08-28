(* bom-basis.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Predefined datatypes.
 *)

signature BOM_BASIS =
  sig

  (* predefined datatypes *)
    val signalTyc : BOMTy.tyc
    val listTyc : BOMTy.tyc
    val rdyqItemTyc : BOMTy.tyc

    val rdyqItemTy : BOMTy.ty

  (* predefined data constructors *)
    val preemptDC : BOMTy.data_con
    val consDC : BOMTy.data_con
    val rdyqConsDC : BOMTy.data_con

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
    val boolTy = BTy.boolTy
    val exnTy = BTy.exnTy
    val exhTy = BTy.exhTy
    val tidTy = BTy.tidTy
    val fiberTy = BTy.fiberTy

  (* ready queue items *)
    val rdyqItemTyc = BOMTyCon.newDataTyc ("rdyq_item", 1)
    val rdyqItemTy = BTy.T_TyCon rdyqItemTyc
    val rdyqConsDC = BOMTyCon.newDataCon rdyqItemTyc
	  ("QITEM", BTy.Tuple, [tidTy, fiberTy, rdyqItemTy])

  (* other predefined datatypes *)
    val signalTyc = BOMTyCon.newDataTyc ("signal", 1) 
    val signalTy = BTy.T_TyCon signalTyc
    val preemptDC = BOMTyCon.newDataCon signalTyc ("PREEMPT", BTy.Transparent, [fiberTy])
    val listTyc = BOMTyCon.newDataTyc ("list", 1)
    val listTy = BTy.T_TyCon listTyc
    val consDC = BOMTyCon.newDataCon listTyc
	  ("CONS", BTy.Tuple, [BTy.T_Any, listTy])

    val sigactTy = BTy.T_Cont[signalTy]

    fun mkTbl nameOf bindings = let
	  val tbl = AtomTable.mkTable (List.length bindings, Fail "table")
	  fun ins v = AtomTable.insert tbl (nameOf v, v)
	  in
	    List.app ins bindings;
	    AtomTable.find tbl
	  end

  (* Type-constructor table *)
    val findTyc : Atom.atom -> BOMTy.tyc option = mkTbl (Atom.atom o BOMTyCon.tycName) [
	    listTyc,
	    rdyqItemTyc,
	    signalTyc
	  ]

  (* Data-constructor table *)
    val findDCon : Atom.atom -> BOMTy.data_con option = mkTbl (Atom.atom o BOMTyCon.dconName) [
	    consDC,
	    rdyqConsDC,
	    preemptDC
	  ]

  end
