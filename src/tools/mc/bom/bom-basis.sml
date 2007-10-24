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
    val stringTy : BOMTy.ty

  (* predefined datatypes *)
    val boolTy : BOMTy.ty
    val listTy : BOMTy.ty
    val optionTy : BOMTy.ty
    val parrayTy : BOMTy.ty
    val ropeTy : BOMTy.ty
    val signalTy : BOMTy.ty
    val rdyqItemTy : BOMTy.ty
    val workQueueTy : BOMTy.ty
    val evtTy : BOMTy.ty
    val chanTy : BOMTy.ty

  (* predefined data constructors *)
    val signalPREEMPT : BOMTy.data_con
    val listCons : BOMTy.data_con
    val optionSOME : BOMTy.data_con
    val ropeLeaf : BOMTy.data_con
    val ropeCat : BOMTy.data_con
    val rdyq_itemQITEM : BOMTy.data_con
    val evtCHOOSE : BOMTy.data_con
    val evtBEVT : BOMTy.data_con

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

  (* predefined types *)
    local
      fun wrap ty = BOMTyUtil.wrap(BTy.T_Raw ty)
    in
    val intTy = wrap BTy.T_Int
    val longTy = wrap BTy.T_Long
    val floatTy = wrap BTy.T_Float
    val doubleTy = wrap BTy.T_Double
    val stringTy = BTy.T_Tuple(false, [BTy.T_Any, BTy.T_Raw BTy.T_Int])
    end

  (* ready queue items *)
    val rdyqItemTyc = BOMTyCon.newDataTyc ("rdyq_item", 1)
    val rdyqItemTy = BTy.T_TyCon rdyqItemTyc
    val rdyq_itemQITEM = BOMTyCon.newDataCon rdyqItemTyc
	  ("QITEM", BTy.Tuple, [tidTy, fiberTy, rdyqItemTy])

  (* other predefined datatypes *)
    val signalTyc = BOMTyCon.newDataTyc ("signal", 1) 
    val signalTy = BTy.T_TyCon signalTyc
    val signalPREEMPT = BOMTyCon.newDataCon signalTyc ("PREEMPT", BTy.Transparent, [fiberTy])
    val listTyc = BOMTyCon.newDataTyc ("list", 1)
    val listTy = BTy.T_TyCon listTyc
    val listCons = BOMTyCon.newDataCon listTyc
	  ("CONS", BTy.Tuple, [BTy.T_Any, listTy])
    val optionTyc = BOMTyCon.newDataTyc ("option", 1)
    val optionTy = BTy.T_TyCon optionTyc
    val optionSOME = BOMTyCon.newDataCon optionTyc
	  ("SOME", BTy.Tuple, [BTy.T_Any])
    val parrayTyc = BOMTyCon.newDataTyc ("parray", 1)
    val parrayTy = BTy.T_TyCon parrayTyc
    val ropeTyc = BOMTyCon.newDataTyc ("rope", 0)
    val ropeTy = BTy.T_TyCon ropeTyc
    val ropeLeaf = BOMTyCon.newDataCon ropeTyc
          ("LEAF", BTy.TaggedTuple 0w0, [intTy, listTy])
    val ropeCat = BOMTyCon.newDataCon ropeTyc
          ("CAT", BTy.TaggedTuple 0w1, [intTy, intTy, ropeTy, ropeTy])
    val sigactTy = BTy.T_Cont[signalTy]

  (* dirty flags *)
    val dirtyFlagTy = BTy.T_Tuple(true, [BTy.T_Enum(0w2)])

  (* primitive event values *)
    val evtTyc = BOMTyCon.newDataTyc ("evt", 0)
    val evtTy = BTy.T_TyCon evtTyc
    val evtCHOOSE = BOMTyCon.newDataCon evtTyc ("CHOOSE", BTy.TaggedTuple 0w0, [evtTy, evtTy])
    val evtBEVT = BOMTyCon.newDataCon evtTyc ("BEVT", BTy.TaggedTuple 0w0, [
	  (* pollFn : unit -> bool *)
	    BTy.T_Fun([unitTy], [exhTy], [boolTy]),
          (* doFn : 'a cont -> unit *)
	    BTy.T_Fun([BTy.T_Cont[BTy.T_Any]], [exhTy], [unitTy]),
          (* blockFn : (bool ref * 'a cont) -> unit *)
	    BTy.T_Fun([dirtyFlagTy, BTy.T_Cont[BTy.T_Any]], [exhTy], [unitTy])
	  ])

  (* The BOM type for channels.  This definition must match that given in
   *
   *	src/lib/hlops/events.def
   *)
    val chanTy = BTy.T_Tuple(true, [
	    boolTy,			(* spinlock *)
	    listTy, listTy,		(* send queue (head and tail) *)
	    listTy, listTy		(* recv queue (head and tail) *)
	  ])


    (* val workQueueTyc = BTy.AbsTyc {name="work_queue", stamp=Stamp.new(), arity=0} *)
    val workQueueTy = BTy.T_Any (* BTy.T_TyCon workQueueTyc *)

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
            optionTyc,
	    rdyqItemTyc,
	    signalTyc
	  ]

  (* Data-constructor table *)
    val findDCon : Atom.atom -> BOMTy.data_con option = mkTbl (Atom.atom o BOMTyCon.dconName) [
	    listCons,
            optionSOME,
            ropeLeaf,
            ropeCat,
	    rdyq_itemQITEM,
	    signalPREEMPT
	  ]

  end
