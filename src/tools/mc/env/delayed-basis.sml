(* delayed-basis.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Items from the basis needed for the flattening transformation's implementation.
 *
 *)

structure DelayedBasis = struct

  structure T = Types
  structure BE = BasisEnv

  type path = string list

  type 'a thunk = 'a Memo.thunk
  fun delay f x = fn () => f x

  local
    val getVar  = BE.getVarFromBasis
    val getTyc  = BE.getTyConFromBasis
    val getDCon = BE.getDConFromBasis
   (* lists of all components in the delayed basis *)
    val tyConsListRef = ref ([] : path list)
    val dConsListRef  = ref ([] : path list)
    val varsListRef   = ref ([] : path list)
   (* record the paths to components in the delayed basis *)
    fun regTyc p  = tyConsListRef := (p :: (!tyConsListRef))
    fun regDCon p = dConsListRef  := (p :: (!dConsListRef))
    fun regVar p  = varsListRef   := (p :: (!varsListRef))
  in
  (* use these to create thunks for delayed basis items *)
    fun memoTyc p  = (regTyc p; Memo.new (delay getTyc p))
    fun memoDCon p = (regDCon p; Memo.new (delay getDCon p)) 
    fun memoVar p  = (regVar p; Memo.new (delay getVar p))
  (* lists of all paths in the delayed basis *)
    fun allTyCons ()   = !tyConsListRef
    fun allDataCons () = !dConsListRef
    fun allVars ()     = !varsListRef
  end

(* tycons *)
  structure TyCon = struct
    val option     = memoTyc ["Option", "option"]
    val refTyc     = memoTyc ["Ref", "ref"] (* "ref" is syntax in sml, so we call this refTyc *)
    val result     = memoTyc ["Result", "result"]
    val mvar       = memoTyc ["MVar", "mvar"]
    val array      = memoTyc ["Array", "array"]
    val cancel     = memoTyc ["Cancelation", "cancelable"]
    val bitvec     = memoTyc ["BitVec", "bitvec"]
    val future     = memoTyc ["MultilispFuture", "future"]
    val rope       = memoTyc ["Rope", "rope"]
    val list_seq   = memoTyc ["ListSeq", "seq"]
    val farray     = memoTyc ["FArray", "f_array"]
    val shape_tree = memoTyc ["ShapeTree", "shape_tree"]
    val int_farray = memoTyc ["IntFArray", "int_farray"]
  end

(* dcons *)
  structure DataCon = struct
    val optSOME   = memoDCon ["Option", "SOME"]
    val optNONE   = memoDCon ["Option", "NONE"]
    val resultRES = memoDCon ["Result", "RES"]
    val resultEXN = memoDCon ["Result", "EXN"]
    val ropeLEAF  = memoDCon ["Rope", "LEAF"]
    val ropeCAT   = memoDCon ["Rope", "CAT"]
    val farray    = memoDCon ["FArray", "FArray"]
    val intFArray = memoDCon ["IntFArray", "FArray"]
    val lf        = memoDCon ["ShapeTree", "Lf"]
    val nd        = memoDCon ["ShapeTree", "Nd"]
  end

(* vars *)
  structure Var = struct

    val listRev       = memoVar ["List", "rev"]

    val refNew        = memoVar ["Ref", "new"]
    val refGet        = memoVar ["Ref", "get"]
    val refSet        = memoVar ["Ref", "set"]

    val mvarNew       = memoVar ["MVar", "new"]
    val mvarPut       = memoVar ["MVar", "put"]
    val mvarTake      = memoVar ["MVar", "take"]

    val arrayArray    = memoVar ["Array", "array"]
    val arrayLength   = memoVar ["Array", "length"]
    val arraySub      = memoVar ["Array", "sub"]
    val arrayUpdate   = memoVar ["Array", "update"]

    val cancelNew     = memoVar ["Cancelation", "new"]
    val cancelCancel  = memoVar ["Cancelation", "cancel"]
    val cancelSpawn   = memoVar ["Cancelation", "cancel"]

    val bitvecNew     = memoVar ["BitVec", "new"]
    val bitvecSet0    = memoVar ["BitVec", "set0"]
    val bitvecSet1    = memoVar ["BitVec", "set1"]
    val bitvecSet0F   = memoVar ["BitVec", "set0F"]
    val bitvecSet1F   = memoVar ["BitVec", "set1F"]
    val bitvecEq      = memoVar ["BitVec", "eq"]

    val future1       = memoVar ["MultilispFuture", "future"]
    val touch1        = memoVar ["MultilispFuture", "touch"]
    val cancel1       = memoVar ["MultilispFuture", "cancel"]
    val poll          = memoVar ["MultilispFuture", "poll"]

    val ropeEmpty     = memoVar ["Rope", "empty"]
    val ropeSingleton = memoVar ["Rope", "singleton"]
    val ropeFromList  = memoVar ["Rope", "fromList"]
    val ropeTabFT     = memoVar ["Rope", "tabFromToP"]
    val ropeTabFTS    = memoVar ["Rope", "tabFromToStepP"]
    val ropeSub       = memoVar ["Rope", "sub"]
    val ropeLength    = memoVar ["Rope", "length"]
    val ropeRange     = memoVar ["Rope", "rangeP"]
    val ropeRangeNS   = memoVar ["Rope", "rangePNoStep"]
    val ropeMapP      = memoVar ["Rope", "mapP"]

    val lseqToList    = memoVar ["ListSeq", "toList"]
    val lseqFromList  = memoVar ["ListSeq", "fromList"]

    val parrayMap     = memoVar ["PArray", "map"]
    val parraySub     = memoVar ["PArray", "sub"]
    val parrayTabFTS  = memoVar ["PArray", "tabFromToStep"]
    val parrayRange   = memoVar ["PArray", "range"]

    val flatSub       = memoVar ["FArray", "flatSub"]
    val nestedSub     = memoVar ["FArray", "nestedSub"]
    val flen          = memoVar ["FArray", "length"]
    val ftab          = memoVar ["FArray", "tab"]
    val ftabFTS       = memoVar ["FArray", "tabFromToStep"]
    val fflatten      = memoVar ["FArray", "flatten"]
    val fmap          = memoVar ["FArray", "flatMap"]
    val greduce       = memoVar ["FArray", "groundReduce"]
    val fapp          = memoVar ["FArray", "app"]

    val fptab         = memoVar ["FArrayPair", "tabulate"]
    val fptabFTS      = memoVar ["FArrayPair", "tabFromToStep"]
    val fpmap         = memoVar ["FArrayPair", "flatMapEq"]

    val ifReduce      = memoVar ["IntFArray", "reduce"]
    val intRange      = memoVar ["IntFArray", "intRange"]
    val intLen        = memoVar ["IntFArray", "length"]
    val intTab        = memoVar ["IntFArray", "tab"]
    val intTabFTS     = memoVar ["IntFArray", "tabFromToStep"]
    val intFlatSub    = memoVar ["IntFArray", "flatSub"]
    val ifFromList    = memoVar ["IntFArray", "fromList"]
    val ifmap         = memoVar ["IntFArray", "flatMap"]

    val ifpTab        = memoVar ["IntFArrayPair", "tabulate"]
    val ifpTabFTS     = memoVar ["IntFArrayPair", "tabFromToStep"]
    val ipMapEq_int   = memoVar ["IntFArrayPair", "flatMapEq_int"]
    val ipReduce      = memoVar ["IntFArrayPair", "flatReduce"]

    val flattenIFF    = memoVar ["FArrayUtil", "flatten_IF_F"]
    val mapIFPoly     = memoVar ["FArrayUtil", "map_IF_poly"]

  end

  structure Ty = struct
    local
      fun con0 tyc = T.ConTy ([], tyc)
      fun con1 (t, tyc) = T.ConTy ([t], tyc)
    in
    fun int_farray () = con0 (TyCon.int_farray ())
    fun farray t = con1 (t, TyCon.farray ())
    fun option t = con1 (t, TyCon.option ())
    fun ref t = con1 (t, TyCon.refTyc ())
    fun result t = con1 (t, TyCon.result ())
    fun mvar t = con1 (t, TyCon.mvar ())
    fun array t = con1 (t, TyCon.array ())
    fun cancel () = con0 (TyCon.cancel ())
    fun bitvec () = con0 (TyCon.bitvec ())
    fun future t = con1 (t, TyCon.future ())
    fun rope t = con1 (t, TyCon.rope ())
    fun shape_tree () = con0 (TyCon.shape_tree ())
    end (* local *)
  end

end
