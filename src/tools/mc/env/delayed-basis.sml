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
    val getHLOp = BE.getHLOpFromBasis
   (* lists of all components in the delayed basis *)
    val tyConsListRef = ref ([] : path list)
    val dConsListRef  = ref ([] : path list)
    val varsListRef   = ref ([] : path list)
    val hlopsListRef  = ref ([] : path list)
   (* record the paths to components in the delayed basis *)
    fun regTyc p  = tyConsListRef := (p :: (!tyConsListRef))
    fun regDCon p = dConsListRef  := (p :: (!dConsListRef))
    fun regVar p  = varsListRef   := (p :: (!varsListRef))
    fun regHLOp p = hlopsListRef  := (p :: (!hlopsListRef))
  in
  (* use these to create thunks for delayed basis items *)
    fun memoTyc p  = (regTyc p; Memo.new (delay getTyc p))
    fun memoDCon p = (regDCon p; Memo.new (delay getDCon p)) 
    fun memoVar p  = (regVar p; Memo.new (delay getVar p))
    fun memoHLOp p = (regHLOp p; Memo.new (delay getHLOp p))
  (* lists of all paths in the delayed basis *)
    fun allTyCons ()   = !tyConsListRef
    fun allDataCons () = !dConsListRef
    fun allVars ()     = !varsListRef
    fun allHLOps ()    = !hlopsListRef
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
    val int_rope   = memoTyc ["IntRope", "int_rope"]
    val dbl_rope   = memoTyc ["DoubleRope", "double_rope"]
    val farray     = memoTyc ["FArray", "farray"]
    val shape      = memoTyc ["Shape", "shape"]
    val int_farray = memoTyc ["IntFArray", "int_farray"]
    val dbl_farray = memoTyc ["DoubleFArray", "double_farray"]
    val list_seq   = memoTyc ["ListSeq", "seq"]
    val arr_seq    = memoTyc ["ArraySeq", "seq"]
    val int_seq    = memoTyc ["IntSeq", "int_seq"]
    val dbl_seq    = memoTyc ["DoubleSeq", "double_seq"]
  end

(* dcons *)
  structure DataCon = struct
    val optSOME   = memoDCon ["Option", "SOME"]
    val optNONE   = memoDCon ["Option", "NONE"]
    val resultRES = memoDCon ["Result", "RES"]
    val resultEXN = memoDCon ["Result", "EXN"]
    val ropeLeaf  = memoDCon ["Rope", "Leaf"]
    val ropeCat   = memoDCon ["Rope", "Cat"]
    val intLeaf   = memoDCon ["IntRope", "Leaf"]
    val intCat    = memoDCon ["IntRope", "Cat"]
    val dblLeaf   = memoDCon ["DoubleRope", "Leaf"]
    val dblCat    = memoDCon ["DoubleRope", "Cat"]
    val farray    = memoDCon ["FArray", "FArray"]
    val intFArray = memoDCon ["IntFArray", "FArray"]
    val dblFArray = memoDCon ["DoubleFArray", "FArray"]
    val lf        = memoDCon ["Shape", "Lf"]
    val nd        = memoDCon ["Shape", "Nd"]
  end

(* hlops *)
  structure HLOp = struct
    val cancelationCancel          = memoHLOp ["Cancelation", "cancel"]
    val cancelationNew             = memoHLOp ["Cancelation", "new"]
    val implicitThreadNewThread    = memoHLOp ["ImplicitThread", "new-thread"]
    val implicitThreadRemoveThread = memoHLOp ["ImplicitThread", "remove-thread-b"]
    val implicitThreadSpawnThread  = memoHLOp ["ImplicitThread", "spawn-thread"]
    val schedulerActionStop        = memoHLOp ["SchedulerAction", "stop"]
    val threadsLocalSpawn          = memoHLOp ["Threads", "local-spawn"]
  end

(* vars *)
  structure Var = struct

    val listRev       = memoVar ["List", "rev"]
    val listTab       = memoVar ["List", "tabulate"]

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

    val maxLeafSize   = memoVar ["LeafSize", "getMax"]

    val rngNElts      = memoVar ["Range", "nElts"]

    val ropeEmpty     = memoVar ["Rope", "empty"]
    val ropeSingleton = memoVar ["Rope", "singleton"]
    val ropeFilterP   = memoVar ["Rope", "filterP"]
    val ropeFromList  = memoVar ["Rope", "fromList"]
    val ropeTabFT     = memoVar ["Rope", "tabFromToP"]
    val ropeTabFTS    = memoVar ["Rope", "tabFromToStepP"]
    val ropeSub       = memoVar ["Rope", "sub"]
    val ropeLength    = memoVar ["Rope", "length"]
    val ropeRange     = memoVar ["Rope", "rangeP"]
    val ropeRangeNS   = memoVar ["Rope", "rangePNoStep"]
    val ropeMap       = memoVar ["Rope", "mapUncurried"]
    val ropeFromSeq   = memoVar ["Rope", "fromSeq"]
    val ropeConcat    = memoVar ["Rope", "concat"]
    
    val ropePairMapP  = memoVar ["RopePair", "mapP"]

    val ropeMap_int   = memoVar ["RopeUtil", "map_int"]
    val ropeMap_dbl   = memoVar ["RopeUtil", "map_double"]

    val irEmpty       = memoVar ["IntRope", "empty"]
    val irLength      = memoVar ["IntRope", "length"]
    val irFromSeq     = memoVar ["IntRope", "fromSeq"]
    val irConcat      = memoVar ["IntRope", "concat"]

    val drEmpty       = memoVar ["DoubleRope", "empty"]
    val drLength      = memoVar ["DoubleRope", "length"]
    val drFromSeq     = memoVar ["DoubleRope", "fromSeq"]
    val drConcat      = memoVar ["DoubleRope", "concat"]

    val lseqToList    = memoVar ["ListSeq", "toList"]
    val lseqFromList  = memoVar ["ListSeq", "fromList"]

    val arrSeqSub     = memoVar ["ArraySeq", "sub"]
    val arrSeqTab     = memoVar ["ArraySeq", "tab"]
    val arrSeqUpd     = memoVar ["ArraySeq", "update"]
    val arrSeqEmpty   = memoVar ["ArraySeq", "empty"]

    val iseqSub       = memoVar ["IntSeq", "sub"]
    val iseqTab       = memoVar ["IntSeq", "tabulate"]
    val iseqUpd       = memoVar ["IntSeq", "update"]
    val iseqEmpty     = memoVar ["IntSeq", "empty"]
    val iseqCreate    = memoVar ["IntSeq", "unsafeCreate"]

    val dseqSub       = memoVar ["DoubleSeq", "sub"]
    val dseqTab       = memoVar ["DoubleSeq", "tabulate"]
    val dseqUpd       = memoVar ["DoubleSeq", "update"]
    val dseqEmpty     = memoVar ["DoubleSeq", "empty"]
    val dseqCreate    = memoVar ["DoubleSeq", "unsafeCreate"]

    val parrayMap     = memoVar ["PArray", "map"]
    val parraySub     = memoVar ["PArray", "sub"]
    val parrayTab     = memoVar ["PArray", "tab"]
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

    val intReduce     = memoVar ["IntFArray", "reduce"]
    val intRange      = memoVar ["IntFArray", "intRange"]
    val intLen        = memoVar ["IntFArray", "length"]
    val intTab        = memoVar ["IntFArray", "tab"]
    val intTabFTS     = memoVar ["IntFArray", "tabFromToStep"]
    val intFlatSub    = memoVar ["IntFArray", "flatSub"]
    val intNestedSub  = memoVar ["IntFArray", "nestedSub"]
    val intMap        = memoVar ["IntFArray", "flatMap"]
    val ifFromList    = memoVar ["IntFArray", "fromList"]

    val dblLen        = memoVar ["DoubleFArray", "length"]
    val dblNestedSub  = memoVar ["DoubleFArray", "nestedSub"]
    val dblFlatSub    = memoVar ["DoubleFArray", "flatSub"]
    val dblTab        = memoVar ["DoubleFArray", "tab"]
    val dblTabFTS     = memoVar ["DoubleFArray", "tabFromToStep"]
    val dblMap        = memoVar ["DoubleFArray", "flatMap"]
    val dblReduce     = memoVar ["DoubleFArray", "reduce"]
    val dfFromList    = memoVar ["DoubleFArray", "fromList"]

    val ifpTab        = memoVar ["IntFArrayPair", "tabulate"]
    val ifpTabFTS     = memoVar ["IntFArrayPair", "tabFromToStep"]
    val ipMapEq_int   = memoVar ["IntFArrayPair", "flatMapEq_int"]
    val ipReduce      = memoVar ["IntFArrayPair", "flatReduce"]

    val flattenIFF    = memoVar ["FArrayUtil", "flatten_IF_F"]
    val mapIFPoly     = memoVar ["FArrayUtil", "map_IF_poly"]

    val shapeSame     = memoVar ["Shape", "same"]

    val println       = memoVar ["Print", "printLn"]

  end

  structure Ty = struct
    local
      fun con0 tyc = T.ConTy ([], tyc ())
      fun con1 (t, tyc) = T.ConTy ([t], tyc ())
    in
    fun rope t = con1 (t, TyCon.rope)
    fun int_rope () = con0 TyCon.int_rope
    fun dbl_rope () = con0 TyCon.dbl_rope
    fun farray t = con1 (t, TyCon.farray)
    fun int_farray () = con0 TyCon.int_farray
    fun dbl_farray () = con0 TyCon.dbl_farray
    fun option t = con1 (t, TyCon.option)
    fun ref t = con1 (t, TyCon.refTyc)
    fun result t = con1 (t, TyCon.result)
    fun mvar t = con1 (t, TyCon.mvar)
    fun array t = con1 (t, TyCon.array)
    fun cancel () = con0 TyCon.cancel
    fun bitvec () = con0 TyCon.bitvec
    fun future t = con1 (t, TyCon.future)
    fun shape () = con0 TyCon.shape
    fun arr_seq t = con1 (t, TyCon.arr_seq)
    fun int_seq () = con0 TyCon.int_seq
    fun dbl_seq () = con0 TyCon.dbl_seq
    end (* local *)
  end

end
