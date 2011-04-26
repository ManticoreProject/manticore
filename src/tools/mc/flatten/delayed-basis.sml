(* delayed-basis.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Items from the basis needed for the flattening transformation's implementation.
 *
 *)

structure DelayedBasis = struct

  structure BE = BasisEnv

  type 'a thunk = unit -> 'a

  fun delay f x = fn () => f x

(* memo : 'a thunk -> 'a thunk *)
(* Creates a memoized ref cell containing the result, which is computed  *)
(*   on demand the first time, and retrieved from the cell subsequently. *)
  val memo : 'a thunk -> 'a thunk = Memo.new'

  val getVar  = BE.getVarFromBasis
  val getTyc  = BE.getTyConFromBasis
  val getDCon = BE.getDConFromBasis
 
(* path makers *)
  fun module m x = m :: [x]
  val farray = module "FArray"
  val farrayPair = module "FArrayPair"
  val shapeTree = module "ShapeTree"
  val intFArray = module "IntFArray"
  val intFArrayPair = module "IntFArrayPair"

(* tycons *)
  structure TyCon = struct
    val mk        = memo o delay getTyc
    val farray    = mk (farray "f_array")
    val shapeTree = mk (shapeTree "shape_tree")
    val intFArray = mk (intFArray "int_farray")
  end

(* dcons *)
  structure DataCon = struct
    val mk        = memo o delay getDCon
    val farray    = mk (farray "FArray")
    val intFArray = mk (intFArray "FArray")
    val lf        = mk (shapeTree "Lf")
    val nd        = mk (shapeTree "Nd")
  end

(* vars *)
  structure Var = struct
    val mk          = memo o delay getVar
    val flatSub     = mk (farray "flatSub")
    val nestedSub   = mk (farray "nestedSub")
    val flen        = mk (farray "length")
    val ftab        = mk (farray "tab")
    val ftabFTS     = mk (farray "tabFromToStep")
    val fptab       = mk (farrayPair "tab")
    val fmap        = mk (farray "flatMap")
    val fpmap       = mk (farrayPair "flatMapEq")
    val ipMapEq_int = mk (intFArrayPair "flatMapEq_int")
    val greduce     = mk (farray "groundReduce")
    val ifReduce    = mk (intFArray "reduce")
    val intRange    = mk (farray "intRange")
    val fapp        = mk (farray "app")
    val intLen      = mk (intFArray "length")
    val intTab      = mk (intFArray "tab")
    val intTabFTS   = mk (intFArray "tabFromToStep")
    val intFlatSub  = mk (intFArray "flatSub")
    val ifFromList  = mk (intFArray "fromList")
  end

end
