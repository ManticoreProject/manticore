(* bom-var.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMVar : sig

    type t (* = (var_kind, BOMTy.t) VarRep.var_rep *)

    type ty = BOMTy.t

    datatype kind
      = VK_None
      | VK_Let of BOMRep.exp
      | VK_RHS of BOMRep.rhs
      | VK_Param
      | VK_Fun of BOMRep.lambda
      | VK_Cont of BOMRep.lambda
      | VK_CFun of BOMRep.c_fun

    val kindToString : kind -> string

    val new : (string * ty) -> t
    val newWithKind : (string * kind * ty) -> t
    val copy : t -> t
    val alias : (t * string option * ty) -> t

    val nameOf : t -> string
    val kindOf : t -> kind
    val setKind : (t * kind) -> unit
    val typeOf : t -> ty
    val setType : (t * ty) -> unit

  (* operations of use counts *)
    val useCount : t -> int
    val clrCount : t -> unit
    val setCount : (t * int) -> unit
    val addToCount : (t * int) -> unit

    val same : (t * t) -> bool
    val compare : (t * t) -> order
    val hash : t -> word

    val toString : t -> string
    val varsToString : t list -> string

  (* per-variable properties *)
    val newProp : (t -> 'a) -> {
	    clrFn : t -> unit,
	    getFn : t -> 'a,
	    peekFn : t -> 'a option,
	    setFn : (t * 'a) -> unit
	  }
    val newFlag : unit -> {
	    getFn : t -> bool,
	    setFn : t * bool -> unit
	  }

    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  (* return the application count reference for a function *)
    val appCntRef : t -> int ref

  (* remove the application count of a function; i.e., set it to zero *)
    val appCntRmv : t -> unit

  (* return the application count of a function *)
    val appCntOf : t -> int

  (* add the use count of the second variable to the use count of the first variable.
   * If the variables are functions, then we also add the application count of the
   * second variable to the first variable's application count.
   *)
    val combineAppUseCnts : t * t -> unit

  end = struct

    fun kindToString BOMRep.VK_None = "None"
      | kindToString (BOMRep.VK_Let _) = "Let"
      | kindToString (BOMRep.VK_RHS _) = "RHS"
      | kindToString BOMRep.VK_Param = "Param"
      | kindToString (BOMRep.VK_Fun _) = "Fun"
      | kindToString (BOMRep.VK_Cont _) = "Cont"
      | kindToString (BOMRep.VK_CFun _) = "CFun"

    structure V = VarFn (
      struct
	type kind = BOMRep.var_kind
	type ty = BOMTy.t
	val defaultKind = BOMRep.VK_None
	val kindToString = kindToString
	val tyToString = BOMTy.toString
      end)

    open V

    type t = var

    datatype kind = datatype BOMRep.var_kind

  (* application counts for functions *)
    local
      val {clrFn, getFn, peekFn, ...} = newProp (fn _ => ref 0)
    in
    val appCntRef = getFn
    val appCntRmv = clrFn
    fun appCntOf v = (case peekFn v of NONE => 0 | (SOME ri) => !ri)
    fun combineAppUseCnts (x as VarRep.V{useCnt=ux, ...}, y as VarRep.V{useCnt=uy, ...}) = (
	  ux := !ux + !uy;
	  case peekFn y
	   of (SOME ry) => let
		val rx = appCntRef x
		in
		  rx := !rx + !ry
		end
	    | NONE => ()
	  (* end case *))
  (* override V.toString with a string representation that includes counts *)
    val toString = fn x => (case peekFn x
	   of NONE => concat[toString x, "#", Int.toString(useCount x)]
	    | SOME r => concat[
		  toString x, "#", Int.toString(useCount x),
		  ".", Int.toString(!r)
		]
	  (* end case *))
    end (* local val ... *)

  end
