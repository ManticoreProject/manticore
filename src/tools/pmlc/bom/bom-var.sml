(* bom-var.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMVar : sig

    datatype kind
      = VK_None
      | VK_Let of exp
      | VK_RHS of rhs
      | VK_Param
      | VK_Fun of lambda
      | VK_Cont of lambda
      | VK_CFun of c_fun

    val kindToString : kind -> string

    include VAR
      where kind = var_kind
      where type ty = BOMTy.t

    type t = var (* = (var_kind, BOMTy.t) VarRep.var_rep *)

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

    datatype kind = datatype BOMRep.var_kind

    fun kindToString VK_None = "None"
      | kindToString (VK_Let _) = "Let"
      | kindToString (VK_RHS _) = "RHS"
      | kindToString VK_Param = "Param"
      | kindToString (VK_Fun _) = "Fun"
      | kindToString (VK_Cont _) = "Cont"
      | kindToString (VK_CFun _) = "CFun"

    structure V = VarFn (
      struct
	type kind = kind
	type ty = BOMTy.t
	val defaultKind = VK_None
	val kindToString = kindToString
	val tyToString = BOMTyUtil.toString
      end)

    open V

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

  (* mapping from functions to the HLOp that they define *)
    val {clrFn = clrHLOp, peekFn = hlop, setFn = setHLOp, ...} =
	  newProp (fn _ => ((raise Fail "no HLOp") : hlop))

    fun isHLOp f = Option.isSome(hlop f)

  end
