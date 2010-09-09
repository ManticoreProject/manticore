(* rope.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Rope : sig

    val maxLeafSize  : unit -> int
    val ropeTyc      : unit -> Types.tycon
    val ropeTy       : Types.ty -> Types.ty
    val ropeLeaf     : unit -> AST.dcon
    val ropeCat      : unit -> AST.dcon

  end = struct

    structure A = AST
    structure B = Basis
    structure T = Types

    exception VariableArityType

    fun maxLeafSize () = Controls.get BasicControl.maxLeafSize

    fun ropeTyc () = (
	  case BasisEnv.getTyFromBasis ["RopeOps", "rope"]
	   of ModuleEnv.TyCon tyc => tyc
	    | _ => raise Fail "wrong kind for rope tyc"
  	   (* end case *))

    (* ropeTy : Types.ty -> Types.ty *)
    fun ropeTy ty = AST.ConTy ([ty], ropeTyc())

    fun ropeLeaf () = (
	  case BasisEnv.getValFromBasis ["RopeOps", "LEAF"]
	   of ModuleEnv.Con dcon => dcon
	    | _ => raise Fail "wrong kind for LEAF"
          (* end case *))

    fun ropeCat () = (
	  case BasisEnv.getValFromBasis ["RopeOps", "CAT"]
	   of ModuleEnv.Con dcon => dcon
	    | _ => raise Fail "wrong kind for CAT"
          (* end case *))

  end (* structure Rope *)
