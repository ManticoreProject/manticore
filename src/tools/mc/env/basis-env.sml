(* basis-env.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module provides the AST transfomations access to the environment produced
 * by compiling the basis code.  
 *)

structure BasisEnv : sig

    val saveBasisEnv : BindingEnv.env -> unit

    val getValFromBasis   : string list -> ModuleEnv.val_bind
    val getTyFromBasis    : string list -> ModuleEnv.ty_def
    val getCFunFromBasis  : string list -> ProgramParseTree.Var.var
    val getBOMTyFromBasis : string list -> ProgramParseTree.Var.var
    val getHLOpFromBasis  : string list -> ProgramParseTree.Var.var

  (* a few convenience methods *)
    val getVarFromBasis   : string list -> AST.var
    val getDConFromBasis  : string list -> AST.dcon
    val getTyConFromBasis : string list -> Types.tycon

  end = struct

    structure PPT = ProgramParseTree
    structure BEnv = BindingEnv
    structure MEnv = ModuleEnv

    val basis : BEnv.env option ref = ref NONE

    fun saveBasisEnv bEnv = (basis := SOME bEnv)

    val pathToString = String.concatWith "."

    fun getModule path = let
	  fun get (bEnv, [x]) = SOME(bEnv, Atom.atom x)
	    | get (bEnv, x::r) = (case BEnv.findMod(bEnv, Atom.atom x)
		 of SOME(_, bEnv) => get (bEnv, r)
		  | NONE => NONE
		(* end case *))
	    | get _ = NONE
	  in
	    case !basis
	      of NONE => raise Fail (String.concat [
		    "getModule(", pathToString path, "): basis not initialized"
		  ])
	       | SOME bEnv => get (bEnv, path)
	    (* end case *)
	  end

    fun notFound path = raise Fail ("Unable to locate " ^ pathToString path)

    fun wrongThing path expected got = let
	  val msg = String.concat(
		"looking up " :: pathToString path :: ": expected "
		  :: expected :: " but got " :: got)
	  in
	    raise Fail msg
	  end

  (* getValFromBasis : string list -> ModuleEnv.val_bind
   * use a path (encoded as a string list) to look up a variable 
   * ex: getValFromBasis ["Future1", "future"]
   *     (looks up Future1.future) 
   *)
    fun getValFromBasis path = (case getModule path
	   of SOME(bEnv, x) => (case BEnv.findVal(bEnv, x)
		 of SOME(BEnv.Var v | BEnv.Con v) => (case ModuleEnv.getValBind v
		       of SOME vb => vb
			| NONE => notFound path
		      (* end case *))
		  | NONE => notFound path
		(* end case *))
	    | _ => notFound path
	  (* end case *))

  (* use a path (or qualified name) to look up a type *)
    fun getTyFromBasis path = (case getModule path
	   of SOME(bEnv, x) => (case BEnv.findTy(bEnv, x)
		 of SOME ty =>  (case ModuleEnv.getTyDef(BEnv.tyId ty)
		       of SOME tyd => tyd
			| NONE => notFound path
		      (* end case *))
		  | NONE => notFound path
		(* end case *))
	    | _ => notFound path
	  (* end case *))

  (* use a path (or qualified name) to look up a C function *)
    fun getCFunFromBasis path = (case getModule path
	   of SOME(_, x) => (case BEnv.findCFun x
		 of SOME v => v
		  | NONE => notFound path
		(* end case *))
	    | _ => notFound path
	  (* end case *))

  (* use a path (or qualified name) to look up a BOM type *)
    fun getBOMTyFromBasis path = (case getModule path
	   of SOME(bEnv, x) => (case BEnv.findBOMTy(bEnv, x)
		 of SOME v => v
		  | NONE => notFound path
		(* end case *))
	    | _ => notFound path
	  (* end case *))

  (* use a path (or qualified name) to look up a HLOp *)
    fun getHLOpFromBasis path = (case getModule path
           of SOME(bEnv, x) => (case BEnv.findBOMHLOp(bEnv, x)
                 of SOME v => v
		  | NONE => notFound path
                 (* end case *))
	    | _ => notFound path
          (* end case *))

  (* look up a variable *)
    fun getVarFromBasis path = (case getValFromBasis path
	   of MEnv.Var x => x
	    | other => let
		val dummyID = ProgramParseTree.Var.new ("dummyID", ())
		val tos = (fn x => MEnv.valBindToString (dummyID, x))
		fun w g = wrongThing path "Var" [g, " ", tos other]
		in
		  case other
		   of MEnv.Con _ => w "Con"
		    | MEnv.Overload _ => w "Overload"
		    | MEnv.EqOp _ => w "EqOp"
		    | _ => raise Fail "impossible"
		  (* end case *)
		end
	  (* end case *))

  (* look up a data constructor *)
    fun getDConFromBasis path = (case getValFromBasis path
	   of MEnv.Con c => c
	    | other => let
		val dummyID = ProgramParseTree.Var.new ("dummyID", ())
		val tos = (fn x => MEnv.valBindToString (dummyID, x))
		fun w g = wrongThing path "Con" [g, " ", tos other]
		in
		  case other
		   of MEnv.Var _ => w "Var"
		    | MEnv.Overload _ => w "Overload"
		    | MEnv.EqOp _ => w "EqOp"
		    | _ => raise Fail "impossible"
		  (* end case *)
		end
	  (* end case *))

  (* look up a type constructor *)
    fun getTyConFromBasis path =(case getTyFromBasis path
	   of MEnv.TyCon c => c
	    | MEnv.TyDef(Types.TyScheme(_, Types.ConTy(_, c))) => c
	    | other => let
		val dummyID = ProgramParseTree.Var.new ("dummyID", ())
		val tos = (fn x => MEnv.tyDefToString (dummyID, x))
		fun w g = wrongThing path "TyCon" [g, " ", tos other]
		in
		  case other
		   of MEnv.TyDef _ => w "TyDef"
		    | MEnv.BOMTyDef _ => w "BOMTyDef"
		    | _ => raise Fail "impossible"
		  (* end case *)
		end
	  (* end case *))

  end
