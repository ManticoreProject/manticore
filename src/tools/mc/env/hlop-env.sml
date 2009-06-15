(* hlop-env.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure HLOpEnv : sig

  (* definition of a hlop *)
    type hlop_def = {
	name : BOM.hlop,		(* the HLOp's identifier *)
	path : string list,		(* full path to the HLOp *)
	inline : bool,			(* should the HLOp be inlined? *)
	def : BOM.lambda,		(* the HLOps definition *)
	externs : (BOM.var * int) list	(* list of external variables (i.e., C functions) *)
					(* that def references paired with a count of the *)
					(* number of references *)
      }

  (* add a group of HLOp defs to the HLop cache *)
    val addDefs : hlop_def list -> unit

  (* find an HLOp by the unique HLOp ID *)
    val findDef : HLOp.hlop -> hlop_def option

  (* get the BOM variables that are bound to the BOM functions, which
   * define the HLOps.
   *)
    val listHLOps : unit -> BOM.Var.Set.set

  end = struct

    structure Tbl = Stamp.Tbl

    type hlop_def = {
	name : BOM.hlop,			(* the HLOp's identifier *)
	path : string list,                     (* full path to the HLOp *)
	inline : bool,				(* should the HLOp be inlined? *)
	def : BOM.lambda,			(* the HLOps definition *)
	externs : (BOM.var * int) list		(* list of external variables (i.e., C functions) *)
						(* that def references paired with a count of the *)
						(* number of references *)
      }

  (* cache of defined HLOps *)
    local 
    val hlops : hlop_def Tbl.hash_table = Tbl.mkTable(128, Fail "HLOp table")
    fun addDef (d as {name as HLOp.HLOp{id, name=n, ...}, def=BOM.FB{f, ...}, ...}) = (
	(* annotate the function as an HLOp *)
	  BOM.Var.setHLOp (f, name);
	(* add mapping from the HLOp's unique ID to its definition *)
	  Tbl.insert hlops (id, d))
    in
  (* add a group of HLOp defs to the HLop cache *)
    val addDefs = List.app addDef
  (* find a HLOp by the unique HLOp ID *)
    fun findDef (HLOp.HLOp{id, ...}) = Tbl.find hlops id
  (* get all the bodies of all defined HLOps *)
    fun listHLOps () =
	  Tbl.fold (fn ({def=BOM.FB{f, ...}, ...}, s) => BOM.Var.Set.add(s, f))
	    BOM.Var.Set.empty hlops
    end

  end
