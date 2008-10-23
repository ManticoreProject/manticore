(* hlop-env.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure HLOpEnv : sig

  (* definition of a hlop *)
    type hlop_def = {
	name : BOM.hlop,			(* the HLOp's identifier *)
	path : string list,                     (* full path to the HLOp *)
	inline : bool,				(* should the HLOp be inlined? *)
	def : BOM.lambda,			(* the HLOps definition *)
	pmlImports : (BOM.var * BOM.var) list,  (* imports from PML *)
	externs : (BOM.var * int) list		(* list of external variables (i.e., C functions) *)
						(* that def references paired with a count of the *)
						(* number of references *)
      }

  (* add a group of HLOp defs to the HLop cache *)
    val addDefs : hlop_def list -> unit
  (* find a HLOp by the unique HLOp ID *)
    val findDef : HLOp.hlop -> hlop_def option
  (* locate a HLOp by path name, e.g., Future1.@touch *)
    val findDefByPath : string list -> hlop_def option
  (* get all the bodies of all defined HLOps *)
    val listHLOps : unit -> BOM.lambda list

  end = struct

    type hlop_def = {
	name : BOM.hlop,			(* the HLOp's identifier *)
	path : string list,                     (* full path to the HLOp *)
	inline : bool,				(* should the HLOp be inlined? *)
	def : BOM.lambda,			(* the HLOps definition *)
	pmlImports : (BOM.var * BOM.var) list,  (* imports from PML *)
	externs : (BOM.var * int) list		(* list of external variables (i.e., C functions) *)
						(* that def references paired with a count of the *)
						(* number of references *)
      }

  (* cache of defined HLOps *)
    local 
    (* path -> hlop_def *)
    val hlopByPath : hlop_def AtomTable.hash_table = AtomTable.mkTable (128, Fail "HLOp path table")
    (* hlop -> hlop_def *)
    val hlops : hlop_def Stamp.Tbl.hash_table = Stamp.Tbl.mkTable(128, Fail "HLOp table")
    val pathToAtom = Atom.atom o String.concatWith "."
    fun addDef (d as {name as HLOp.HLOp{id, name=n, ...}, path, inline, def, externs, pmlImports}) = (
        (* associate a HLOp with its unique path *)
	  AtomTable.insert hlopByPath (pathToAtom path, d);
	(* associate a HLOp with a unique ID *)
	  Stamp.Tbl.insert hlops (id, d))    
    in
  (* add a group of HLOp defs to the HLop cache *)
    val addDefs = List.app addDef
  (* find a HLOp by the unique HLOp ID *)
    fun findDef (HLOp.HLOp{id, ...}) = Stamp.Tbl.find hlops id
  (* locate a HLOp by path name, e.g., Future1.@touch *)
    fun findDefByPath path = AtomTable.find hlopByPath (pathToAtom path)
  (* get all the bodies of all defined HLOps *)
    fun listHLOps () = List.map #def (Stamp.Tbl.listItems hlops)
    end

    val {
           getFn=getHLOp : ProgramParseTree.Var.var -> hlop_def option, 
	   setFn=setHLOp : (ProgramParseTree.Var.var * hlop_def option) -> unit, ...
        } = 
	   ProgramParseTree.Var.newProp (fn _ => NONE)

  end
