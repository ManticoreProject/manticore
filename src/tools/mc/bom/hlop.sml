(* hlop.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A generic representation of "high-level" operators in the BOM
 *)

structure HLOp =
  struct

    datatype param_ty
      = PARAM of BOMTy.ty		(* required parameter *)
      | OPT of BOMTy.ty			(* optional parameter *)
      | VEC of BOMTy.ty			(* zero or more instances *)

    type hlop_sig = {		(* High-level operation signature *)
	params : param_ty list,		(* parameter signature *)
	exh : BOMTy.ty list,
	results : BOMTy.ty list		(* list of results *)
      }

    datatype hlop = HLOp of {
	name : Atom.atom,
	id : Stamp.stamp,
	sign : hlop_sig,
	returns : bool			(* true, if the operation returns *)
(* FIXME: need effects *)
      }

    fun toString (HLOp{name, ...}) = "@" ^ Atom.toString name
    fun name (HLOp{name, ...}) = name
    fun hash (HLOp{id, ...}) = Stamp.hash id
    fun same (HLOp{id=a, ...}, HLOp{id=b, ...}) = Stamp.same(a, b)

    fun isPure _ = false (* FIXME *)

    datatype attributes = NORETURN

  (* new : Atom.atom * hlop_sig * attributes list -> hlop *)
    fun new (name, sign, attrs) = let
	  val id = Stamp.new()
	  val returns = ref true
	  fun doAttr NORETURN = returns := false
	  in
	    List.app doAttr attrs;
	    HLOp{name = name, id = id, sign = sign, returns = !returns}
	  end

    local
    (* param_tyToString : param_ty -> string *)
      fun param_tyToString (PARAM t) = "PARAM " ^ (BOMTy.toString t)
	| param_tyToString (OPT t) = "OPT " ^ (BOMTy.toString t)
	| param_tyToString (VEC t) = "VEC " ^ (BOMTy.toString t)
    (* sigToString : hlop_sig -> string *)
      fun sigToString ({params, exh, results}) = let
	    val ps = map param_tyToString params
	    val es = map BOMTy.toString exh
	    val rs = map BOMTy.toString results
	    val commas = String.concatWith ", "
	    in
	      String.concat[
		  "sig\n",
		  "  params: ", (commas ps), "\n",
		  "  exh: ", (commas es), "\n",
		  "  results: ", (commas rs), "\n",
		  "end\n"
		]
	    end    
    in

  (* toDebugString : hlop -> string *)
    fun toDebugString (HLOp {name, id, sign, returns}) = let
	  val n = Atom.toString name
	  val i = Stamp.toString id
	  val s = sigToString sign
	  val r = if returns then "true" else "false"
	  in
	    String.concat[
		"begin HLOp ----\n", 
		n, "<", i, ">:\n",
		s,
		"returns: ", r, "\n",
		"end HLOp -----\n"
	      ]
	  end

    end (* local *)

  end (* HLOp *)
