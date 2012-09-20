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
	returns : bool,			(* true, if the operation returns *)
        pure : bool,                    (* is the HLOp guaranteed to be pure? *)
        constr : bool                   (* does the HLOp create an object worthy of tracking (CML channel) *)
      }

    fun toString (HLOp{name, id, pure, constr, ...}) = concat["@", Atom.toString name, "#", Stamp.toString id, if constr then "C" else "", if pure then "P" else ""]
    fun name (HLOp{name, ...}) = name
    fun hash (HLOp{id, ...}) = Stamp.hash id
    fun same (HLOp{id=a, ...}, HLOp{id=b, ...}) = Stamp.same(a, b)
    fun compare (HLOp{id=a, ...}, HLOp{id=b, ...}) = Stamp.compare(a, b)

    fun isPure (HLOp{pure, ...}) = pure
    fun isConstr (HLOp{constr, ...}) = constr

    datatype attributes = NORETURN
                        | PURE
                        | CONSTR

  (* new : Atom.atom * hlop_sig * attributes list -> hlop *)
    fun new (name, sign, attrs) = let
	  val id = Stamp.new()
	  val returns = ref true
          val pure = ref false
          val constr = ref false
	  fun doAttr NORETURN = returns := false
            | doAttr PURE = pure := true
            | doAttr CONSTR = constr := true
	  in
	    List.app doAttr attrs;
	    HLOp{name = name, id = id, sign = sign, returns = !returns, pure = !pure, constr = !constr}
	  end

    local
    (* param_tyToString : param_ty -> string *)
     fun param_tyToString (PARAM t) = "PARAM " ^ (BOMTyUtil.toString t)
	| param_tyToString (OPT t) = "OPT " ^ (BOMTyUtil.toString t)
	| param_tyToString (VEC t) = "VEC " ^ (BOMTyUtil.toString t)
    in

  (* sigToString : hlop_sig -> string *)
    fun sigToString ({params, exh, results}) = let
	  val ps = map param_tyToString params
	  val es = map BOMTyUtil.toString exh
	  val rs = map BOMTyUtil.toString results
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

  (* toDebugString : hlop -> string *)
    fun toDebugString (HLOp{name, id, sign, returns, pure, constr}) = let
	  val n = Atom.toString name
	  val i = Stamp.toString id
	  val s = sigToString sign
	  val r = if returns then "true" else "false"
          val p = if pure then "true" else "false"
          val c = if constr then "true" else "false"
	  in
	    String.concat[
		"(* begin HLOp ----\n", 
		n, "<", i, ">:\n",
		s,
		"returns: ", r, "\n",
		"pure: ", p, "\n",
		"constr: ", c, "\n",
		"end HLOp ----- *)\n"
	      ]
	  end

    end (* local *)

  end (* HLOp *)
