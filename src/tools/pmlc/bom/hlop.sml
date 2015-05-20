(* hlop.sml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A generic representation of "high-level" operators in the BOM
 *)

structure HLOp : sig

    type hlop_sig = {		    (* High-level operation signature *)
	params : BOMTy.t list,		(* parameter signature *)
	exh : BOMTy.t list,
	results : BOMTy.t list		(* list of results *)
      }

(* FIXME: this type should be abstract *)
    datatype t = HLOp of {
	name : string,
	stamp : Stamp.stamp,
	sign : hlop_sig,
	returns : bool,			(* true, if the operation returns *)
        pure : bool,                    (* is the HLOp guaranteed to be pure? *)
        constr : bool                   (* does the HLOp create an object worthy of tracking (CML channel) *)
      }

    val nameOf : t -> string

    val toString : t -> string

  end = struct

    type hlop_sig = BOMRep.hlop_sig
    datatype t = datatype BOMRep.hlop

    fun nameOf (HLOp{name, ...}) = name

    fun toString (HLOp{name, stamp, pure, constr, ...}) = concat[
	    "@", name, "#", Stamp.toString stamp,
	    if constr then "C" else "", if pure then "P" else ""
	  ]

    fun hash (HLOp{stamp, ...}) = Stamp.hash stamp
    fun same (HLOp{stamp=a, ...}, HLOp{stamp=b, ...}) = Stamp.same(a, b)
    fun compare (HLOp{stamp=a, ...}, HLOp{stamp=b, ...}) = Stamp.compare(a, b)

    fun isPure (HLOp{pure, ...}) = pure
    fun isConstr (HLOp{constr, ...}) = constr

    datatype attributes = NORETURN | PURE | CONSTR

  (* new : Atom.atom * hlop_sig * attributes list -> hlop *)
    fun new (name, sign, attrs) = let
	  val stamp = Stamp.new()
	  val returns = ref true
          val pure = ref false
          val constr = ref false
	  fun doAttr NORETURN = returns := false
            | doAttr PURE = pure := true
            | doAttr CONSTR = constr := true
	  in
	    List.app doAttr attrs;
	    HLOp{
		name = name, stamp = stamp, sign = sign,
		returns = !returns, pure = !pure, constr = !constr
	      }
	  end

  (* sigToString : hlop_sig -> string *)
    fun sigToString ({params, exh, results}) = let
	  val ps = map BOMTy.toString params
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

  (* toDebugString : hlop -> string *)
    fun toDebugString (HLOp{name, stamp, sign, returns, pure, constr}) = let
	  val i = Stamp.toString stamp
	  val s = sigToString sign
	  val r = if returns then "true" else "false"
          val p = if pure then "true" else "false"
          val c = if constr then "true" else "false"
	  in
	    String.concat[
		"(* begin HLOp ----\n", 
		name, "<", i, ">:\n",
		s,
		"returns: ", r, "\n",
		"pure: ", p, "\n",
		"constr: ", c, "\n",
		"end HLOp ----- *)\n"
	      ]
	  end

  end (* HLOp *)
