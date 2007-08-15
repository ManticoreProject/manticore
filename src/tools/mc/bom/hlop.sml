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

  end
