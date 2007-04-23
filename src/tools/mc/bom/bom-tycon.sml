(* bom-tycon.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utility code for datatypes in BOM.
 *)

structure BOMTyCon =
  struct

    datatype tyc = datatype BOMTy.tyc
    datatype data_con = datatype BOMTy.data_con
    datatype dcon_rep = datatype BOMTy.dcon_rep

    fun toString (DataTyc{name, ...}) = name

  (* convert datatypes to their representation types *)
(* FIXME: we need to recursively convert the argument type; but watch out for infinite
 * recursion!!!
 *)
    fun toRepTy (DataTyc{nNullary, cons, ...}) = (case (nNullary, !cons)
	   of (0, [DCon{rep=Transparent, argTy=[ty], ...}]) => ty
	    | (0, [DCon{rep=Tuple, argTy, ...}]) => BOMTy.T_Tuple argTy
	    | (0, [DCon{rep=TaggedTuple tag, argTy, ...}]) => BOMTy.T_Tuple(BOMTy.T_Enum tag :: argTy)
	    | (_, []) => BOMTy.T_Enum(Word.fromInt nNullary - 0w1)
(* FIXME: we need a union type in BOM for this situation *)
	    | _ => BOMTy.T_Any
	  (* end case *))

  (* create a new datatype tycon *)
    fun newDataTyc (name, nNullary) = DataTyc{
	    name = name,
	    stamp = Stamp.new(),
	    nNullary = nNullary,
	    cons = ref[]
	  }

  (* add a data constructor to a datatype tycon *)
    fun newDataCon (DataTyc{cons, ...}) (name, rep, argTy) = let
	  val dc = DCon{
		  name = name,
		  stamp = Stamp.new(),
		  rep = rep,
		  argTy = argTy
		}
	  in
	    cons := !cons @ [dc];
	    dc
	  end

  end
