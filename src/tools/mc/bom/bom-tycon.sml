(* bom-tycon.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMTyCon =
  struct

    datatype tyc		      (* high-level type constructor *)
      = DataTyc of {
	  name : string,
	  stamp : Stamp.stamp,		(* a unique stamp *)
	  nNullary : int,		(* the number of nullary constructors *)
	  cons : data_con list		(* list of non-nullary constructors *)
	}

    and data_con = DCon of {	      (* a data-constructor function *)
	  name : string,		(* the name of the constructor *)
	  stamp : Stamp.stamp,		(* a unique stamp *)
	  rep : dcon_rep		(* the representation of values constructed by this *)
					(* constructor *)
	}

    and dcon_rep		      (* representation of data-constructor functions; note: *)
				      (* this type does not include constants. *)
      = Transparent			(* data-constructor represented directly by its argument *)
      | Boxed				(* heap-allocated box containing value *)
      | TaggedBox of word		(* heap-allocated tag/value pair *)

    fun toString (DataTyc{name, ...}) = name

  end
