(* var-rep.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generic representation of variables.
 *)

structure VarRep =
  struct

    datatype ('kind, 'ty) var_rep = V of {
	name : string,
	id : Stamp.stamp,
	kind : 'kind ref,
	useCnt : int ref,
	ty : 'ty ref,
	props : PropList.holder
      }

    fun compare (V {id, ...}, V {id = id', ...}) = Stamp.compare (id, id')

    fun toString (V {name, id, ...}) = name ^ Stamp.toString id

  end
