(* var-rep.sml
 *
 * COPYRIGHT (c) 2006 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure VarRep =
  struct

    datatype ('kind, 'ty) var_rep = V of {
	name : Atom.atom,
	id : Stamp.stamp,
	kind : 'kind ref,
	ty : 'ty,
	props : PropList.holder
      }

  end
