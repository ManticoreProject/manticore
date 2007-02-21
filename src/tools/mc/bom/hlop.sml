(* hlop.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A generic representation of "high-level" operators in the BOM
 *)

structure HLOp =
  struct

    datatype hlop = HLOp of {
	name : Atom.atom,
	id : Stamp.stamp
      }

  end
