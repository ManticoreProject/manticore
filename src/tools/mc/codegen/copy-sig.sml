(* copy-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Functions for copying pseudo registers.
 *)

signature COPY = sig

    structure MTy : MLRISC_TYPES

    (* This function copies a list of expressions into registers.  It uses
     * the parallel copy form when possible. *)
    val copy : {src : MTy.mlrisc_tree list, dst : MTy.mlrisc_reg list}
	       -> MTy.T.stm list

    (* Given a list of registers, allocate fresh registers and copy the 
     * values into fresh registers. *)
    val fresh : MTy.mlrisc_reg list -> {
		stms : MTy.T.stm list,
		regs : MTy.mlrisc_reg list
		}

end (* COPY *)
