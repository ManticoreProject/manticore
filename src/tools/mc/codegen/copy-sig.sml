(* copy-sig.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate MLRISC parallel copy instructions.
 *)

signature COPY =
  sig

    structure MTy : MLRISC_TYPES

  (* a utility routine for generating code to copy a bunch of MLRISC expressions
   * to a bunch of registers.  The generated code uses parallel copies, when
   * possible.
   *)
    val copy : {
	    src : MTy.mlrisc_tree list,
	    dst : MTy.mlrisc_reg list
	  } -> MTy.T.stm list

  (* given a list of registers, allocate fresh registers and copy the values from
   * the registers to the new registers.
   *)
    val fresh : MTy.mlrisc_reg list -> {
	    stms : MTy.T.stm list,
	    regs : MTy.mlrisc_reg list
	  }

  end
