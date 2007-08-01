(* error.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Common infrastructure for error reporting in the Manticore compiler.
 *)

structure Error : sig

  (* global flag to record the existance of errors *)
    val anyErrors : bool ref

  (* the current input file *)
    val sourceFile : string ref

  (* the current sourcemap *)
    val sourceMap : AntlrStreamPos.sourcemap ref

  (* print an error message and record the fact that there was an error *)
    val say : string list -> unit

  end = struct

  (* global flag to record the existance of errors *)
    val anyErrors = ref false

  (* the current input file *)
    val sourceFile = ref ""

  (* the current sourcemap *)
    val sourceMap = ref(AntlrStreamPos.mkSourcemap())

  (* print an error message and record the fact that there was an error *)
    fun say l = (
	  anyErrors := true;
	  TextIO.output(TextIO.stdErr, String.concat l);
	  TextIO.output1(TextIO.stdErr, #"\n"))

  end
