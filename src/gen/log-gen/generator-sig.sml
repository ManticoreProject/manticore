(* generator-sig.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

signature GENERATOR =
  sig

  (* destination path relative to root of Manticore source tree *)
    val path : string

    val gen : TextIO.outstream * LoadFile.log_file_desc -> unit

  end
