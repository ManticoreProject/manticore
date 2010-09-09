(* generator-sig.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

signature GENERATOR =
  sig

  (* name of template file *)
    val template : string

  (* destination path relative to root of Manticore source tree *)
    val path : string

    val hooks : TextIO.outstream * LoadFile.log_file_desc -> (string * (unit -> unit)) list

  end
