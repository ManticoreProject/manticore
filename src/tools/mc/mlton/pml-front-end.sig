(* front-end.sig
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

signature PML_FRONT_END =
  sig

    structure Sxml : SXML

    val compileMLB : {input : File.t} -> Sxml.prog
    val compilePML : {input : File.t list} -> Sxml.prog

  end
