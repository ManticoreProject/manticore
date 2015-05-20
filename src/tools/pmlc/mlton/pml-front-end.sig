(* front-end.sig
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

signature PML_FRONT_END =
  sig

    structure Sxml : SXML
    structure Tycon : TYCON

    val init : unit -> unit

    val compileMLB : {input : File.t} -> Sxml.Program.t
    val compilePML : {input : File.t list} -> Sxml.Program.t
    val conTycon : Sxml.Con.t -> Sxml.Tycon.t
    val tyconCons : Sxml.Tycon.t -> {con: Sxml.Con.t,
                                        hasArg: bool} vector

  end
