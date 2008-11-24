(* compiler.sig
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The things the scripts need to know about a compiler.
 *)

signature COMPILER = sig

  val languageName : string

(* The suffix for programs in the source language in question. *)
  val ext          : string

(* A function to construct an executable name from the infile name. *)
(* e.g., for ocaml, a program called "foo.ml" will have an exec name "foo" *)
(* e.g., for manticore, all programs have the exec name "a.out" *)
  val mkExe        : string -> string

(* A function to make the compile command for given infile and outfile. *)
(* e.g., for ocaml, mkCmd "foo.ml" --> "ocamlc -o foo foo.ml" *)
  val mkCmd        : string -> string

(* A function naming the extra files needed to be cleaned up after test is done. *)
(* e.g., ocamlc leaves a .cmi and a .cmo in its wake *)
  val detritus     : string -> string list  

end
