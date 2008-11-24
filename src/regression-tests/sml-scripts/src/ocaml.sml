(* ocaml.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure OCaml : COMPILER = struct
  val languageName = "ocaml"
  fun getCompilerPath () = "ocamlc"
  fun setCompilerPath _ = raise Fail "not supported"
  val ext = "ml"
  val base = OS.Path.base
  fun mkExe infile = base infile
  fun mkCmd infile = concat ["ocamlc -o ", base infile, " ", infile]
  fun detritus infile = map (fn s => concat [base infile, ".", s]) ["cmi", "cmo"]
end

