(* mc.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure MC : COMPILER = struct
  val languageName = "manticore"
  val ext = "pml"
  fun mkExe infile = "a.out"
  fun mkCmd infile = concat ["mc ", infile]
  fun detritus infile = [OS.Path.base infile ^ ".s"]
end

