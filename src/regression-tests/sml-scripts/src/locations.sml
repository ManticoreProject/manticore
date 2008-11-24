(* locations.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Where to find files.
 *)

structure Locations = struct

  val mc = ref "mc"
  val defaultReport = OS.FileSys.fullPath "../../reports/current/results.html" handle SysErr => raise Fail "1"
  val archiveDir = OS.FileSys.fullPath "../../reports/archive" handle SysErr => raise Fail "2"

end
