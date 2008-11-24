(* locations.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Where to find files.
 *)

structure Locations = struct

  val mc = ref (OS.FileSys.fullPath "../../../../bin/mc")
  val defaultReport = OS.FileSys.fullPath "../../reports/current/results.html"
  val archiveDir = OS.FileSys.fullPath "../../reports/archive"

end
