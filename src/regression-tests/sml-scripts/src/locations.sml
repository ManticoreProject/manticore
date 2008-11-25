(* locations.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Relative paths to certain files.
 *)

structure Locations = struct

  val mc = ref "mc"
  val defaultReport = OS.Path.joinDirFile {dir  = OS.FileSys.fullPath "../../reports/current",
					   file = "results.html"}
  val archiveDir = OS.FileSys.fullPath "../../reports/archive"

end

