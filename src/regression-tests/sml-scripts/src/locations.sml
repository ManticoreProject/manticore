(* locations.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Paths to certain files.
 *)

structure Locations = struct

  val datestamp = Date.fmt "%Y-%m-%d-%H-%M-%S"

  val mc = ref "mc"

  val reportsDir = OS.FileSys.fullPath "../../reports"
  val goalsDir   = OS.FileSys.fullPath "../../goals"
  val archiveDir = OS.Path.joinDirFile {dir=reportsDir, file="archive"} 
  val currentDir = OS.Path.joinDirFile {dir=reportsDir, file="current"}
  val defaultRpt = OS.Path.joinDirFile {dir=currentDir, file="results.html"}  

  fun simpleRpt d = let
    val rpt = "report-" ^ (datestamp d)
    in
      OS.Path.joinDirFile {dir=reportsDir, file=rpt}
    end

end

