structure Main = struct

  structure U = Utils
  structure M = RunTestsFn(MC)

(* sys : string -> OS.Process.status *)
  val sys = OS.Process.system

(* main : string -> unit *)
  fun main (progname, [localFile]) = 
       (M.main (U.currentRevision (), 
		SOME (OS.FileSys.fullPath localFile));
	OS.Process.success)
    | main (progname, []) = 
       (M.main (U.currentRevision (),
		NONE);
        OS.Process.success)
    | main _ = 
       (print "usage: run-tests [LOCALFILE]\n";
	OS.Process.failure)

  fun foo () = let
    val rpt = M.run ()
    val htm = ReportHTML.mkReport rpt
    in
      MiniHTML.toFile (htm, "/home/adamshaw/MCResults/current/foo.html");
      ArchiveReport.report rpt
    end

end
