structure Main = struct

  structure U = Utils
  structure R = RunTestsFn(MC)

(* sys : string -> OS.Process.status *)
  val sys = OS.Process.system

(* main : string -> unit *)
  fun main (progname, [localFile]) = 
       (R.main (U.currentRevision (), 
		SOME (OS.FileSys.fullPath localFile));
	OS.Process.success)
    | main (progname, []) = 
       (R.main (U.currentRevision (),
		NONE);
        OS.Process.success)
    | main _ = 
       (print "usage: run-tests [LOCALFILE]\n";
	OS.Process.failure)

(* main : string * string list -> OS.Process.status *)
  fun main (progname, files) = let
    val rpt   = R.run ()
    val hrpt  = ReportHTML.mkReport rpt   
    val write = fn f => MiniHTML.toFile (hrpt, f)
    in
      ArchiveReport.report rpt;
      app write files;
      OS.Process.success 
    end

  fun foo () = let
    val rpt = R.run ()
    val htm = ReportHTML.mkReport rpt
    in
      MiniHTML.toFile (htm, "/home/adamshaw/MCResults/current/foo.html");
      ArchiveReport.report rpt
    end

end
