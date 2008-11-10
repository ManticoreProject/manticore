structure Main = struct

  structure U = Utils
  structure R = RunTestsFn(MC)

(* sys : string -> OS.Process.status *)
  val sys = OS.Process.system

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

end
