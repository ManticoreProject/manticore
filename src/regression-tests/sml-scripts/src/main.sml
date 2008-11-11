structure Main = struct

  structure U = Utils
  structure R = RunTestsFn(MC)

(* sys : string -> OS.Process.status *)
  val sys = OS.Process.system

(* existingDir : string -> bool *)
  fun existingDir f = (OS.FileSys.isDir f) handle SysErr => false

(* main : string * string list -> OS.Process.status *)
  fun main (progname, files) = let
    val rpt     = R.run ()
    val hrpt    = ReportHTML.mkReport rpt   
    fun write f = let
      val f' = if existingDir f 
	       then OS.Path.joinDirFile {dir=f, file="results.html"}
	       else f
      in
	print ("Generating report in " ^ f' ^ ".\n");
	MiniHTML.toFile (hrpt, f')
      end
    in
      ArchiveReport.report rpt;
      app write files;
      OS.Process.success 
    end

end
