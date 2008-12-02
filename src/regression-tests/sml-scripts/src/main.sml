(* main.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Driver for scripts.
 * The command line script "run-tests" has the following options:
 *   -m FILE the path to mc (default is "mc" and assumes it's in the path)
 *   -c FILE a local copy to write the HTML to in addition to ../reports/current/results.html 
 *)

structure Main = struct

  structure G = GetOpt
  structure U = Utils
  structure L = Locations
  structure R  = RunTestsFn(MC)

  fun println s = (print s; print "\n")

  val space = String.concatWith " "

(* sys : string -> OS.Process.status *)
  val sys = OS.Process.system

(* existingDir : string -> bool *)
(* This function returns true if the given pathname names *)
(*   an existing directory, false otherwise. *)
  fun existingDir f = (OS.FileSys.isDir f) handle SysErr => false

(* usage : string -> unit *)
  fun usage optMsg = 
   (case optMsg of SOME msg => println msg | NONE => ();
    println "usage: run-tests [options]";
    println "  options:";
    println "    -c <file> (specify local copy of html results)";
    println "    -m <path> (specify location of mc)"; 
    raise Fail (case optMsg of SOME msg => msg | NONE => ""))

(* main : string * string list -> OS.Process.status *)
(* Run the tests and generate HTML reports for all named files. *)
  fun main (progname, args) = let
    (* build infrastructure for command-line options, including local state *)
    val mcPath = ref "mc"
    val htdocs = ref [L.defaultRpt]
    fun noteMC path = (mcPath := path; path)
    fun noteLocal path = (htdocs := (!htdocs)@[path]; path)
    fun mkOpt(s,l,act,h) = {short=s, long=[l], desc=G.ReqArg(act,"FILE"), help=h}
    val opts = [
      mkOpt ("m", "mc", noteMC, "mc path"),
      mkOpt ("c", "local-copy", noteLocal, "local copy of html output") 
    ]
    fun failWith s = raise Fail s
    val (opts, nonOpts) = 
      G.getOpt {argOrder=G.RequireOrder, options=opts, errFn=failWith} args
    val _ = (case nonOpts 
	       of [] => () 
		| _ => let 
                    val msg = space ("run-tests: unexpected command-line argument(s) =>"::nonOpts)
                    in
		      usage (SOME msg)
                    end
	       (* end case *))
    (* run the report and generate HTML *)
    val _       = MC.setCompilerPath (!mcPath)
    val _       = println "running tests..."
    val rpt     = R.run ()
    val hrpt    = ReportHTML.mkReport rpt   
    fun write f = let
      val f' = if existingDir f 
	       then OS.Path.joinDirFile {dir=f, file="results.html"}
	       else f
      in
	println ("generating html report in " ^ f');
	MiniHTML.toFile (hrpt, f')
      end
    in
      ArchiveReport.report rpt;
      Report.mkReport rpt;
      app write (!htdocs);
      OS.Process.success 
    end

end
