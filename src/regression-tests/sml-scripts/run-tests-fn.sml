(* run-tests-fn.sml
 *
 * COPYRIGHT (c) 2008 Adam Shaw (http://people.cs.uchicago.edu/~adamshaw)
 * All rights reserved.
 *
 * A script to run the tests in the goals directory and generate reports.
 *)

(* FIXME: What I really want is some sort of test_result value,
 *        that I render in some other module. Here testing and
 *        rendering are tied together -- too tightly coupled.
 *)

(* FIXME: Create and delete all the exes, tmps, etc. somewhere SAFE. *)

functor RunTestsFn (L : COMPILER) = struct

  structure HTML = MiniHTML
  structure FS = OS.FileSys
  structure P = OS.Path
  structure U = Utils
  val sys = OS.Process.system

(* datestamp : Date.date -> string *)
  val datestamp = Date.fmt "%Y-%m-%d.%H-%M-%S"

(* codify : string list -> HTML.html *)
  fun codify loc = let
    fun loop ([], acc)   = rev acc
      | loop ([h], acc)  = rev (HTML.str h :: acc)
      | loop (h::t, acc) = loop (t, HTML.br :: HTML.str h :: acc)
    in
      HTML.codeH (HTML.sequence (loop (loc, [])))
    end

fun halt() = raise Fail "halt"

(* runTest : date * string -> HTML.html *)
(* Run a test with given filename. Produce a piece of HTML giving results. *)
  fun runTest (d, filename) = let
    val exeFile = L.mkExe filename
    val okFile  = concat [P.dir filename, "/", P.file (P.base filename) ^ ".ok"]
    val resultsFile = U.freshTmp (FS.fullPath ".", "results")
    val compileCmd = L.mkCmd filename
    val compileSucceeded = sys compileCmd
    val tmps = exeFile :: resultsFile :: L.detritus filename
    fun cleanup () = app U.rm tmps
    in
     (if compileSucceeded = 0 then let
	val diffFile = U.freshTmp (FS.fullPath ".", "diffs")
        val makeOutput = sys (concat ["./", exeFile, " > ", resultsFile])
        val diffCmd = concat ["diff ", resultsFile, " ", okFile, " > ", diffFile]
	val diffProc = sys diffCmd
        in
         (if OS.FileSys.fileSize diffFile = 0 then
	    HTML.pCS ("results", "Test succeeded.")
          else
            HTML.sequence ([HTML.pCS ("results", "Test failed."),
			    HTML.pCH ("results",
			      HTML.tableCH ("results",
			        HTML.sequence ([
				  HTML.trH (
				    HTML.sequence ([
				      HTML.th ("expected results"),
				      HTML.th ("actual results")])),
			          HTML.trH (
				    HTML.sequence ([
				      HTML.tdH (codify (U.textOf okFile)),
				      HTML.tdH (codify (U.textOf resultsFile))]))])))]))
	  before 
            U.rm diffFile
        end
      else
        HTML.pCS ("results", "Compilation failed."))
     before cleanup ()
    end

(* main : string -> unit *)
  fun main (currentRevision, localCopy) = let
    val testDateTime = Time.now ()
    val d = Date.fromTimeLocal testDateTime
    val dateTimeString = Date.toString d
    val title = "Manticore: Regression Test Results"
    val goalDirs = let
      fun ds d = U.dirsWithin (concat [U.dotdot ".", "/", d])
      in
        ds "par" @ ds "seq"
      end 
(*  val goalDirs = U.dirsWithin (U.dotdot "." ^ "/phony") *)
    val goalDirs' = List.filter (not o (String.isPrefix ".") o OS.Path.file) goalDirs
    fun goalHeader goalDir = HTML.h2CS ("goal", concat [P.file (P.dir goalDir), "/", P.file goalDir])
    fun fileHeader filename = HTML.h3CS ("testfile", P.file filename)
    fun processDir goalDir = let
      val h2 = goalHeader goalDir
      val fs = U.filesWithin (String.isSuffix ("." ^ L.ext)) goalDir
      in
        HTML.divCAH ("goal", [{key="id", value=P.file goalDir}],
		     HTML.sequence (h2 :: 
				    U.interleave (map fileHeader fs, 
						  map (fn f => runTest (d, f)) fs)))
      end
    val body = HTML.sequence (HTML.h1 title :: 
			      HTML.h2CS ("datetime", dateTimeString) ::
			      HTML.h3CS ("revision", currentRevision) ::
			      map processDir goalDirs')
    val htdoc = HTML.htdoc (title, ["../results.css"], body)
    in
      HTML.toFile (htdoc, 
		   concat ["../reports/archive/", datestamp d, ".results.html"]);
      HTML.toFile (htdoc, 
		   "../reports/current/results.html");
      case localCopy 
        of NONE => ()
	 | SOME file => HTML.toFile (htdoc, file)
    end

end
