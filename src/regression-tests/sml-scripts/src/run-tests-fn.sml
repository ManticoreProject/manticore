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

functor RunTestsFn (L : COMPILER) = struct

  structure H  = MiniHTML
  structure FS = OS.FileSys
  structure P  = OS.Path
  structure T  = Tests
  structure U  = Utils

  val sys = OS.Process.system

(* datestamp : Date.date -> string *)
  val datestamp = Date.fmt "%Y-%m-%d.%H-%M-%S"

(* runTest' : date * string -> {outcome:T.outcome, expected:string, actual:string} *)
  fun runTest' (d, filename) = let
    val cwd = FS.getDir ()
    val exeFile = L.mkExe filename
    val okFile  = concat [P.dir filename, "/", P.file (P.base filename), ".ok"]
    val resFile = U.freshTmp (cwd, "results")
    val compileCmd = L.mkCmd filename
    val compileSucceeded = sys compileCmd
    val tmps = exeFile :: resFile :: L.detritus filename
    fun cleanup () = app U.rm tmps
    in
     (if compileSucceeded = 0 then let
        val diffFile = U.freshTmp (cwd, "diffs")
        val makeOutput = sys (concat ["./", exeFile, " > ", resFile])
	val diffCmd = concat ["diff ", resFile, " ", okFile, " > ", diffFile]
	val diffProc = sys diffCmd
	val expected = String.concatWith "\n" (U.textOf okFile)
	val actual   = String.concatWith "\n" (U.textOf resFile)
        in
	 {outcome = if FS.fileSize diffFile = 0 then T.TestSucceeded else T.TestFailed,
	  expected = expected,
	  actual = actual}
	 before U.rm diffFile
	end
      else
        {outcome = T.DidNotCompile,
	 expected = "",
	 actual = ""})
     before cleanup ()
    end

(* runTest : date * string -> H.html *)
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
	    H.pCS ("results", "Test succeeded.")
          else
            H.seq ([H.pCS ("results", "Test failed."),
			    H.pCH ("results",
			      H.tableCH ("results",
					 [H.trH [H.th "expected results",
						 H.th "actual results"],
					  H.trH [H.tdH (ReportHTML.codify (String.concatWith "\n" (U.textOf okFile))),
						 H.tdH (ReportHTML.codify (String.concatWith "\n" (U.textOf resultsFile)))]]))]))
	  before 
            U.rm diffFile
        end
      else
        H.pCS ("results", "Compilation failed."))
     before cleanup ()
    end

(* runGoal : date * string -> string -> T.goal *)
  fun runGoal (d, ver) goalDir = let
    val ts = U.filesWithin (String.isSuffix ("." ^ L.ext)) goalDir
    val readme = T.readRawReadme d (goalDir ^ "/README")
    val g = OS.Path.file goalDir
    fun mk t = let
      val {outcome, expected, actual} = runTest' (d, t)
      in
        T.TR {stamp = T.STAMP {timestamp=d, version=ver},
	      goalName = g,
	      testName = OS.Path.file t,
	      outcome = outcome,
	      expected = expected,
	      actual = actual}
      end
    in 
      T.G (readme, map mk ts)
    end

(* run : unit -> T.report *)
  fun run () = let
    val now      = Date.fromTimeLocal (Time.now ())
    val ver      = U.currentRevision ()
    val goals    = U.dirsWithin (concat [U.dotdotdot ".", "/goals"])
    val noHidden = List.filter (not o (String.isPrefix ".") o OS.Path.file)
    val pvalOnly = List.filter ((fn s => s = "par-pval") o OS.Path.file)
    in
      map (runGoal (now, ver)) (noHidden goals) 
(*     map (runGoal (now, ver)) (List.take (noHidden goals, 1))  *)
(*     map (runGoal (now, ver)) (pvalOnly goals)  *)
    end 

(* main : string * string  -> unit *)
  fun main (currentRevision, localCopy) = let
    val testDateTime = Time.now ()
    val d = Date.fromTimeLocal testDateTime
    val dateTimeString = Date.toString d
    val title = "Manticore: Regression Test Results"
    val goalDirs = let
      fun ds d = U.dirsWithin (concat [U.dotdot (U.dotdot "."), "/", d])
      in
        ds "goals"
      end 
(*  val goalDirs = U.dirsWithin (U.dotdot "." ^ "/phony") *)
    val goalDirs' = List.filter (not o (String.isPrefix ".") o OS.Path.file) goalDirs
    fun goalHeader goalDir = let
      val readmeTxt = String.concatWith "\n" (U.textOf (goalDir ^ "/README"))
      in
        H.seq [H.h2CS ("goal", P.file goalDir),
		       H.pCS  ("README", readmeTxt)]
      end
    fun fileHeader filename = H.h3CS ("testfile", P.file filename)
    fun processDir goalDir = let
      val h2 = goalHeader goalDir
      val fs = U.filesWithin (String.isSuffix ("." ^ L.ext)) goalDir
      in
        H.divCAH ("goal", [{key="id", value=P.file goalDir}],
		     H.seq (h2 :: 
			    U.interleave (map fileHeader fs, 
					  map (fn f => runTest (d, f)) fs)))
      end
    val body = H.seq (H.h1 title :: 
		      H.h2CS ("datetime", dateTimeString) ::
		      H.h3CS ("revision", currentRevision) ::
		      map processDir goalDirs')
    val htdoc = H.htdoc (title, ["../results.css"], body)
    in
      H.toFile (htdoc, 
		   concat ["../../reports/archive/", datestamp d, ".results.html"]);
      H.toFile (htdoc, 
		   "../../reports/current/results.html");
      case localCopy 
        of NONE => ()
	 | SOME file => H.toFile (htdoc, file)
    end

end
