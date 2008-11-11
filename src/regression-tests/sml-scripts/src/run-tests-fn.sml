(* run-tests-fn.sml
 *
 * COPYRIGHT (c) 2008 Adam Shaw (http://people.cs.uchicago.edu/~adamshaw)
 * All rights reserved.
 *
 * A script to run the tests in the goals directory and generate reports.
 *)

functor RunTestsFn (L : COMPILER) = struct

  structure H = MiniHTML
  structure F = OS.FileSys
  structure P = OS.Path
  structure T = Tests
  structure U = Utils

  val sys = OS.Process.system

  fun joinDF (d, f) = P.joinDirFile {dir=d, file=f}
  fun joinBE (b, e) = P.joinBaseExt {base=b, ext=SOME(e)}

(* datestamp : Date.date -> string *)
  val datestamp = Date.fmt "%Y-%m-%d.%H-%M-%S"

(* runTest : date * string -> {outcome:T.outcome, expected:string, actual:string} *)
  fun runTest (d, filename) = let
    val _ = (print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n";
	     print ("Testing " ^ filename ^ ".\n"))
    val cwd = F.getDir ()
    val exeFile = L.mkExe filename
    val okFile  = joinDF (P.dir filename,
			  joinBE (P.file (P.base filename), "ok"))
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
	val diffSucceeded = sys diffCmd 
        val actual = String.concatWith "\n" (U.textOf resFile)
	in
          if diffSucceeded = 0 then let	
	    val expected = String.concatWith "\n" (U.textOf okFile)
            in
	      {outcome = if F.fileSize diffFile = 0 then T.TestSucceeded else T.TestFailed,
	       expected = expected,
	       actual = actual}
	      before U.rm diffFile
	    end
	  else
	    (print (concat ["**********\n",
			    "WARNING: no .ok file for ", P.file filename, "\n",
			    "Please add an .ok file for ", filename, ".\n",
			    "**********\n"]);
	     U.rm diffFile;
	     {outcome = T.TestFailed, expected = "*** No .ok file! ***", actual = actual})
        end
      else
        {outcome = T.DidNotCompile,
	 expected = "",
	 actual = ""})
     before (print ("Done testing " ^ filename ^ ".\n");
	     print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n";
	     cleanup ())
	     
    end

(* runGoal : date * string -> string -> T.goal *)
  fun runGoal (d, ver) goalDir = let
    val ts = U.filesWithin (String.isSuffix ("." ^ L.ext)) goalDir
    val readme = T.readRawReadme d (goalDir ^ "/README")
    val g = OS.Path.file goalDir
    fun mk t = let
      val {outcome, expected, actual} = runTest (d, t)
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
(*     map (runGoal (now, ver)) (pvalOnly goals)                 *)
    end 

end
