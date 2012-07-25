(* report.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Write tests (as defined in the Tests module) to simple text files in the reports directory.
 *)

structure Report = struct

  structure U = Utils
  structure L = Locations
  structure T = Tests

(* println : string -> unit *)
  fun println s = (print s; print "\n")

(* writeln : TextIO.outstream -> string -> unit *)
  fun writeln outstream s = let
    fun out s = TextIO.output (outstream, s)
    in
      List.app out [s, "\n"]
    end

(* test_result : T.test_result -> string *)
  fun test_result (tr as T.TR info) = let
    val {testName, goalName, outcome, expected, actual, ...} = info
    fun foo whatHapd = concat ["***** ", whatHapd, " for goal ", goalName, " in file ", testName, "."] 
    in
      case outcome
        of T.DidNotCompile reason => foo ("Compile failed: " ^ reason)
	 | T.TestSucceeded => foo "Check succeeded"
	 | T.TestFailed => concat [foo "Check failed", "\n",
				   "expected {\n", expected, "\n} ",
				   "actual {\n", actual, "\n}"]
    end

(* goal : T.goal -> string list *)
  fun goal (T.G (me, results)) = List.map test_result results

(* report : T.report -> unit *)
  fun mkReport r = let
    val (d, ver) = 
     (case r
        of (T.G (_, T.TR {stamp, ...}::_))::_ => let
             val T.STAMP {timestamp, version} = stamp
             in
               (timestamp, version)
             end
         | _ => raise Fail "empty report"
        (* end case *))
    val outfile = L.simpleRpt d
    val outstream = TextIO.openOut outfile
    val outln = writeln outstream
    in
      println ("generating text report in " ^ outfile);
      outln "Manticore: Regression Test Results";
      outln outfile;
      outln ("revision " ^ ver);
      app (app outln o goal) r;
      TextIO.closeOut outstream
    end
    
end
