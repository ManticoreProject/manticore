(* archive-report.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Write tests (as defined in the Tests module) to the archive folder.
 *)

structure ArchiveReport = struct

  structure T = Tests
  structure L = Locations

  val pathcat = OS.Path.concat
  fun joinBE (b,e) = OS.Path.joinBaseExt {base=b, ext=SOME e}

(* datestamp : Date.date -> string *)
  val datestamp = Date.fmt "%Y-%m-%d.%H-%M-%S"

(* goal : T.goal -> unit *)
(* Assumes the goal is well-formed in that all goalNames and stamps match. *)
  fun goal (T.G (rdme, rs)) = let
    val T.README {timestamp=d, goalName=g, ...} = rdme					  
    val base = concat [datestamp d, ".", g]
    fun wtr (tr as T.TR {testName=t, ...}) = let
      val base' = concat [base, ".", t]
      val filename = joinBE (base', "result")
      val fullpath = pathcat (L.archiveDir, filename)	
      in
        T.writeTestResult (tr, fullpath)
      end
    in
      T.writeReadme (rdme, pathcat (L.archiveDir, joinBE (base, "readme")));
      app wtr rs
    end      
  
(* report : T.report -> unit *)
(* Write out a report to a batch of files. *)
  val report = app goal

end
