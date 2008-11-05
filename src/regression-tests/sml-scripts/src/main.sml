structure Main = struct

  structure U = Utils
  structure M = RunTestsFn(MC)

(* sys : string -> OS.Process.status *)
  val sys = OS.Process.system

(* currentRevision : unit -> string *)
(* Gets the current svn revision of the Manticore project. *)
(* Returns a string like "Revision: 2420\n". *)
(* This needs to be run from within the Manticore tree. *)
  fun currentRevision () = let
    val tmpfile = U.freshTmp (OS.FileSys.fullPath ".", "revision")
    val cmd = "svn info | grep Revision > " ^ tmpfile
    val _ = sys cmd
    val revisions = List.filter (String.isPrefix "Revision") (U.textOf tmpfile)
    in 
     (case revisions
        of [] => "no revision number available"
	 | [r] => r
	 | rs => raise Fail (String.concatWith ";" ("too many revisions available\n" :: rs))
        (* end case *))
     before U.rm tmpfile
    end    

(* main : string -> unit *)
  fun main (progname, [localFile]) = 
       (M.main (currentRevision (), 
		SOME (OS.FileSys.fullPath localFile));
	OS.Process.success)
    | main (progname, []) = 
       (M.main (currentRevision (),
		NONE);
        OS.Process.success)
    | main _ = 
       (print "usage: run-tests [LOCALFILE]\n";
	OS.Process.failure)

end
