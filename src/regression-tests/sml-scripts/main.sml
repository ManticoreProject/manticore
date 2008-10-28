structure Main = struct

  structure U = Utils
  structure M = RunTestsFn(MC)

(* sys : string -> OS.Process.status *)
  val sys = OS.Process.system

(* currentRevision : unit -> string *)
  fun currentRevision () = let
    val tmpfile = U.freshTmp (OS.FileSys.fullPath ".", "revision")
    val cmd = "svn info > " ^ tmpfile
    val _ = sys cmd
    val revisions = List.filter (String.isPrefix "Revision") (U.textOf tmpfile)
    in 
     (case revisions
        of [] => "no revision number available"
	 | [r] => r
	 | rs => raise Fail (String.concatWith ";" ("too many revisions available\n" :: rs)))
     before U.rm tmpfile
    end    

(* FIXME: The local file name here should be read in at the command line. *)
  fun main () = M.main (currentRevision (), 
			SOME (OS.FileSys.fullPath "/home/adamshaw/MCResults/current/results.html"))

end
