(* utils.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Various utilities for SML scripting.
 *)

(* FIXME : There are existing library routines to do lots of things done below.
 *         Refactor accordingly.
 *)

structure Utils = struct

  structure FS = OS.FileSys
  val sys = OS.Process.system

(* rm : string -> unit *)
  fun rm f = (FS.remove f) handle SysErr => ()

(* exists : string -> bool *)
  fun exists fullpath = 
   (case FS.fileId fullpath
      of nonfailure => true)
      handle SysErr => false

(* freshTmp : string * string -> string *)
(* e.g. freshTmp ("/home/adamshaw", "sml") --> "/home/adamshaw/tmpfile0.sml" *)
  fun freshTmp (fullpath, suffix) = let
    val upperBound = 30 (* if it fails this many times, something's probably wrong *)
    val msg = concat ["Tried to make a fresh tmp file name; giving up after ", 
		      Int.toString upperBound, " tries."]
    fun loop n = let
      val f = concat [fullpath, "/tmpfile", Int.toString n, ".", suffix]
      in 
        if not (exists f)
	then f
	else (if (n+1 >= upperBound) then raise Fail msg else ();
	      loop (n+1))
      end
    in
      loop 0
    end

(* last : 'a list -> 'a *)
  fun last [] = raise Fail "empty"
    | last [x] = x
    | last (_::xs) = last xs

(* butlast : 'a list -> 'a list *)
  fun butlast [] = raise Fail "empty"
    | butlast [x] = []
    | butlast (x::xs) = x :: butlast xs

(* dotdot : string -> string *)
  fun dotdot d = OS.Path.dir (FS.fullPath d)

(* dotdotdot : string -> string *)
  fun dotdotdot d = OS.Path.dir (OS.Path.dir (FS.fullPath d))

(* filesWithin : (string -> bool) -> string -> string list *)
  fun filesWithin pred fullname = let
    val tmp = FS.fullPath "."
    val _ = FS.chDir fullname
    val d = FS.openDir fullname
    fun loop acc =
     (case FS.readDir d
        of NONE => rev acc
         | SOME s => if pred s
		     then loop (FS.fullPath s :: acc)
		     else loop acc)
    val result = loop []
    in
      FS.closeDir d;
      FS.chDir tmp;
      result
    end

(* dirsWithin : string -> string list *)
  val dirsWithin = filesWithin FS.isDir

(* println : string -> unit *)
  fun println s = (print s; print "\n")

(* between : 'a -> 'a list -> 'a list *)
(* Wedge a y in between all the xs. *)
  fun between y xs = let
    fun loop ([], acc)  = rev acc
      | loop ([x], acc) = rev (x::acc)
      | loop (x::xs, acc) = loop (xs, y::x::acc)
    in
      loop (xs, [])
    end
 
(* interleave : 'a list * 'a list -> 'a list *)
  fun interleave ([], []) = []
    | interleave (x::xs, y::ys) = x :: y :: interleave (xs, ys)
    | interleave (_, _) = raise Fail "unequal lengths"

(* lastOccurrenceOf : char -> string -> int option *)
  fun lastOccurrenceOf c s = let
    fun find ([], _) = NONE
      | find (h::t, n) = if h=c then SOME n else find (t, n-1)
    in
      find (rev (explode s), String.size s - 1)
    end

(* stripSuffix : string -> string *)
  fun stripSuffix filename = let
    val opt = lastOccurrenceOf #"." filename
    in
      case opt 
        of NONE => filename
	 | SOME n => String.substring (filename, 0, n)
    end

(* textOf : string -> string list *)
  fun textOf filename = let
    val instream = TextIO.openIn filename
    fun read acc =
     (case TextIO.inputLine instream
        of NONE => rev acc
	 | SOME s => read (s :: acc)
        (* end case *))
    in
      read [] before TextIO.closeIn instream
    end

(* stripTrailingNewline : string -> string *)
(* Strips the trailing newline character from a string, *)
(* iff that's what the last character is. *)
  fun stripTrailingNewline s = let
    val cs = explode s
    val sc = rev cs
    in
      case sc
        of #"\n"::t => implode (rev t)
         | _ => s
    end

(* currentRevision : unit -> string *)
(* Gets the current svn revision of the Manticore project. *)
(* Returns a string like "Revision: 2420". *)
(* This should be run from within the Manticore tree. *)
(* FIXME: This should really be fetched from the *version* of mc. *)
  fun currentRevision () = let
    val tmpfile = freshTmp (OS.FileSys.fullPath ".", "revision")
    val cmd = "svn info | grep Revision > " ^ tmpfile
    val _ = sys cmd
    val revisions = List.filter (String.isPrefix "Revision") (textOf tmpfile)
    in 
     (case revisions
        of [] => "no revision number available"
	 | [r] => stripTrailingNewline r
	 | rs => raise Fail (String.concatWith ";" ("too many revisions available\n" :: rs))
        (* end case *))
     before rm tmpfile
    end    

(* alphasort : ('a -> string) -> 'a list -> 'a list *)
(* Provide a function to make strings (for sorting) of given alphas. *)
(* Alphabetical sorting, A to Z. *)
  fun alphasort getKey = let
    fun greaterThan (x1, x2) = String.> (getKey x1, getKey x2)
    in
      ListMergeSort.sort greaterThan
    end

(* antialphasort : ('a -> string) -> 'a list -> 'a list *)
(* Provide a function to make strings (for sorting) of given alphas. *)
(* Alphabetical sorting, Z to A. *)
  fun antialphasort getKey = let
    fun lte (x1, x2) = String.<= (getKey x1, getKey x2)
    in
      ListMergeSort.sort lte
    end

(* which : string -> string option *)
(* Find the named program if possible. *)
  fun which prog = let
    val noNewlines = implode o (List.filter (fn c => c <> #"\n")) o explode
    val p = Unix.execute ("/usr/bin/which", [prog])
    val ins = Unix.textInstreamOf p
    val optFullPath = case TextIO.inputLine ins 
		        of SOME loc => SOME (noNewlines loc) 
			 | NONE => NONE
    in
      Unix.reap p;
      optFullPath 
    end

end
