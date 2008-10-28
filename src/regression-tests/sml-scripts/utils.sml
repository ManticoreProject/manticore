(* utils.sml
 *
 * COPYRIGHT (c) 2008 Adam Shaw (http://people.cs.uchicago.edu/~adamshaw)
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
    fun loop n = let
      val f = concat [fullpath, "/tmpfile", Int.toString n, ".", suffix]
      in 
        if not (exists f)
	then f
	else loop (n+1)
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
    val inStream = TextIO.openIn filename
    fun read acc =
     (case TextIO.inputLine inStream
        of NONE => rev acc
	 | SOME s => read (s :: acc)
        (* end case *))
    in
      read []
    end

end
