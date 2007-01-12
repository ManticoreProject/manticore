(* utf8-sig.sml
 *
 * COPYRIGHT (c) 2004 The Moby Project (moby.cs.uchicago.edu)
 * All rights reserved.
 *
 * Routines for working with UTF8 encoded strings.
 *)

signature UTF8 =
  sig

    type wchar = Word.word

    exception Incomplete
	(* raised by some operations when applied to incomplete strings. *)

    val explode : string -> wchar list
	(* return the list of wide characters that are encoded by a string *)
    val fold : ((wchar * 'a) -> 'a) -> 'a -> string -> 'a
	(* fold a function over the Unicode characters in the string *)
    val size : string -> int
	(* return the number of Unicode characters *)

    val encode : wchar -> string
	(* return the UTF8 encoding of a wide character *)

    val isAscii : wchar -> bool
    val toAscii : wchar -> char		(* truncates to 7-bits *)
    val fromAscii : char -> wchar	(* truncates to 7-bits *)

    val toString : wchar -> string
	(* return a printable string representation of a wide character *)

  end

