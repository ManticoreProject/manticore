(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Instream0 =
struct

local
fun String_rev s = let
      val n = String.size s
      val n1 = n - 1
      in
	CharVector.tabulate (n, fn i => String.sub (s, n1 - i))
      end
fun String_fold (s, b, f) = CharVector.foldl f b s
fun Char_equals (c1 : char, c2) = (c1 = c2)
in
structure String = (*String0*)String
structure Char = (*String.Char*)Char
structure I = Pervasive.TextIO
open I

type t = I.instream

val standard = stdIn
val close = closeIn
val inputChar = input1
val peekChar = lookahead   
val endOf = endOfStream

fun foldChars (ins, a, f) =
   let
      fun loop a =
         case inputLine ins of
            NONE => a
          | SOME l => loop ((*String.fold*)String_fold (l, a, f))
   in
      loop a
   end

fun foldLines (ins, ac, f) =
   let
      fun loop ac =
         case inputLine ins of
            NONE => ac
          | SOME l => loop (f (l, ac))
   in loop ac
   end

fun foreachLine (ins, f) = foldLines (ins, (), f o #1)

fun inputTo (i: t, p: char -> bool): string =
   let
      val maxListLength = 1000
      fun finish chars = (*String.rev*)String_rev (String.implode chars)
      fun loop (n, chars, strings) =
         case peekChar i of
            NONE => (chars, strings)
          | SOME c =>
               if p c
                  then (chars, strings)
               else
                  let
                     val chars = c :: chars
                     val _ = inputChar i
                  in
                     if n > 0
                        then loop (n - 1, chars, strings)
                     else loop (maxListLength,
                                [],
                                finish chars :: strings)
                  end
      val (chars, strings) = loop (maxListLength, [], [])
   in
      concat (rev (finish chars :: strings))
   end

fun sameContents (in1, in2) =
   let
      fun loop () =
         case (input1 in1, input1 in2) of
            (NONE, NONE) => true
          | (SOME c1, SOME c2) => (*Char.equals*)Char_equals (c1, c2) andalso loop ()
          | _ => false
   in loop ()
   end

fun inputToSpace i = inputTo (i, Char.isSpace)
fun inputToChar (i, c) = inputTo (i, fn c' => (*Char.equals*)Char_equals (c, c'))
fun ignoreSpaces i = ignore (inputTo (i,not o Char.isSpace))

fun inputNothing _ = ()

fun layout _ = Layout.str "<instream>"

(*val set = MLton.TextIO.setIn *)

end (* local *)
end

structure In0 = Instream0
