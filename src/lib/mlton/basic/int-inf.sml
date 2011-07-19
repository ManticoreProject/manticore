(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure IntInf: INT_INF =
   struct
   open Pervasive.IntInf

structure In = In0

structure Int =
   struct
      open Pervasive.IntInf

      type t = int

      val zero = fromInt 0

      val one = fromInt 1

      val layout = Layout.str o toString

      val equals = op =
   end

structure R =
   OrderedRing (structure R = 
                   RingWithIdentity (structure R = Ring (Int)
                                     open R Pervasive.IntInf
                                     val one = fromInt 1)
                open R Pervasive.IntInf
                val {compare, ...} =
                   Relation.lessEqual {< = op <, equals = equals})
open R

exception Input
fun input i = (In.ignoreSpaces i
               ; (case fromString (In.inputToSpace i) of
                     NONE => raise Input
                   | SOME n => n))

structure I = EuclideanRing (open R Pervasive.IntInf
                                  val toIntInf = fn x => x
                            val metric = toIntInf o abs
                            val monics = Stream.infinite (two, fn n => n + one)
                            val unitEquivalent = abs
    
)
open I

fun isEven n = isZero (n mod two)

val isOdd = not o isEven

fun toCommaString n =
   let
      fun loop (chars, accum) =
         let
            fun done () = implode (rev chars @ accum)
         in
            case chars of
               x1 :: x2 :: x3 :: chars =>
                  (case chars of
                      [] => done ()
                    | [#"~"] => done ()
                    | _ => loop (chars, #"," :: x3 :: x2 :: x1 :: accum))
             | _ => done ()
         end
   in loop (rev (explode (toString n)), [])
   end

fun choose (n, k) =
   let val k = max (k, n - k)
   in prodFromTo {from = add1 k, to = n, term = fn i => i}
      div factorial (n - k)
   end

fun output (n, out) = Out.output (out, toString n)

fun largest (i, f) =
   let
      fun loop (n: t) =
         if f n
            then n
         else loop (sub1 n)
   in
      loop i
   end

fun smallest (i, f) =
   let
      fun loop (n: t) =
         if f n
            then n
         else loop (add1 n)
   in loop i
   end

fun least (start: t, stop: t, f: int -> bool): int option =
   let
      fun loop (i: t) =
         if i >= stop
            then NONE
         else if f i
                 then SOME i
              else loop (i + one)
   in loop start
   end

fun 'a fold (start: t, stop: t, a: 'a, f: int * 'a -> 'a): 'a =
   let
      val _ = Assert.assert ("Integer.fold", fn () => start <= stop + one)
      fun loop (i: t, a: 'a): 'a =
         if i >= stop
            then a
         else loop (i + one, f (i, a))
   in loop (start, a)
   end

fun forall (start: t, stop: t, f: int -> bool): bool =
   Exn.withEscape
   (fn escape => (fold (start, stop, (), fn (i, ()) =>
                       if f i then () else escape false)
                  ; true))

fun exists (start, stop, f) = not (forall (start, stop, not o f))

fun 'a foldDown (start: t, stop: t, a: 'a, f: int * 'a -> 'a): 'a =
   let
      val _ = Assert.assert ("Integer.foldDown", fn () => start <= stop + one)
      fun loop (i: t, a: 'a) =
         if i < start
            then a
         else loop (sub1 i, f (i, a))
   in loop (sub1 stop, a)
   end

fun map (start: t, stop: t, f: t -> 'a): 'a list =
   foldDown (start, stop, [], fn (i, l) => f i :: l)

fun for (start: t, stop: t, f: t -> unit): unit =
   fold (start, stop, (), f o #1)

fun forDown (start: t, stop: t, f: t -> unit): unit =
   foldDown (start, stop, (), f o #1)

fun scan (radix, reader) = Int.scan radix reader

fun format (i, r) = fmt r i

      fun toIntInf x = x

      val hash = let
         val prime =
             (Word.toIntInf o Word.~ o Word.fromInt)
              (case Word.wordSize of
                  6 => 3
                | 7 => 1
                | 8 => 5
                | 14 => 3
                | 15 => 19
                | 16 => 15
                | 30 => 35
                | 31 => 1
                | 32 => 5
                | 62 => 57
                | 63 => 25
                | 64 => 59
                | 126 => 137
                | 127 => 1
                | 128 => 159
                | _ => Error.bug "Unknown Word.wordSize")
      in
         fn i => Word.fromIntInf (i mod prime)
      end

      local
         open Pervasive.IntInf
      in
         val andb = andb
         val log2 = log2
         val notb = notb
         val orb = orb
         val xorb = xorb
         val op ~>> = ~>>
         val op << = <<
      end

      structure M = MaxPow2ThatDivides (open IntInf Int
                                        val andb = andb
                                        val orb = orb
                                        val << = <<
                                        val >> = ~>>)
      open M
   end

structure LargeInt = IntInf
