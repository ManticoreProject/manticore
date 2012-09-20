(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_INTEGER =
   sig
      type t

      val + : t * t -> t
      val ~ : t -> t
      val * : t * t -> t
      val equals: t * t -> bool
      val layout: t -> Layout.t
      val zero: t

      val - : t * t -> t
      val double: t -> t
      val isZero: t -> bool
      val square: t -> t
      val sum: t list -> t

      val one: t

      val add1: t -> t
      val dec: t ref -> unit
      (* fromInt n = 1 + ... + 1, n times. *)
      val fromInt: (*Pervasive.*)Int.int -> t
      val fromIntInf: (*Pervasive.*)IntInf.int -> t
      val inc: t ref -> unit
      val negOne: t
      val pow: t * (*Pervasive.*)Int.int -> t
      val powInf : t * (*Pervasive.*)IntInf.int -> t
      val pows: (t * (*Pervasive.*)Int.int) list -> t (* simultaneous exponentiation *)
      val powsInf: (t * (*Pervasive.*)IntInf.int) list -> t
      val prod: t list -> t
      val sub1: t -> t
      val three: t
      val two: t

      (* Two elements a, b are "unit equivalent" if there is a unit element u
       * such that au = b.
       *)

      val divMod: t * t -> t * t
      val metric: t -> (*Pervasive.*)IntInf.int
      (* Monics should be an (infinite) stream of all (except for zero and one)
       * the representatives of the unit equivalence classes in nondecreasing
       * order of metric.
       *)
      val monics: t Stream.t
      (* Map an element to the representative of its unit equivalence class. *)
      val unitEquivalent: t -> t

      val div: t * t -> t
      val divides: t * t -> bool
      val extendedEuclid: t * t -> t * t * t
      val extendedEuclidTerm: t * t * (t * t -> bool) -> t * t * t
      val factor: t -> (t * (*Pervasive.*)Int.int) list
      val gcd: t * t -> t
      val isComposite: t -> bool
      val isPrime: t -> bool
      val lcm: t * t -> t
      val primes: t Stream.t
      val mod: t * t -> t
      type int = t

      val < : t * t -> bool
      val <= : t * t -> bool
      val > : t * t -> bool
      val >= : t * t -> bool
      val abs: t -> t
      val choose: t * t -> t
      val compare: t * t -> Relation.t
      val exists: t * t * (t -> bool) -> bool
      val factorial: t -> t
      val fold: t * t * 'a * (t * 'a -> 'a) -> 'a
      val foldDown: t * t * 'a * (t * 'a -> 'a) -> 'a
      val for: t * t * (t -> unit) -> unit
      val forall: t * t * (t -> bool) -> bool
      val format: t * StringCvt.radix -> string 
      val forDown: t * t * (t -> unit) -> unit
      val fromString: string -> t option
      exception Input
      val input: In0.t -> t
      val isEven: t -> bool
      val isNegative: t -> bool
      val isOdd: t -> bool
      val isPositive: t -> bool
      (* largest (i, f) is the largest j <= i such that f j *)
      val largest: t * (t -> bool) -> t
      val least: t * t * (t -> bool) -> t option
      val map: t * t * (t -> 'a) -> 'a list
      val max: t * t -> t
      val min: t * t -> t
      val output: t * Out.t -> unit
      val quot: t * t -> t 
      val quotRem: t * t -> t * t
      val rem: t * t -> t
      val scan: (StringCvt.radix * (char, 'a) StringCvt.reader)
         -> (t, 'a) StringCvt.reader
      (* smallest (i, f) is the smallest j >= i such that f j *)
      val smallest: t * (t -> bool) -> t
      (* val sum: {from: t, to: t, term: t -> t} -> t *)
      val toCommaString: t -> string
      val toInt: t -> (*Pervasive.*)Int.int
      val toIntInf: t -> (*Pervasive.*)IntInf.int
      val toLarge: t -> (*Pervasive.*)LargeInt.int
      val toString: t -> string

      val andb: t * t -> t
      val hash: t -> word
      val log2: t -> Int.int
      val maxPow2ThatDivides: t -> word
      val notb: t -> t
      val orb: t * t -> t
      val xorb: t * t -> t
      val << : t * word -> t
      val ~>> : t * word -> t
   end

structure MLtonIntInf : MLTON_INTEGER =
struct

structure List = MLtonList

open IntInf

structure In = In0

type t = int

val zero = fromInt 0
val one = fromInt 1

val layout = Layout.str o toString

val equals : int * int -> bool = op =

fun isZero a = equals(a, zero)

fun double n = n + n

fun square n = n * n

fun sum l = MLtonList.fold(l, zero, op +)

val base = {one = one, layout = layout, times = op *}
val pow = Power.power base
val powInf = Power.powerInf base
val pows = Power.simultaneous base
val powsInf = Power.simultaneousInf base

(*
local
   fun 'a
      make {zero: 'a, < : 'a * 'a -> bool, ~ : 'a -> 'a,
            power: {one: t,
                    layout: t -> Layout.t,
                    times: t * t -> t
                    } -> (t * 'a) -> t}
      (i: 'a) : t =
      let
         val (i, fix) =
            if i < zero
               then (~ i, IntInf.~)
            else (i, fn x => x)
      val i = power{one = zero, layout = layout, times = op +} (one, i)
      in fix i
      end
in
   val fromInt = make{zero = 0,
                      < = op <,
                      ~ = Pervasive.Int.~,
                      power = Power.power}
   val fromIntInf = make{zero = 0,
                         < = IntInf.<,
                         ~ = IntInf.~,
                         power = Power.powerInf}
end
*)
val fromInt = IntInf.fromInt
fun toIntInf (i : t) = i
fun fromIntInf (i : IntInf.int) = i

(* val fromIntInf =
 *    Trace.trace("fromIntInf", Layout.str o IntInf.toString, layout) fromIntInf
 *)

fun add1 i = i + one

fun sub1 i = i - one

fun inc r = r := add1(!r)

fun dec r = r := sub1(!r)

fun prod l = List.fold(l, one, op * )

val negOne = sub1 zero

val two = add1 one

val three = add1 two

val pows =
   Trace.traceAssert
   ("RingWithIdentity.pows",
    List.layout (Layout.tuple2 (layout, Layout.str o Int.toString)),
    layout,
    fn l => (true, fn r => equals (r, List.fold (l, one, fn ((b, e), ac) =>
                                                 ac * pow (b, e)))))
   pows

val powsInf =
   Trace.traceAssert
   ("RingWithIdentity.powsInf",
    List.layout (Layout.tuple2 (layout, Layout.str o Pervasive.IntInf.toString)),
    layout,
    fn l => (true, fn r => equals (r, List.fold (l, one, fn ((b, e), ac) =>
                                                 ac * powInf (b, e)))))
   powsInf

fun isPositive n = n > zero

fun isNegative n = n < zero

fun abs n = if isPositive n then n else ~ n

fun foldl(from, to, b, f) =
   let fun fold(n, a) = if n > to then a
                       else fold(add1 n, f(a,n))
   in fold(from, b)
   end

local 
   fun abs (combine, base) {from, to, term} =
      foldl(from, to, base, fn (a, i) => combine(a, term i))
in
   val sumFromTo = abs(op +, zero)
   val prodFromTo = abs(op *, one)
end

fun factorial n = prodFromTo{from = one, to = n, term = fn i => i}

fun max(m, n) = if m > n then m else n

fun min(m, n) = if m < n then m else n

exception Input
fun input i = (In.ignoreSpaces i
               ; (case fromString (In.inputToSpace i) of
                     NONE => raise Input
                   | SOME n => n))

val metric = toIntInf o abs
val monics = Stream.infinite (two, fn n => n + one)
val unitEquivalent = abs

val divMod =
   Trace.traceAssert
   ("EuclideanRing.divMod",
    Layout.tuple2(layout, layout),
    Layout.tuple2(layout, layout),
    fn (p, q) => (not(equals(q, zero)),
                  fn (d, m) => (equals(p, q * d + m)
                                andalso (equals(m, zero)
                                         orelse IntInf.<(metric m, metric q)))))
   divMod

fun p div q = #1(divMod(p, q))

fun p mod q = #2(divMod(p, q))

fun divides(d: t, x: t): bool = equals(x mod d, zero)

val divides =
   Trace.trace("EuclideanRing.divides", Layout.tuple2(layout, layout), (*Bool.layout*)Layout.str o Bool.toString) divides

(* Taken from page 812 of CLR. *)
fun extendedEuclidTerm(a: t, b: t, done: t * t -> bool, trace): t * t * t =
   let
      fun loop(a, b) =
         if done(a, b)
            then (a, one, zero)
         else let val (d, m) = divMod(a, b)
                  val (d', x', y') = loop(b, m)
              in (d', y', x' - d * y')
              end
   in trace loop(a, b)
   end

fun makeTraceExtendedEuclid f =
   Trace.traceAssert
   ("EuclideanRing.extendedEuclid",
    Layout.tuple2(layout, layout),
    Layout.tuple3(layout, layout, layout),
    fn (a, b) => (not(isZero a) andalso not(isZero b),
                  fn (d, x, y) => (f(d, x, y)
                                   andalso equals(d, a * x + b * y))))

local
   val trace =
      makeTraceExtendedEuclid
      (fn (d, x, y) => divides(d, x) andalso divides(d, y))
in
   (* Page 72 of Bach and Shallit. *)
   (* Identical to algorithm on page 23 of Berlekamp. *)
   (* This algorithm is slower (about 2x) than the recursive extendedEuclid
    * given above, but stores only a constant number of ring elements.
    * Thus, for now, it is overridden below.
    *)
   fun extendedEuclid(u0: t, u1: t): t * t * t =
      let
         val rec loop =
            fn (r as {m11, m12, m21, m22, u, v, nEven}) =>
            (Assert.assert("EuclideanRing.extendedEuclid", fn () =>
                           equals(u0, m11 * u + m12 * v)
                           andalso equals(u1, m21 * u + m22 * v)
                           andalso equals(if nEven then one else negOne,
                                          m11 * m22 - m12 * m21))
             ; if isZero v
                  then r
               else 
                  let val (q, r) = divMod(u, v)
                  in loop{m11 = q * m11 + m12,
                          m12 = m11,
                          m21 = q * m21 + m22,
                          m22 = m21,
                          u = v,
                          v = r,
                          nEven = not nEven}
                  end)
         val {m12, m22, u, nEven, ...} =
            loop{m11 = one, m12 = zero, m21 = zero, m22 = one,
                 u = u0, v = u1, nEven = true}
         val (a, b) = if nEven then (m22, ~m12) else (~m22, m12)
      in (u, a, b)
      end

   val _ = extendedEuclid

   fun extendedEuclid (a, b) =
      extendedEuclidTerm (a, b, fn (_, b) => equals (b, zero), trace)
end   

local
   val trace = makeTraceExtendedEuclid(fn _ => true)
in
   val extendedEuclidTerm =
      fn (a, b, done) => extendedEuclidTerm(a, b, done, trace)
end

val lastPrime = ref one

fun gcd(a, b) = if isZero b then a else gcd(b, a mod b)

fun lcm(a, b) = (a * b) div gcd(a, b)

val primes: t Stream.t =
   let
      fun loop(s: t Stream.t) =
         Stream.delay
         (fn () => 
          let val (p, s) = valOf(Stream.force s)
             val _ = lastPrime := p
          in Stream.cons
             (p, loop(Stream.keep(s, fn x => not(divides(p, x)))))
          end)
   in loop monics
   end

type factors = (t * Int.int) list

fun factor(n: t): factors =
   let
      fun loop(n: t, primes: t Stream.t, factors: factors) =
         if equals(n, one)
            then factors
         else let val (p, primes) = valOf(Stream.force primes)
                  val (n, k) =
                     let
                        fun loop(n, k) =
                           let val (q, r) = divMod(n, p)
                           in if isZero r
                                 then loop(q, Int.+(k, 1))
                              else (n, k)
                           end
                     in loop(n, 0)
                     end
              in loop(n, primes,
                      if k = 0
                         then factors
                      else (p, k) :: factors)
              end
   in loop(n, primes, [])
   end

val factor =
   Trace.traceAssert
   ("EuclideanRing.factor", layout, List.layout (Layout.tuple2(layout, Layout.str o Int.toString)),
    fn n => (not(isZero n), fn factors =>
             equals(n, List.fold(factors, one, fn ((p, k), prod) =>
                                 prod * pow (p, k)))))
   factor

fun existsPrimeOfSmallerMetric(m: IntInf.int, f: t -> bool): bool =
   let
      fun loop primes =
         let val (p, primes) = valOf(Stream.force primes)
         in IntInf.<(metric p, m)
            andalso (f p orelse loop primes)
         end
   in loop primes
   end

fun isPrime(r: t): bool =
   let val r = unitEquivalent r
   in existsPrimeOfSmallerMetric(IntInf.+ (metric r, 1),
                                 fn p => equals(r, p))
   end

fun isComposite(r: t): bool =
   existsPrimeOfSmallerMetric(metric r, fn p => divides(p, r))

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

fun scan (radix, reader) = IntInf.scan radix reader

fun format (i, r) = fmt r i


val hash = let
      val prime =
       (Word.toLargeIntX o Word.~ o Word.fromInt)
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
	 fn i => Word.fromLargeInt (i mod prime)
      end

structure M = MaxPow2ThatDivides (type t = t
				  val equals = equals
				  val orb = orb
				  val zero = zero
				  val one = one
				  val andb = andb
				  val orb = orb
				  val << = <<
				  val >> = ~>>
				  val op <= = op <=)
open M

end
