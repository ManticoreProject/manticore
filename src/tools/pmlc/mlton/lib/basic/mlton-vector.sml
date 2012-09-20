(* Copyright (C) 2012 John Reppy
 * Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_VECTOR =
   sig
      type 'a t = 'a vector
      exception New

      val length: 'a t -> int
      val sub: 'a t * int -> 'a
      val unfoldi: int * 'b * (int * 'b -> 'a * 'b) -> 'a t * 'b

      val compare: 'a t * 'a t * ('a * 'a -> order) -> order
      val concat: 'a t list -> 'a t
      val concatV: 'a t t -> 'a t
      val contains: 'a t * 'a * ('a * 'a -> bool) -> bool
      val copy: 'a t -> 'a t
      val dropPrefix: 'a t * int -> 'a t
      val dropSuffix: 'a t * int -> 'a t
      val equals: 'a t * 'b t * ('a * 'b -> bool) -> bool
      val exists: 'a t * ('a -> bool) -> bool
      val existsi: 'a t * (int * 'a -> bool) -> bool
      val existsR: 'a t * int * int * ('a -> bool) -> bool
      val fold2: 'a t * 'b t * 'c * ('a * 'b * 'c -> 'c) -> 'c
      val fold3From:
         'a t * 'b t * 'c t * int * 'd * ('a * 'b * 'c * 'd -> 'd) -> 'd
      val fold3: 'a t * 'b t * 'c t * 'd * ('a * 'b * 'c * 'd -> 'd) -> 'd
      datatype ('a, 'b) continue =
         Continue of 'a
       | Done of 'b
      (* fold' (v, i, b, f, g)
       * folds over v starting at index i with state b, applying f to each
       * index, vector element, and state, continuing depending on what f
       * returns.  If the end of the vector is reached, g is applied to the
       * state.
       *)
      val fold':
         'a t * int * 'b * (int * 'a * 'b -> ('b, 'c) continue) * ('b -> 'c)
         -> 'c
      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val foldFrom: 'a t * int * 'b * ('a * 'b -> 'b) -> 'b
      val foldi: 'a t * 'b * (int * 'a * 'b -> 'b) -> 'b
      val foldi2From: 'a t * 'b t * int * 'c * (int * 'a * 'b * 'c -> 'c) -> 'c
      val foldr: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val foldri: 'a t * 'b * (int * 'a * 'b -> 'b) -> 'b
      val foldr2: 'a t * 'b t * 'c * ('a * 'b * 'c -> 'c) -> 'c
      val forall: 'a t * ('a -> bool) -> bool
      val forall2: 'a t * 'b t * ('a * 'b -> bool) -> bool
      val foralli: 'a t * (int * 'a -> bool) -> bool
      val foreach: 'a t * ('a -> unit) -> unit
      val foreachi: 'a t * (int * 'a -> unit) -> unit
      val foreachi2: 'a t * 'b t * (int * 'a * 'b -> unit) -> unit
      val foreachr: 'a t * ('a -> unit) -> unit
      val foreachri: 'a t * (int * 'a -> unit) -> unit
      val foreach2: 'a t * 'b t * ('a * 'b -> unit) -> unit
      val foreachR: 'a t * int * int * ('a -> unit) -> unit
      val foreach3: 'a t * 'b t * 'c t * ('a * 'b * 'c -> unit) -> unit
      val fromArray: 'a array -> 'a t
      val fromList: 'a list -> 'a t
      val fromListMap: 'a list * ('a -> 'b) -> 'b t
      val fromListRev: 'a list -> 'a t
      val indexi: 'a t * (int * 'a -> bool) -> int option
      val index: 'a t * ('a -> bool) -> int option
      val indices: bool t -> int t
      val isEmpty: 'a t -> bool
      val isSorted: 'a t * ('a * 'a -> bool) -> bool
      (* isSortedRange (v, l, u, <=) checks if [l, u) is sorted. *)
      val isSortedRange: 'a t * int * int * ('a * 'a -> bool) -> bool
      val isSubsequence: 'a t * 'b t * ('a * 'b -> bool) -> bool
      val keepAll: 'a t * ('a -> bool) -> 'a t
      val keepAllMap: 'a t * ('a -> 'b option) -> 'b t
      val keepAllMapi: 'a t * (int * 'a -> 'b option) -> 'b t
      val keepAllMap2: 'a t * 'b t * ('a * 'b -> 'c option) -> 'c t
      val keepAllSome: 'a option t -> 'a t
      val last: 'a t -> 'a
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val loop: 'a t * ('a -> 'b option) * (unit -> 'b) -> 'b
      val loopi: 'a t * (int * 'a -> 'b option) * (unit -> 'b) -> 'b
      val map: 'a t * ('a -> 'b) -> 'b t
      val map2: 'a t * 'b t * ('a * 'b -> 'c) -> 'c t
      val map3: 'a t * 'b t * 'c t * ('a * 'b * 'c -> 'd) -> 'd t
      val mapAndFold: 'a t * 'b * ('a * 'b -> 'c * 'b) -> 'c t * 'b
      val map2AndFold: 'a t * 'b t * 'c * ('a * 'b * 'c -> 'd * 'c) -> 'd t * 'c
      val mapi: 'a t * (int * 'a -> 'b) -> 'b t
      val new: int * 'a -> 'a t
      val new0: unit -> 'a t
      val new1: 'a -> 'a t
      val new2: 'a * 'a -> 'a t
      val new3: 'a * 'a * 'a -> 'a t
      val new4: 'a * 'a * 'a * 'a -> 'a t
      val new5: 'a * 'a * 'a * 'a * 'a -> 'a t
      val new6: 'a * 'a * 'a * 'a * 'a * 'a -> 'a t
      val partition: 'a t * ('a -> bool) -> {no: 'a t, yes: 'a t}
      val partitioni: 'a t * (int * 'a -> bool) -> {no: 'a t, yes: 'a t}
      val peek: 'a t * ('a -> bool) -> 'a option
      val peeki: 'a t * (int * 'a -> bool) -> (int * 'a) option
      val peekMap: 'a t * ('a -> 'b option) -> 'b option
      val peekMapi: 'a t * ('a -> 'b option) -> (int * 'b) option
      val prefix: 'a t * int -> 'a t
      val randomElement: 'a t -> 'a
      val removeDuplicates: 'a t * ('a * 'a -> bool) -> 'a t
      val removeFirst: 'a t * ('a -> bool) -> 'a t
      val rev: 'a t -> 'a t
      val size: 'a t -> int
      val splitLast: 'a t -> 'a t * 'a
      val tabulate: int * (int -> 'a) -> 'a t
      val tabulator: int * (('a -> unit) -> unit) -> 'a t
      val toArray: 'a t -> 'a array
      val toList: 'a t -> 'a list
      val toListMap: 'a t * ('a -> 'b) -> 'b list
      val toListRev: 'a t -> 'a list
      val toString: ('a -> string) -> 'a t -> string
      val unzip: ('a * 'b) t -> 'a t * 'b t
      val unzip3: ('a * 'b * 'c) t -> 'a t * 'b t * 'c t
      val zip: 'a t * 'b t -> ('a * 'b) t
   end

structure MLtonVector : MLTON_VECTOR =
struct

(*open S*)
type 'a t = 'a vector
exception New = Size
val length = Vector.length
val sub = Vector.sub
fun unfoldi (n, a, f) = let
      val r = ref a
      val v = Vector.tabulate (n, fn i => let
		val (b, a') = f (i, !r)
		val _ = r := a'
		in
		  b
		end)
      in
	(v, !r)
      end
val unsafeSub = Unsafe.Vector.sub

val size = length

fun unfold (n, a, f) = unfoldi (n, a, f o #2)

(*fun tabulate (n, f) = #1 (unfoldi (n, (), fn (i, ()) => (f i, ())))*)
val tabulate = Vector.tabulate

fun fromArray a =
   tabulate (Array.length a, fn i => Unsafe.Array.sub (a, i))

fun toArray v =
   Array.tabulate (length v, fn i => sub (v, i))

datatype ('a, 'b) continue =
   Continue of 'a
  | Done of 'b

fun fold' (v, start, b, f, g) =
   let
      val n = length v
      fun loop (i, b) =
         if i >= n
            then g b
         else
            case f (i, unsafeSub (v, i), b) of
               Continue b => loop (i + 1, b)
             | Done c => c
   in
      if 0 <= start andalso start <= n
         then loop (start, b)
      else Error.bug "Vector.fold'"
   end

fun foldFrom (v, start, b, f) =
   fold' (v, start, b,
          fn (_, a, b) => Continue (f (a, b)),
          fn b => b)

fun fold (a, b, f) = foldFrom (a, 0, b, f)

fun isEmpty a = 0 = length a

fun dropPrefix (v, n) = tabulate (length v - n, fn i => sub (v, i + n))

fun dropSuffix (v, n) = tabulate (length v - n, fn i => sub (v, i))

fun new (n, x) = tabulate (n, fn _ => x)

fun mapi (a, f) = tabulate (length a, fn i => f (i, unsafeSub (a, i)))

fun map (v, f) = mapi (v, f o #2)

fun copy v = map (v, fn x => x)

fun existsR (v, start, stop, f) =
   fold' (v, start, (),
          fn (i, a, ()) => if i = stop
                              then Done false
                           else if f a
                                   then Done true
                                else Continue (),
          fn _ => false)

fun foldi (v, b, f) = fold' (v, 0, b, Continue o f, fn b => b)

fun loopi (v, f, g) =
   fold' (v, 0, (),
          fn (i, a, ()) => (case f (i, a) of
                               NONE => Continue ()
                             | SOME b => Done b),
          g)

fun loop (v, f, g) = loopi (v, f o #2, g)

fun peekMapi (v, f) =
   let
      val n = length v
      fun loop i =
         if i = n
            then NONE
         else
            (case f (sub (v, i)) of
                NONE => loop (i + 1)
              | SOME b => SOME (i, b))
   in
      loop 0
   end

fun peekMap (v, f) =
   loop (v,
         fn a => (case f a of
                     NONE => NONE
                   | z => SOME z),
         fn () => NONE)

fun fromListMap (l, f) =
   let
      val r = ref l
   in
      tabulate (List.length l, fn _ =>
                case !r of
                   [] => Error.bug "Vector.fromListMap"
                 | x :: l => (r := l; f x))
   end

fun fromList l = fromListMap (l, fn x => x)

fun foldr2 (a, a', b, f) =
   let
      val n = length a
      val n' = length a'
      fun loop (i, b) =
         if i < 0
            then b
         else loop (i - 1, f (unsafeSub (a, i), unsafeSub (a', i), b))
   in
      if n = n'
         then loop (n - 1, b)
      else Error.bug "Vector.foldr2"
   end

fun foldi2From (a, a', start, b, f) =
   let
      val n = length a
      val n' = length a'
      fun loop (i, b) =
         if i >= n
            then b
         else loop (i + 1, f (i, unsafeSub (a, i), unsafeSub (a', i), b))
   in
      if n = n' andalso 0 <= start andalso start <= n 
         then loop (start, b)
      else Error.bug "Vector.foldi2From"
   end

fun foldi2 (a, a', b, f) = foldi2From (a, a', 0, b, f)

fun foreachi2 (v, v', f) =
   foldi2 (v, v', (), fn (i, x, x', ()) => f (i, x, x'))

fun fold2 (a, a', b, f) =
   foldi2 (a, a', b, fn (_, x, x', b) => f (x, x', b))

fun fold3From (a, a', a'', start, b, f) =
   let
      val n = length a
      val n' = length a'
      val n'' = length a''
      fun loop (i, b) =
         if i >= n
            then b
         else loop (i + 1, f (unsafeSub (a, i),
                              unsafeSub (a', i),
                              unsafeSub (a'', i),
                              b))
   in
      if n = n' andalso n = n'' andalso 0 <= start andalso start <= n
         then loop (start, b)
      else Error.bug "Vector.fold3From"
   end

fun fold3 (a, a', a'', b, f) = fold3From (a, a', a'', 0, b, f)

fun foreachR (v, start, stop, f: 'a -> unit) =
   if 0 <= start andalso start <= stop andalso stop <= length v
      then
         let
            fun step (i, a, ()) =
               if i >= stop
                  then Done ()
               else (f a; Continue ())
         in
            fold' (v, start, (), step, fn () => ())
         end
   else Error.bug "Vector.foreachR"

fun foreach2 (a, a', f) =
   fold2 (a, a', (), fn (x, x', ()) => f (x, x'))

fun forall2 (v, v', f) =
   let
      val n = length v
      fun loop i =
         i = n
         orelse (f (sub (v, i), sub (v', i))
                 andalso loop (i + 1))
   in
      if n = length v'
         then loop 0
      else Error.bug "Vector.forall2"
   end

fun foreach3 (v1, v2, v3, f: 'a * 'b * 'c -> unit) =
   let
      val n = length v1
      val _ =
         if n = length v2 andalso n = length v3
            then ()
         else Error.bug "Vector.foreach3"
      fun loop i =
         if i = n
            then ()
         else (f (sub (v1, i), sub (v2, i), sub (v3, i))
               ; loop (i + 1))
   in
      loop 0
   end

fun foreachi (a, f) = foldi (a, (), fn (i, x, ()) => f (i, x))

fun foreach (a, f) = foreachi (a, f o #2)

fun 'a peeki (v, f) =
   let
      val n = length v
      fun loop i =
         if i = n
            then NONE
         else let
                 val x = sub (v, i)
              in
                 if f (i, x)
                    then SOME (i, x)
                 else loop (i + 1)
              end
   in
      loop 0
   end

fun peek (a, f) = MLtonOption.map (peeki (a, f o #2), #2)

fun existsi (a, f) = isSome (peeki (a, f))

fun exists (a, f) = existsi (a, f o #2)

fun contains (v, a, f) = exists (v, fn a' => f (a, a'))

fun foralli (a, f) = not (existsi (a, not o f))

fun forall (a, f) = foralli (a, f o #2)

fun equals (a, a', equals) =
   length a = length a'
   andalso foralli (a, fn (i, x) => equals (x, unsafeSub (a', i)))

fun foldri (a, b, f) = let
      fun loop (i, acc) = if (i < 0)
	    then acc
	    else loop (i-1, f(i, unsafeSub(a, i), acc))
      in
	loop (length a - 1, b)
      end
(* was:
   Int.foldDown (0, length a, b, fn (i, b) => f (i, unsafeSub (a, i), b))
*)

fun foldr (a, b, f) =
   foldri (a, b, fn (_, a, b) => f (a, b))

fun foreachri (a, f) = foldri (a, (), fn (i, x, ()) => f (i, x))

fun foreachr (a, f) = foreachri (a, f o #2)

fun toList a = foldr (a, [], op ::)

fun toListMap (a, f) = foldr (a, [], fn (a, ac) => f a :: ac)

fun layout l v = Layout.tuple (toListMap (v, l))

fun toString xToString l =
   Layout.toString (layout (Layout.str o xToString) l)

fun new0 () = tabulate (0, fn _ => Error.bug "Vector.new0")

fun new1 x = tabulate (1, fn _ => x)

fun new2 (x0, x1) = tabulate (2, fn 0 => x0 | 1 => x1 | _ => Error.bug "Vector.new2")

fun new3 (x0, x1, x2) =
   tabulate (3,
             fn 0 => x0
              | 1 => x1
              | 2 => x2
              | _ => Error.bug "Vector.new3")

fun new4 (x0, x1, x2, x3) =
   tabulate (4,
             fn 0 => x0
              | 1 => x1
              | 2 => x2
              | 3 => x3
              | _ => Error.bug "Vector.new4")

fun new5 (x0, x1, x2, x3, x4) =
   tabulate (5,
             fn 0 => x0
              | 1 => x1
              | 2 => x2
              | 3 => x3
              | 4 => x4
              | _ => Error.bug "Vector.new5")

fun new6 (x0, x1, x2, x3, x4, x5) =
   tabulate (6,
             fn 0 => x0
              | 1 => x1
              | 2 => x2
              | 3 => x3
              | 4 => x4
              | 5 => x5
              | _ => Error.bug "Vector.new6")

fun unzip (a: ('a * 'b) t) = (map (a, #1), map (a, #2))

fun unzip3 (a: ('a * 'b * 'c) t) = (map (a, #1), map (a, #2), map (a, #3))

fun rev v =
   let
      val n = length v
      val n1 = n - 1
   in
      tabulate (n, fn i => unsafeSub (v, n1 - i))
   end

fun fromListRev l = rev (fromList l)

fun mapAndFold (v, b, f) =
   let
      val r = ref b
      val v = map (v, fn x =>
                   let
                      val (c, b) = f (x, !r)
                      val _ = r := b
                   in c
                   end)
   in (v, !r)
   end

fun map2i (v, v', f) =
   let
      val n = length v
   in
      if n = length v'
         then tabulate (n, fn i => f (i, unsafeSub (v, i), unsafeSub (v', i)))
      else Error.bug "Vector.map2i"
   end

fun map2 (v, v', f) = map2i (v, v', fn (_, x, x') => f (x, x'))

fun map2AndFold (v, v', b, f) =
   let
      val r = ref b
      val v =
         map2 (v, v', fn (x, x') =>
               let
                  val (y, b) = f (x, x', !r)
                  val _ = r := b
               in y
               end)
   in (v, !r)
   end

fun map3 (v1, v2, v3, f) =
   let
      val n = length v1
   in
      if n = length v2 andalso n = length v3
         then tabulate (n, fn i => f (unsafeSub (v1, i),
                                      unsafeSub (v2, i),
                                      unsafeSub (v3, i)))
      else Error.bug "Vector.map3"
   end

fun zip (v, v') = map2 (v, v', fn z => z)

local
   fun doit (f, mapi) =
      let
         val n = ref 0
         val b = mapi (fn x =>
                       let
                          val b = f x
                          val _ = if isSome b then n := 1 + !n else ()
                       in b
                       end)
         val r = ref 0
         fun loop (i: int) =
            case unsafeSub (b, i) of
               NONE => loop (i + 1)
             | SOME b => (r := i + 1; b)
      in tabulate (!n, fn _ => loop (!r))
      end
in
   fun keepAllMapi (a, f) = doit (f, fn f => mapi (a, f))
   fun keepAllMap2i (a, b, f) = doit (f, fn f => map2i (a, b, f))
end

fun keepAllMap (v, f) = keepAllMapi (v, f o #2)

fun keepAllMap2 (v, v', f) = keepAllMap2i (v, v', fn (_, x, x') => f (x, x'))

fun keepAllSome v = keepAllMap (v, fn a => a)

fun keepAll (v, f) = keepAllMap (v, fn a => if f a then SOME a else NONE)

fun compare (v, v', comp) =
   let
      val n = length v
      val n' = length v'
   in
      Relation.lexico
      (Int.compare (n, n'), fn () =>
       let
          fun loop i =
             if i = n
                then EQUAL
             else 
                Relation.lexico
                (comp (unsafeSub (v, i), unsafeSub (v', i)), fn () =>
                 loop (i + 1))
       in
          loop 0
       end)
   end

fun toListRev v = fold (v, [], op ::)

fun last v =
   let
      val n = length v
   in
      if n = 0
         then Error.bug "Vector.last"
      else unsafeSub (v, n - 1)
   end

fun tabulator (n: int, f: ('a -> unit) -> unit) =
   let
      val a = Pervasive.Array.array (n, NONE)
      val r = ref 0
      val _ =
         f (fn x =>
            let
               val i = !r
            in
               if i >= n
                  then Error.bug "Vector.tabulator: too many elements"
               else (Pervasive.Array.update (a, i, SOME x)
                     ; r := i + 1)
            end)
   in
      if !r < n
         then Error.bug "Vector.tabulator: not enough elements"
      else tabulate (n, fn i => valOf (Pervasive.Array.sub (a, i)))
   end

fun 'a concat (vs: 'a t list): 'a t =
   case vs of
      [] => new0 ()
    | v :: vs' => 
         let
            val n = MLtonList.fold (vs, 0, fn (v, s) => s + length v)
         in
            #1 (unfold (n, (0, v, vs'),
                        let
                           fun loop (i, v, vs) =
                              if i < length v
                                 then (sub (v, i), (i + 1, v, vs))
                              else
                                 case vs of
                                    [] => Error.bug "Vector.concat"
                                  | v :: vs => loop (0, v, vs)
                        in loop
                        end))
         end

fun concatV vs =
   if 0 = length vs then
      new0 ()
   else
      let
         val n = fold (vs, 0, fn (v, s) => s + length v)
         fun state i = (i, sub (vs, i), 0)
      in
         #1 (unfold (n, state 0,
                     let
                        fun loop (i, v, j) =
                           if j < length v then
                              (sub (v, j), (i, v, j + 1))
                           else
                              loop (state (i + 1))
                     in loop
                     end))
      end

fun splitLast v =
   let
      val n = length v
   in
      if n <= 0
         then Error.bug "Vector.splitLast"
      else (tabulate (n - 1, fn i => unsafeSub (v, i)),
            unsafeSub (v, n - 1))
   end

fun isSortedRange (v: 'a t,
                   start: int,
                   stop: int,
                   le : 'a * 'a -> bool): bool =
   (Assert.assert
    ("Vector.isSortedRange", fn () =>
     0 <= start andalso start <= stop andalso stop <= length v)
    ; start = stop
      orelse
      let
         fun loop (i, prev) =
            i >= stop
            orelse let val cur = sub (v, i)
                   in
                      le (prev, cur)
                      andalso loop (i + 1, cur)
                   end
      in loop (start + 1, sub (v, start))
      end)

fun isSorted (v, op <=) = isSortedRange (v, 0, length v, op <=)

fun indexi (v, f) =
   fold' (v, 0, (),
          fn (i, a, _) => if f (i, a) then Done (SOME i) else Continue (),
          fn _ => NONE)

fun index (v, f) = indexi (v, f o #2)

fun indices (a: bool t): int t =
   keepAllMapi (a, fn (i, b) => if b then SOME i else NONE)

val indices =
   Trace.trace ("Vector.indices", layout (Layout.str o Bool.toString), layout (Layout.str o Int.toString))
   indices

fun isSubsequence (va, vb, f) =
   let
      val na = length va
      val nb = length vb
      fun loop (ia, ib) =
         ia >= na
         orelse let
                   val a = sub (va, ia)
                   fun loop' ib =
                      ib < nb
                      andalso if f (a, sub (vb, ib))
                                 then loop (ia + 1, ib + 1)
                              else loop' (ib + 1)
                in
                   loop' ib
                end
   in
      loop (0, 0)
   end

fun removeFirst (v, f) =
   let
      val seen = ref false
      val v = keepAll (v, fn a =>
                       not (f a)
                       orelse (!seen)
                       orelse (seen := true
                               ; false))
      val _ = if !seen then () else Error.bug "Vector.removeFirst"
   in
      v
   end

fun partitioni (v, f) =
   let
     val n = ref 0
     val v' = mapi (v, fn (i, x) =>
                    let
                      val b = f (i, x)
                      val _ = if b then n := 1 + !n else ()
                    in
                      (x,b)
                    end)
     val n = !n
     val r = ref 0
     fun loop b (i:int) =
       case unsafeSub (v', i) of
         (x, b') => if b = b' 
                      then (r := i + 1; x)
                      else loop b (i + 1)
     val yes = tabulate (n, fn _ => loop true (!r))
     val _ = r := 0
     val no = tabulate (length v - n, fn _ => loop false (!r))
   in
     {yes = yes, no = no}
   end

fun partition (v, f) = partitioni (v, f o #2)

fun prefix (v, n) = tabulate (n, fn i => sub (v, i))

fun removeDuplicates (v, equals) =
   keepAllMapi (v, fn (i, x) =>
                if i > 0 andalso equals (x, sub (v, i - 1))
                   then NONE
                else SOME x)

fun randomElement v = sub (v, Random.natLessThan (length v))

end
