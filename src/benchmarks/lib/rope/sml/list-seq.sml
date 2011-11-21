structure ListSeq : SEQ = struct

fun failwith s = raise Fail s
fun subscript () = raise Subscript

type 'a seq = 'a list

fun empty () = nil
val isEmpty = List.null
val length = List.length
fun fromList s = s
fun toList s = s
fun singleton x = [x]

fun cat2 (s1, s2) = s1 @ s2
val catN = List.concat
fun split2 s = let
  val l = length s
  in (List.take (s, l div 2), List.drop (s, l div 2))
  end
fun splitN (s, n) = failwith "todo"

val take = List.take
val drop = List.drop
fun insert (s, i, x) = let
  val s1 = List.take (s, i + 1)
  val s2 = List.drop (s, i + 1)
  in
    s1 @ [x] @ s2
  end
val sub = List.nth

datatype progress = datatype Progress.progress

val tabulate = List.tabulate

fun tabulateUntil cond ((lo, hi), f) = let
  fun t (lo, xs) = 
    if lo >= hi then
      Done (List.rev xs)
    else if cond () then
      More (List.rev xs)
    else
      t (lo + 1, f lo :: xs)
  in
    t (lo, nil)
  end

val map = List.map

fun mapUntil cond f s = let
  fun lp (s, acc) = 
    (case s
      of nil => Done (List.rev acc)
       | y::ys => if cond ()
                  then More(s, List.rev acc)
                  else lp (ys, (f y)::acc))
  in
    lp (s, nil)
  end

val reduce = List.foldl

fun reduceUntil cond f b s = let
  fun lp (s, acc) =
    (case s
      of nil => Done acc
       | y::ys => if cond () 
                  then More(acc, s)
                  else lp (ys, f (acc, y)))
  in
    lp (s, b)
  end

fun scan f b s = let
  fun f' (x, (s, ss)) = (f (x, s), s :: ss)
  val (_, ss) = List.foldl f' (b, nil) s
  in
    List.rev ss
  end

fun scanUntil cond f b s = let
  fun lp (s, (x, xs)) = 
    (case s
      of nil => (x, Done (List.rev xs))
       | y::ys => if cond ()
		  then (x, More(s, List.rev xs))
		  else lp (ys, (f (y, x), x :: xs)))
  in
    lp (s, (b, nil))
  end

val filter = List.filter

fun filterUntil cond f s = let
  fun lp (s, acc) =
    (case s
      of nil => Done (List.rev acc)
       | y::ys => if cond ()
                  then More(s, List.rev acc)
                  else lp (ys, if f y then y::acc else acc))
  in
    lp (s, nil)
  end

val app = List.app

end
