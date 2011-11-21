structure VectorSeq : SEQ = struct

fun failwith s = raise Fail s
fun subscript () = raise Subscript

type 'a seq = 'a Vector.vector

val fromList = Vector.fromList
fun fromListRev xs = fromList (List.rev xs)

fun empty () = fromList nil
val length = Vector.length
fun isEmpty s = length s = 0
fun toList v = Vector.foldr (fn (x, ls) => x :: ls) nil v
fun singleton x = fromList (x::nil)

val tabulate = Vector.tabulate

val sub = Vector.sub

fun cat2 (x, y) = let
  val xn = length x
  val yn = length y
  fun elt i =
    if i < xn then
      sub (x, i)
    else
      sub (y, i-xn)
  in
    tabulate (xn+yn, elt)      
  end
fun catN ss = fromList (List.concat (List.map toList ss))

fun take (s, n) = let
  val len = length s
  in
    if n >= len then s
    else tabulate (n, fn i => sub (s, i))
  end
fun drop (s, n) = let
  val len = length s
  in
    if n >= len then (empty())
    else tabulate (len-n, fn i => sub (s, i+n))
  end

fun split2 s = let
  val l = length s
  in (take (s, l div 2), drop (s, l div 2))
  end
fun splitN ss = failwith "todo"

fun insert (s, i, x) = let
  val s1 = take (s, i + 1)
  val s2 = drop (s, i + 1)
  in
    cat2 (s1, cat2 (singleton x, s2))
  end

datatype progress = datatype Progress.progress

fun tabulateUntil cond ((lo, hi), f) = let
  fun t (lo, xs) = 
    if lo >= hi then
      Done (fromListRev xs)
    else if cond () then
      More (fromListRev xs)
    else
      t (lo + 1, f lo :: xs)
  in
    t (lo, nil)
  end

val map = Vector.map

fun mapUntil cond f s = let
  val len = length s
  fun lp (i, acc) = 
    if i < len then
      if cond () then
	More (drop (s, i), fromListRev acc)
      else
	lp (i+1, f (sub (s, i))::acc)
    else
      Done (fromListRev acc)
  in
    lp (0, nil)
  end

val reduce = Vector.foldl

fun reduceUntil cond f b s = let
  fun lp (i, acc) =
    if i < length s then
      if cond () then
	More (acc, drop (s, i))
      else
	lp (i+1, f (acc, sub (s, i)))
    else
      Done acc
  in
    lp (0, b)
  end

fun scan f b s = let
  fun f' (x, (s, ss)) = (f (x, s), s :: ss)
  val (_, ss) = Vector.foldl f' (b, nil) s
  in
    fromListRev ss
  end

fun scanUntil cond f b s = let
  fun lp (i, (acc, xs)) =
    if i < length s then
      if cond () then
        (acc, More(drop (s, i), fromListRev xs))
      else
        lp (i+1, (f (sub (s, i), acc), acc :: xs))
    else
      (acc, Done (fromListRev xs))
  in
    lp (0, (b, nil))
  end

fun filter pred s =
  if isEmpty s then (empty())
  else let
    val len = length s
    fun lp (i, acc) = 
      if i < 0 then
	fromList acc
      else let
	val x = sub (s, i)
	in
	  if pred x then lp (i-1, x::acc)
	  else lp (i-1, acc) 
	end
    in
      lp (len-1, nil)
    end

fun filterUntil cond f s = let
  fun lp (i, acc) =
    if i < length s then
      if cond () then
        More (drop (s, i), fromListRev acc)
      else
        lp (i+1, if f (sub (s, i)) then sub (s, i) :: acc else acc)
    else
      Done (fromListRev acc)
  in
    lp (0, nil)
  end

val app = Vector.app

end
