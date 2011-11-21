structure IntSeq = struct

fun failwith s = raise Fail s
fun subscript () = raise Fail "subscript"

structure A = IntArray

type seq = IntArray.array

fun empty () = A.array (0, 0)
val length = A.length
fun isEmpty s = length s = 0
val sub = A.sub
fun toList v = List.tabulate (length v, fn i => sub (i, v))
fun singleton x = A.array (1, x)

val tabulate = A.tabulate

fun appi f s = let
  fun lp i =
    if i < length s then (
      f (i, sub (s, i));
      lp (i + 1))
    else
      ()
  in
    lp 0
  end

fun app f s = let
  fun f' (_, x) = f x
  in
    appi f' s
  end

fun fromList xs = let
  val a = A.array (List.length xs, 0)
  fun lp (xs, i) = (case xs
   of nil => 
        ()
    | x :: xs => (
        A.update (a, i, x);
	lp (xs, i + 1)))
  in
    lp (xs, 0);
    a
  end

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
fun catN ss = let
  val len = List.foldl (fn (x, y) => x + y) 0 (List.map length ss)
  val res = A.array (len, 0)
  fun lp (ss, i) = (case ss
    of nil => 
         ()
     | s :: ss => (
         appi (fn (j, x) => A.update (res, i + j, x)) s;
	 lp (ss, i + length s)))
  in
    lp (ss, 0);
    res
  end

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


fun filter f s = let
  val len = length s
  fun lp (i, acc) = 
    if i < 0 then
      fromList acc
    else let
      val x = sub (s, i)
      in
	if f x then lp (i-1, x::acc)
	else lp (i-1, acc) 
      end
  in
    lp (len-1, nil)
  end

fun fromListRev xs = fromList (List.rev xs)

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


end
