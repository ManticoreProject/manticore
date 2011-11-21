fun copies n = fn x => List.tabulate (n, fn _ => x)

(* prototype *)

val maxLeafSize = 10

datatype 'a rope
  = Leaf of 'a vector
  | Cat of 'a rope * 'a rope

fun mkLeaf xs = let
  val n = List.length xs
  in
    if n > maxLeafSize then raise Fail "length" else Leaf (Vector.fromList xs)
  end

fun ropeFromLeaves ls = let
  fun lp [] = []
    | lp [r] = [r]
    | lp (r1::r2::t) = Cat(r1,r2)::lp(t)
  fun lp' rs = (case lp rs
    of [] => Leaf (Vector.fromList [])
     | [r] => r
     | rs' => lp' rs')
  in
    lp' ls
  end

fun takeDrop n xs = let
  fun lp ([], _, h) = (List.rev h, [])
    | lp (t as x::xs, i, h) =
        if i=0 then
          (List.rev h, t)
	else if i>0 then
	  lp (xs, i-1, x::h)
        else
          raise Fail "negative?"
  in
    lp (xs, n, [])
  end

fun chop sz xs = let
  fun lp ([], acc) = List.rev acc
    | lp (xs, acc) = let
        val (h, t) = takeDrop sz xs
        in
          lp (t, h::acc)
        end
  in
    lp (xs, [])
  end

fun ropeFromList xs = let
  val ss = chop maxLeafSize xs
  in
    ropeFromLeaves (map mkLeaf ss)
  end

datatype shape
  = Lf of int * int
  | Nd of shape list

datatype 'a farray 
  = F of 'a rope * shape

fun ropeLength r = (case r
  of Leaf ns => Vector.length ns
   | Cat (rL, rR) => ropeLength rL + ropeLength rR
  (* end case *))

fun locations s = (case s
  of Lf _ => raise Fail "Lf"
   | Nd ss => let
       fun lp (ss, acc) = (case ss
         of [] => List.concat (List.rev acc)
	  | s::ss => (case s
              of Lf (lo, hi) => let
                   val n = hi-lo
                   val ns = copies n lo
                   in
		     lp (ss, ns::acc)
		   end
	       | Nd _ => raise Fail "Nd"
              (* end case *))
         (* end case *))
       val list = lp (ss, [])
       in
         Vector.fromList list
       end
  (* end case *))

fun addInto ps (n, i) = let
  fun ai [] = [(i,n)]
    | ai ((i',n')::t) = 
        if i'=i then (i',n+n')::t else (i',n')::ai(t)
  in
    ai ps
  end

fun sums (locs, lo) = (fn ns => let
  val len = Vector.length ns
  fun go (i, ns, acc) =
    if i-lo>=len then acc
    else let 
      val n = Vector.sub (ns, i-lo)
      val acc' = addInto acc (n, Vector.sub (locs, i))
      in
        go (i+1, ns, acc')
      end
  in
    go (lo, ns, [])
  end)

fun cleave xs = let
  fun lp (acc, [x]) = (List.rev acc, x)
    | lp (acc, x::xs) = lp (x::acc, xs)
    | lp (acc, _) = raise Fail "empty"
  in
    lp ([], xs)
  end

infix ++ 
fun ps ++ qs = (case (ps, qs)
  of ([], _) => qs
   | (_, []) => ps
   | (_, _) => let
       val (butlast, (i,m)) = cleave ps
       val ((j,n),t) = (case qs
         of x::xs => (x,xs)
	  | _ => raise Fail "?")
       in
         if i=j then
           butlast @ (i,m+n)::t
         else
           ps @ qs
       end
  (* end case *))

fun segsum (nss : int farray) = let
  val F (data, shape) = nss
  val locs = locations shape
  fun lp (r, lo) = (case r
    of Leaf ns => sums (locs, lo) ns
     | Cat (rL, rR) => let
         val (l, r) = (lp (rL, lo), lp (rR, lo + ropeLength rL))
         in
           l ++ r
         end
    (* end case *))
  val data' = ropeFromList (List.map #2 (lp (data, 0)))
  val shape' = Lf (0, ropeLength data')
  in
    F (data', shape')
  end

fun test sz = let
  val data = ropeFromList (copies (sz*sz) 1)
  val leaves = List.tabulate (sz, fn i => Lf (i*sz, i*sz+sz))
  val shape = Nd leaves
  val x = F (data, shape)
  in
    (x, segsum x)
  end
