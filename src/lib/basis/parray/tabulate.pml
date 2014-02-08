structure Tabulate = struct

val C = 2

fun failwith s = (print ("Fail with " ^ s ^ "\n"); raise Fail s)
fun subscript () = raise Fail "subscript"

structure RT = Runtime
structure Seq = Seq
structure RTy = RopeTy

datatype progress = datatype Progress.progress

type 'a seq = 'a Seq.seq
datatype rope = datatype RTy.rope

fun length rp = (case rp
  of Leaf s => Seq.length s
   | Cat (l, _, _, _) => l)
   
fun depth rp = (case rp
  of Leaf _ => 0
   | Cat (_, d, _, _) => d)
   
fun empty () = Leaf (Seq.empty ())
fun isEmpty rp = length rp = 0
fun singleton x = Leaf (Seq.singleton x)
fun inBounds (r, i) = (i < length r) andalso (i >= 0)

fun leaf s =
  if Seq.length s > LeafSize.getMax () then
    failwith "bogus leaf size"
  else 
    Leaf s

fun leaves rp = (case rp
  of Leaf s => s::nil
   | Cat (_, _, rp1, rp2) => leaves rp1 @ leaves rp2)

fun toSeq rp = Seq.catN (leaves rp)

fun subInBounds (rp, i) = (case rp
  of Leaf s => Seq.sub (s, i)
   | Cat (_, _, r1, r2) =>
     if i < length r1 then 
       subInBounds (r1, i)
     else 
       subInBounds (r2, i - length r1))
fun sub (rp, i) =
  if inBounds (rp, i) then
    subInBounds (rp, i)
  else
    subscript ()


fun printRope rp i = if i = length rp
                     then ()
                     else (print (Int.toString (sub(rp, i)) ^ ", "); printRope rp (i+1))
    

(* non-coalescing rope concatenation *)
fun nccat2 (rp1, rp2) = let
  val l = length rp1 + length rp2
  val d = Int.max (depth rp1, depth rp2) + 1
  in
    Cat (l, d, rp1, rp2)
  end

(* coalescing rope concatenation *)
fun ccat2 (rp1, rp2) =
  if length rp1 + length rp2 <= LeafSize.getMax () then
    Leaf (toSeq (nccat2 (rp1, rp2)))
  else
    nccat2 (rp1, rp2)

(*** Cursor navigation ***)

datatype ('a, 'b) gen_ctx
  = GCTop
  | GCLeft of ('a, 'b) gen_ctx * 'a
  | GCRight of 'b * ('a, 'b) gen_ctx

type ('a, 'b) gen_cur = 'a * ('a, 'b) gen_ctx

fun ctxLength lengthL lengthR (c : ('a,'b) gen_ctx) = let
  fun len c = (case c
    of GCTop => 
	 (0, 0)
     | GCLeft (c, r) =>
	 let
	     val (np, nu) = len c
	 in
	     (np, nu + lengthR r)
	 end
     | GCRight (l, c) =>
	 let 
	     val (np, nu) = len c
	 in
	     (np + lengthL l, nu)
	 end)
  in
    len c
  end


fun leftmostLeaf (rp, c) = (case rp
  of Leaf x => (x, c)
   | Cat (_, _, l, r) => leftmostLeaf (l, GCLeft (c, r)))

fun finish j cur = let
  fun u (f, c) = (case c
    of GCTop => f
     | GCLeft (c, r) => u (j (f, r), c)
     | GCRight (l, c) => u (j (l, f), c))
  in
    u cur
  end

fun start rp = let
  val (s, c) = leftmostLeaf (rp, GCTop)
  in
    (leaf s, c)
  end

fun next leftmost jnL cur = let
  fun n (f, c) = (case c
    of GCTop => 
	 Done f
     | GCLeft (c', r) =>
	 More (leftmost (r, GCRight (f, c')))
     | GCRight (l, c') =>
	 n (jnL (l, f), c'))
  in
    n cur
  end

datatype dir = Left | Right
type ('a, 'b) unzipped_gen_ctx = 
   'b list * 'a list * dir list
type ('a, 'b) unzipped_gen_cur =
   'a * ('a, 'b) unzipped_gen_ctx

fun unzipCtx c = (case c
  of GCTop =>
       (nil, nil, nil)
   | GCLeft (c, r) => let
      val (ls, rs, ds) = unzipCtx c
      in
	(ls, r :: rs, Left :: ds)
      end
   | GCRight (l, c) => let
      val (ls, rs, ds) = unzipCtx c
      in
	(l :: ls, rs, Right :: ds)
      end)

fun unzipCursor (rp, c) = (rp, unzipCtx c)

fun zipCtx (ls, rs, ds) = (case (ls, rs, ds)
  of (nil, nil, nil) =>
       GCTop
   | (ls, r :: rs, Left :: ds) => 
       GCLeft (zipCtx (ls, rs, ds), r)
   | (l :: ls, rs, Right :: ds) =>
       GCRight (l, zipCtx (ls, rs, ds))
   | _ => failwith "zipCtx")

fun zipCursor (rp, c) = (rp, zipCtx c)


(*Returns a 4-tuple, where the first element contains the first half of the ropes, second is the middle
**interval, third is the index into the middle interval that the split should occur, and last is the 
**intervals belonging to the second half*)
fun divide length (intvs, k) = let
  fun d (intvs, k) = (case intvs
    of intv :: intvs =>
	 if k <= length intv then
	   (nil, intv, k, intvs)
	 else let
	   val (intvs1, m, k', intvs2) = d (intvs, k - length intv)
	   in
	     (intv :: intvs1, m, k', intvs2)
	   end
     | _ => failwith "divide")
  in
    d (intvs, k)
  end

type 'b rebuilder = 
  ('b rope list * dir list * dir list * int * int * int * int)

fun encodeRope rps = let
  fun e rs = (case rs
    of rp::nil => 
         rp
     | rp :: rps =>
         nccat2 (rp, e rps)
     | _ => failwith "encodeRope")
  in
    (e rps, List.length rps)
  end

fun decodeRope (rp, n) = 
  if n = 1 then
    rp::nil
  else (case rp
    of Cat (_, _, rp1, rp2) =>
       rp1 :: decodeRope (rp2, n - 1)
     | _ => failwith "decodeRope")

(*mkU is the identity function, return More(us, GCRight(Leaf ps, c)) if ps is non empty, otherwise
**return More(us, c) if it is empty*)
fun more length mkU mkP (us : int * int, ps : 'a Seq.seq, c : ('a, 'b) gen_ctx) = let
  val c' : ('a, 'b) gen_ctx = if length ps = 0 then c else GCRight (mkP ps, c)
  in
    More (mkU us, c')
  end


(* The following implementation of tabulate uses index ranges of the
    form (lo, hi) where
    - lo denotes the first index of the range
    - hi denotes the index hi' + 1 where hi' is the largest index of the
      range
*)

fun intervalLength (lo, hi) = hi - lo

fun tabulateSequence f (lo, hi) = 
  Seq.tabulate (intervalLength (lo, hi), fn i => f (lo + i))

(* pre: intervalLength (lo, hi) > 1 *)
fun splitInterval2 (lo, hi) = let 
  val m = (lo + hi) div 2
  in
    ((lo, m), (m, hi))
  end

fun tabulateSequential f intv = let
  fun t intv = let
    val len = intervalLength intv
    in
      if len <= LeafSize.getMax () orelse len < 2 then
	leaf (tabulateSequence f intv)
      else let
        val (int1, int2) = splitInterval2 intv
        in
	  nccat2 (t int1, t int2)
        end
    end
  in
    t intv
  end


fun leftmostTab (intv, c) =
  if intervalLength intv <= LeafSize.getMax () then
    (intv, c)
  else let
    val (intv1, intv2) = splitInterval2 intv
    in
      leftmostTab (intv1, GCLeft (c, intv2))
    end
fun nextTab cur = next leftmostTab nccat2 cur

fun encodeCur intvs = let
  fun e intvs = (case intvs
    of intv::nil =>
         (intv, GCTop)
     | intv :: intvs => let
         val (intv', c) = e intvs
         in
	   (intv, GCLeft (c, intv'))
	 end
     | _ => failwith "encodeCur")
  in
    (e intvs, List.length intvs)
  end


fun decodeRopeTab (rp, n) = let
  fun d (rp, n) =
    if n = 1 then
      rp::nil
    else (case rp
      of Cat (_, _, rp1, rp2) =>
           rp2 :: d (rp1, n - 1)
       | _ => (print "decodeRopeTab fail\n"; failwith "decodeRope"))
  in
    List.rev (d (rp, n))
  end
  
fun cursorLength lengthL lengthR (f : int * int, c : ('a, 'b) gen_ctx) = let
  val (np : int, nu : int) = ctxLength lengthL lengthR c
  in
    (np, nu + lengthR f)
  end


fun numUnprocessed lengthL lengthR cur = let
  val (_, nu) = cursorLength lengthL lengthR cur
  in
    nu
  end

fun numUnprocessedTab cur = numUnprocessed length intervalLength cur

fun tabulateUntil cond (cur, f) = let
  fun t (intv : int * int, c : ('a, 'b) gen_ctx) = 
    (case Seq.tabulateUntil cond (intv, f)
      of More (ps : 'a Seq.seq) => let
 	   val (lo : int, hi : int) = intv
           val us : int * int = (lo + Seq.length ps, hi)
	   in
             if numUnprocessedTab (us, c) < 2 then let
	       val us' = 
                 (case Seq.tabulateUntil (fn _ => false) (us, f)
		    of Done x => x
		     | _ => failwith "expected Done")
	       val ps' = Seq.cat2 (ps, us')
	       in
		 case nextTab (leaf ps', c)
		  of Done p' => Done p'
		   | More (s', c') => t (s', c')
	       end
	     else
	       more Seq.length (fn x => x) leaf (us, ps, c)
	   end
       | Done ps => (case nextTab (leaf ps, c)
           of Done p' => Done p'
	    | More (intv', c') => t (intv', c')))
  val (intv : int * int, c : ('a, 'b) gen_ctx) = leftmostTab cur
  in
    t (intv, c)
  end

(* pre: 0 <= i < cursorLength (intv, c) *)
fun moveToIx ((intv, (ls, rs, ds)), i) = let
  val len = intervalLength intv
  in
    if len = 1 then
      (intv, (ls, rs, ds))
    else let
      val (intv1, intv2) = splitInterval2 intv
      in
	if i < intervalLength intv1 then
	  moveToIx ((intv1, (ls, intv2 :: rs, Left :: ds)), i)
	else
	  moveToIx ((intv2, (intv1 :: ls, rs, Right :: ds)), 
		       i - intervalLength intv1)
      end
  end
(*
fun rootU (rp : 'a rope, uc) = (case uc
  of (nil, nil, nil) => rp
   | (ls, r :: rs, Left :: ds) => rootU (nccat2 (rp, r), (ls, rs, ds))
   | (l :: ls, rs, Right :: ds) => rootU (nccat2 (l, rp), (ls, rs, ds))
   | (l::ls, nil, nil) => failwith "l::ls, nil, nil"
   | _ => failwith "rootU")
*)

fun check2 uc = case uc 
    of (nil, nil, nil) => print "nil, nil, nil\n"
     | (ls, r :: rs, Left :: ds) => (print "Left\n"; check2 (ls, rs, ds))
     | (l :: ls, rs, Right :: ds) => (print "Right\n"; check2(ls, rs, ds))
     | (l::ls, nil, nil) => print "l::ls, nil, nil)\n"
     | _ => failwith "rootU\n"

(*
fun check uc = 
    let val (ls, rs, ds) = uc
    in case ds
        of Left :: ds => (case rs
                            of r::rs => (print "Left\n"; check (ls, rs, ds))
                             | nil => print "rs is nil\n")
         | Right :: ds => (case ls 
                            of l::ls => (print "Right\n"; check(ls, rs, ds))
                             | nil => print "ls is nil\n")
         | nil => case ls
                   of l::ls => (case rs
                        of r::rs => print "ds is nil, but ls and rs are not\n"
                         | nil => print "ds is nil, but ls is not\n"
                         )
                   |nil => print "nil, nil, nil\n"
                   
    end
*)    

fun fib n = if n <= 2
            then 1
            else fib(n-1) + fib(n-2)

fun rootU(rp, uc') =
    let fun helper(rp, uc) = (fib 5; case uc
          of (nil, nil, nil) => (rp)
           | (ls, r :: rs, Left :: ds) => ( helper (nccat2 (rp, r), (ls, rs, ds)))
           | (l :: ls, rs, Right :: ds) =>(helper (nccat2 (l, rp), (ls, rs, ds)))
           | (l::ls, nil, nil) => (print "about to raise exception\n";  failwith "l::ls, nil, nil")
           | _ => failwith "rootU")
    in helper(rp, uc')
    end



fun cursorAtIxIntv (intv, i) = 
  if 0 <= i andalso i < intervalLength intv then
    moveToIx ((intv, (nil, nil, nil)), i)
  else
    subscript ()

fun join decode finish zipCursor (rp1, rp2, (ls, ds, mds, n1, n2, l1, l2)) = let
  val (xs1, xs2) = (decode (rp1, l1), decode (rp2, l2)) 
  val (rps1, ms) = (List.take (xs1, n1), List.drop (xs1, n1)) 
  val (mn, mls) = (List.last ms, List.rev (List.take (ms, List.length ms - 1)))
  val (mrs, rps2) = (List.take (xs2, n2), List.drop (xs2, n2)) 
  val m = finish (zipCursor (mn, (mls, mrs, mds))) 
  val ropes = rps1 @ (m::nil) @ rps2 
  in
    case ropes
     of rp::rs => (zipCursor (rp, (ls, rs, ds)) handle e => (check2(ls, rs, ds); raise e))
      | _ => failwith "join"
  end
  
fun splitAt length encode cursorAtIx unzipCursorL unzipCursorR cur n = let
  val (rp, (ls, rs, ds)) = unzipCursorL cur
  val (rps1, m, k, rps2) = divide length (rp :: rs, n)
  val (mn, (mls, mrs, mds)) = cursorAtIx (m, k - 1)
  val (n1, n2) = (List.length rps1, List.length mrs)
  val (xs1, xs2) = (rps1 @ List.rev mls @ (mn::nil), mrs @ rps2)
  val ((rp1, l1), (rp2, l2)) = (encode xs1, encode xs2)
  in
    (rp1, rp2, (ls, ds, mds, n1, n2, l1, l2))
  end

fun tabulateLTS PPT (intv : int * int, f : int -> 'a) = let
  fun t cur = (case tabulateUntil (*RT.hungryProcs*) (fn _ => true) (cur, f)
    of Done rp => rp
     | More (cur' : (('a, 'b) gen_cur)) => let
	 val mid = numUnprocessedTab cur' div 2
	 fun id x = x
	 val (cur1 : ('a, 'b) gen_cur, cur2 : ('a, 'b) gen_cur, reb) = 
	       splitAt intervalLength encodeCur cursorAtIxIntv id id (unzipCursor cur') mid
	 val rp1 = t cur1
	 val rp2 = t cur2
	 in
	   join decodeRopeTab id rootU (rp1, rp2, reb)
         end)
  in
    t (intv, GCTop)
  end


    
end

