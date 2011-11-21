functor RopeImplFn (
  structure Seq : SEQ
  structure RT  : RUNTIME
  val C : real
) (* : ROPE *) = struct

fun failwith s = raise Fail s
fun subscript () = raise Fail "subscript"

structure Seq = Seq
structure RTy = RopeTyFn (structure Seq = Seq)

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
fun ropeOK rp = let
  fun length' rp = (case rp
    of Leaf s => Seq.length s
     | Cat (_, _, rp1, rp2) => length' rp1 + length' rp2)
  fun depth' rp = (case rp
    of Leaf _ => 0
     | Cat (_, _, rp1, rp2) => Int.max (depth' rp1, depth' rp2) + 1)
  fun check rp = (case rp
    of Leaf s => true
     | Cat (_, _, rp1, rp2) => 
         length rp = length' rp andalso depth rp = depth' rp andalso
	 check rp1 andalso check rp2)
  in
    check rp
  end

fun leaf s =
  if Seq.length s > LeafSize.getMax () then
    failwith "bogus leaf size"
  else 
    Leaf s

fun toList rp = (case rp
  of Leaf s => Seq.toList s
   | Cat(_, _, l, r) => toList l @ toList r)

fun leaves rp = (case rp
  of Leaf s => s::nil
   | Cat (_, _, rp1, rp2) => leaves rp1 @ leaves rp2)

fun toSeq rp = Seq.catN (leaves rp)

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
    leaf (toSeq (nccat2 (rp1, rp2)))
  else
    nccat2 (rp1, rp2)

fun split2 rp =
  (case rp 
    of Leaf s => let val (s1, s2) = Seq.split2 s
                 in (leaf s1, leaf s2)
                 end
     | Cat (_, _, l, r) => (l, r))

fun splitN _ = failwith "todo"

fun fromList xs = let
  val l = List.length xs
  in
    if l < LeafSize.getMax () orelse l = 1 then leaf (Seq.fromList xs)
    else nccat2 (fromList (List.take (xs, l div 2)),
                 fromList (List.drop (xs, l div 2)))
  end

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

fun seqSplitAtIx2 (s, i) = 
  (Seq.take (s, i + 1), Seq.drop (s, i + 1))

fun splitAtIx2' (rp, i) = (case rp
  of Leaf s => let
       val (s1, s2) = seqSplitAtIx2 (s, i)
       in
         (leaf s1, leaf s2)
       end
   | Cat (_, _, l, r) =>
       if i = length l - 1 then
	 (l, r)
       else if i < length l then let
         val (l1, l2) = splitAtIx2' (l, i)
	 in
	   (l1, nccat2 (l2, r))
	 end
       else let
         val (r1, r2) = splitAtIx2' (r, i - length l)
         in
	   (nccat2 (l, r1), r2)
         end)
fun splitAtIx2 (rp, i) =
  if inBounds (rp, i) then
    splitAtIx2' (rp, i)
  else
    subscript ()

fun seqLast s = Seq.sub (s, Seq.length s - 1)

fun log base x = Math.ln x / Math.ln base
val log2 = log 2.0
fun isBalanced rp =
  (case rp
    of Leaf _ => true
     | _ => real (depth rp) <= C * log2 (real (length rp)))

local
fun balanceSequential rp =
  if isBalanced rp then
    rp
  else if length rp <= LeafSize.getMax () orelse length rp < 2 then
    leaf (toSeq rp)
  else let
    val (rp1, rp2) = splitAtIx2 (rp, length rp div 2 - 1)
    in
      nccat2 (balanceSequential rp1, balanceSequential rp2)
    end

fun balanceETS SST rp =
  if isBalanced rp then
    rp
  else if length rp <= LeafSize.getMax () orelse length rp < 2 then
    leaf (toSeq rp)
  else if length rp <= SST then 
    balanceSequential rp
  else let
    val (rp1, rp2) = splitAtIx2 (rp, length rp div 2 - 1)
    in
      nccat2 (RT.par2 (fn () => balanceETS SST rp1, 
		       fn () => balanceETS SST rp2))
    end
in
fun balance rp = (case ChunkingPolicy.get ()
  of ChunkingPolicy.Sequential => 
       balanceSequential rp
   | ChunkingPolicy.ETS SST => 
       balanceETS SST rp
   | ChunkingPolicy.LTS PPT => 
     (* balanceLTS PPT rp *)
     (* TODO: fix me *)
     balanceETS 10000 rp)
end

fun cat2 (rp1, rp2) = balance (nccat2 (rp1, rp2))

fun catN rps = balance (List.foldr nccat2 (empty ()) rps)

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

fun cursorLength lengthL lengthR (f, c) = let
  val (np, nu) = ctxLength lengthL lengthR c
  in
    (np, nu + lengthR f)
  end

fun numUnprocessed lengthL lengthR cur = let
  val (_, nu) = cursorLength lengthL lengthR cur
  in
    nu
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

fun splitCtx (jL, bL) (jR, bR) c = let
  fun s c = (case c
    of GCTop => 
	 (bL, bR)
     | GCLeft (c, r) => let
	 val (p, u) = s c
	 in
	   (p, jR (r, u))
	 end
     | GCRight (l, c) => let
	 val (p, u) = s c
	 in
	   (jL (p, l), u)
	 end)
  in
    s c
  end

fun splitCursor (jL, bL) (jR, bR) (f, c) = let
  val (p, u) = splitCtx (jL, bL) (jR, bR) c
  in
    (p, jR (f, u))
  end

datatype dir = Left | Right
type ('a, 'b) unzipped_gen_ctx = 
   'b list * 'a list * dir list
type ('a, 'b) unzipped_gen_cur =
   'a * ('a, 'b) unzipped_gen_ctx

fun unzippedCtxOK (ls, rs, ds) =
  List.length rs = List.length (List.filter (fn x => x = Left) ds) andalso
  List.length ls = List.length (List.filter (fn x => x = Right) ds) 

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

(* split the sequence into three parts: sequence before ith element; *)
(* ith element; sequence after ith element *)
fun seqSplitAtIx3 (s, i) = let
  val (ls, rs) = seqSplitAtIx2 (s, i)
  in
    (Seq.take (ls, Seq.length ls - 1), seqLast ls, rs)
  end

fun cursorAtIx (rp, i) = let
  fun nav ((rp, (ls, rs, ds)), i) = (case rp
    of Leaf s =>
         if Seq.length s = 1 then
	   (leaf s, (ls, rs, ds))
	 else let
	   val (l, m, r) = seqSplitAtIx3 (s, i)
	   val c' = (leaf l :: ls, leaf r :: rs, Right :: Left :: ds)
	   in
	     (leaf (Seq.singleton m), c')
	   end
    | Cat (_, _, l, r) =>
	if i < length l then
	  nav ((l, (ls, r :: rs, Left :: ds)), i)
	else
	  nav ((r, (l :: ls, rs, Right :: ds)), i - length l))
  in
    if inBounds (rp, i) then
      nav ((rp, (nil, nil, nil)), i)
    else
      subscript ()
  end

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

fun splitAt length encode cursorAtIx unzipCursorL unzipCursorR cur n = let
  val (rp, (ls, rs, ds)) = unzipCursorL cur
  val (rps1, m, k, rps2) = divide length (rp :: rs, n)
  val (mn, (mls, mrs, mds)) = cursorAtIx (m, k - 1)
  val (n1, n2) = (List.length rps1, List.length mrs)
  val (xs1, xs2) = (rps1 @ mls @ (mn::nil), mrs @ rps2)
  val ((rp1, l1), (rp2, l2)) = (encode xs1, encode xs2)
  in
    (rp1, rp2, (ls, ds, mds, n1, n2, l1, l2))
  end

fun join decode finish zipCursor (rp1, rp2, (ls, ds, mds, n1, n2, l1, l2)) = let
  val (xs1, xs2) = (decode (rp1, l1), decode (rp2, l2))
  val (rps1, ms) = (List.take (xs1, n1), List.drop (xs1, n1))
  val (mn, mls) = (List.last ms, List.take (ms, List.length ms - 1))
  val (mrs, rps2) = (List.take (xs2, n2), List.drop (xs2, n2))
  val m = finish (zipCursor (mn, (mls, mrs, mds)))
  val rp :: rs = rps1 @ (m::nil) @ rps2
  in
    zipCursor (rp, (ls, rs, ds))
  end

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

fun more length mkU mkP (us, ps, c) = let
  val c' = if length ps = 0 then c else GCRight (mkP ps, c)
  in
    More (mkU us, c')
  end

local

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

fun tabulateETS SST (n, f) = let
  fun t intv = let
    val len = intervalLength intv 
    in
      if len <= SST orelse len < 2 then
	tabulateSequential f intv
      else let
	val (intv1, intv2) = splitInterval2 intv
	in
	  nccat2 (RT.par2 (fn () => t intv1, fn () => t intv2))
	end
    end
  in
    t (0, n)
  end

fun numUnprocessedTab cur = numUnprocessed length intervalLength cur
fun leftmostTab (intv, c) =
  if intervalLength intv <= LeafSize.getMax () then
    (intv, c)
  else let
    val (intv1, intv2) = splitInterval2 intv
    in
      leftmostTab (intv1, GCLeft (c, intv2))
    end
fun nextTab cur = next leftmostTab nccat2 cur

fun tabulateUntil cond (cur, f) = let
  fun t (intv, c) = 
    (case Seq.tabulateUntil cond (intv, f)
      of More ps => let
 	   val (lo, hi) = intv
           val us = (lo + Seq.length ps, hi)
	   in
             if numUnprocessedTab (us, c) < 2 then let
	       val Done us' = Seq.tabulateUntil (fn _ => false) (us, f)
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
  val (intv, c) = leftmostTab cur
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

fun cursorAtIx (intv, i) = 
  if 0 <= i andalso i < intervalLength intv then
    moveToIx ((intv, (nil, nil, nil)), i)
  else
    subscript ()

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

fun decodeRope (rp, n) = let
  fun d (rp, n) =
    if n = 1 then
      rp::nil
    else (case rp
      of Cat (_, _, rp1, rp2) =>
           rp2 :: d (rp1, n - 1)
       | _ => failwith "decodeRope")
  in
    List.rev (d (rp, n))
  end

fun rootU (rp, uc) = (case uc
  of (nil, nil, nil) => rp
   | (ls, r :: rs, Left :: ds) => rootU (nccat2 (rp, r), (ls, rs, ds))
   | (l :: ls, rs, Right :: ds) => rootU (nccat2 (l, rp), (ls, rs, ds))
   | _ => failwith "rootU")

fun tabulateLTS PPT (n, f) = let
  fun t cur = (case tabulateUntil RT.hungryProcs (cur, f)
    of Done rp => rp
     | More cur' => let
	 val mid = numUnprocessedTab cur' div 2
	 fun id x = x
	 val (cur1, cur2, reb) = 
	       splitAt intervalLength encodeCur cursorAtIx id id (unzipCursor cur') mid
	 val (rp1, rp2) = RT.par2 (fn () => t cur1, fn () => t cur2)
	 in
	   join decodeRope id rootU (rp1, rp2, reb)
         end)
  in
    t ((0, n), GCTop)
  end
in
fun tabulate (n, f) = 
  if n < 0 then raise Size else
  (case ChunkingPolicy.get ()
    of ChunkingPolicy.Sequential => 
         tabulateSequential f (0, n)
     | ChunkingPolicy.ETS SST => 
         tabulateETS SST (n, f)
     | ChunkingPolicy.LTS PPT => 
         tabulateLTS PPT (n, f))
end (* local *)

local
fun mapSequential f rp = (case rp
  of Leaf s => 
       leaf (Seq.map f s)
   | Cat(len, d, l, r) => 
       Cat (len, d, mapSequential f l, mapSequential f r))
fun mapETS SST f rp =
  if length rp <= SST then mapSequential f rp
  else let 
    val (l, r) = split2 rp
    in
      nccat2 (RT.par2 (fn () => mapETS SST f l, 
                       fn () => mapETS SST f r))
    end

fun numUnprocessedMap cur = numUnprocessed length length cur
fun finishMap cur = finish nccat2 cur
fun nextMap cur = next leftmostLeaf nccat2 cur

fun mapUntil cond f cur = let
  fun m (s, c) = (case Seq.mapUntil cond f s
     of More (us, ps) => 
	  if numUnprocessedMap (leaf us, c) < 2 then
	    (case nextMap (leaf (Seq.cat2 (ps, Seq.map f us)), c)
	      of Done p' => Done p'
	       | More (s', c') => m (s', c'))
	  else
	    more Seq.length leaf leaf (us, ps, c)
      | Done ps => (case nextMap (leaf ps, c)
	  of Done p' => Done p'
	   | More (s', c') => m (s', c')))
  val (s, c) = leftmostLeaf cur
  in
    m (s, c)
  end

fun mapLTS PPT f rp = let
  fun m cur = (case mapUntil RT.hungryProcs f cur
    of Done rp => rp
     | More cur' => let
	 val mid = numUnprocessedMap cur' div 2
	 val (rp1, rp2, reb) = 
	       splitAt length encodeRope cursorAtIx unzipCursor unzipCursor cur' mid
	 val (rp1', rp2') = 
	       RT.par2 (fn () => m (start rp1), fn () => m (start rp2))
         in
	   finishMap (join decodeRope finishMap zipCursor (rp1', rp2', reb))
         end)
  in
    if PPT <> 1 then failwith "PPT != 1 currently unsupported" else
    m (start rp)
  end
in
fun map f rp = (case ChunkingPolicy.get ()
  of ChunkingPolicy.Sequential => mapSequential f rp
   | ChunkingPolicy.ETS SST => mapETS SST f rp
   | ChunkingPolicy.LTS PPT => mapLTS PPT f rp)
end

local
fun reduceSequential f b rp =
  (case rp
    of Leaf s => 
         Seq.reduce f b s
     | Cat(_, _, l, r) => 
         f (reduceSequential f b l, reduceSequential f b r))

fun reduceETS SST f b rp = let
  fun red rp =
    if length rp <= SST then 
      reduceSequential f b rp
    else let
      val (l, r) = splitAtIx2 (rp, length rp div 2 - 1)
      in
        f (RT.par2 (fn () => red l, fn () => red r))
      end
  in
    red rp
  end

fun numUnprocessedRed cur = numUnprocessed (fn _ => 0) length cur

fun reduceUntil PPT cond f b cur = let
  fun red (s, c) = (case Seq.reduceUntil cond f b s
    of Done p => (case next leftmostLeaf f (p, c)
         of Done p => Done p
	  | More (s', c') => red (s', c'))
     | More (p, us) =>
         if numUnprocessedRed (leaf us, c) < 2 then
	    (case next leftmostLeaf f (Seq.reduce f p us, c)
	      of Done p' => Done p'
	       | More (s', c') => red (s', c'))
	  else
            More (leaf us, GCRight (p, c)))
  val (s, c) = leftmostLeaf cur
  in
    red (s, c)
  end

fun reduceLTS PPT f b rp = let
  fun red rp = (case reduceUntil PPT RT.hungryProcs f b (rp, GCTop)
    of Done v => v
     | More cur => let
	 val (p, u) = splitCursor (f, b) (cat2, empty ()) cur
	 val mid = numUnprocessedRed cur div 2
	 val (u1, u2) = splitAtIx2 (u, mid - 1) handle _ => raise Fail (Int.toString (mid-1)^" "^Int.toString (length u)^" " ^Int.toString (numUnprocessedRed cur))
         in
	   f (p, f (RT.par2 (fn () => red u1, fn () => red u2)))
         end)
  in
    red rp
  end
in
fun reduce f b rp = (case ChunkingPolicy.get ()
  of ChunkingPolicy.Sequential => reduceSequential f b rp
   | ChunkingPolicy.ETS SST => reduceETS SST f b rp
   | ChunkingPolicy.LTS PPT => reduceLTS PPT f b rp)
end (* local *)

(*local*)
fun scanSequential f b rp = let
  fun s (rp, acc) = (case rp
    of Leaf s => let
	 val s' = Seq.scan f acc s
	 in
	    (Leaf s', f (seqLast s, seqLast s'))
	 end
     | Cat (len, d, rp1, rp2) => let
         val (rp1', acc) = s (rp1, acc)
	 val (rp2', acc) = s (rp2, acc)
         in
	   (Cat (len, d, rp1', rp2'), acc)
         end)
  val (srp, _) = s (rp, b)
  in
    srp
  end

datatype 'a mc_rope
  = MCLeaf of 'a * 'a Seq.seq
  | MCCat of 'a * int * int * 'a mc_rope * 'a mc_rope

fun mcempty b = MCLeaf (b, Seq.empty ())

fun mcval mcrp = (case mcrp
  of MCLeaf (c, _) => c
   | MCCat (c, _, _, _, _) => c)

fun mclength mcrp = (case mcrp
  of MCLeaf (_, s) => Seq.length s
   | MCCat (_, len, _, _, _) => len)

fun mcdepth mcrp = (case mcrp
  of MCLeaf _ => 0
   | MCCat (_, _, d, _, _) => d)

fun mcleaf' (b, s) = 
  if Seq.length s > LeafSize.getMax () then
    failwith "bogus leaf size"
  else 
    MCLeaf (b, s)
fun mcleaf (f, b, s) = mcleaf' (Seq.reduce f b s, s)

fun mcsingleton e = MCLeaf (e, Seq.singleton e)

fun toListMC rp = (case rp
  of MCLeaf (_, s) => Seq.toList s
   | MCCat(_, _, _, l, r) => (toListMC l) @ (toListMC r))

fun mcnccat2 (f, mcrp1, mcrp2) = let
  val c = f (mcval mcrp1, mcval mcrp2)
  val l = mclength mcrp1 + mclength mcrp2
  val d = Int.max (mcdepth mcrp1, mcdepth mcrp2) + 1
  in
    MCCat (c, l, d, mcrp1, mcrp2)
  end

fun mcnccat2' f (mcrp1, mcrp2) = mcnccat2 (f, mcrp1, mcrp2)

fun mcleaves rp = (case rp
  of MCLeaf (_, s) => s::nil
   | MCCat (_, _, _, rp1, rp2) => mcleaves rp1 @ mcleaves rp2)

fun mcToSeq rp = Seq.catN (mcleaves rp)

fun mcccat2 (f, mcrp1, mcrp2) =
  if mclength mcrp1 + mclength mcrp2 <= LeafSize.getMax () then 
    mcleaf' (f (mcval mcrp1, mcval mcrp2), mcToSeq (mcnccat2 (f, mcrp1, mcrp2)))
  else
    mcnccat2 (f, mcrp1, mcrp2)

fun mcsplit2 (f, b, mcrp) =
  (case mcrp 
    of MCLeaf (_, s) => let val (s1, s2) = Seq.split2 s
			in (mcleaf (f, b, s1), mcleaf (f, b, s2))
			end
     | MCCat (_, _, _, l, r) => (l, r))

fun mcInBounds (r, i) = (i < mclength r) andalso (i >= 0)

fun leftmostMCLeaf (mcrp, c) = (case mcrp
  of MCLeaf (cv, s) => (cv, s, c)
   | MCCat (_, _, _, l, r) => leftmostMCLeaf (l, GCLeft (c, r)))

fun startMC rp = let
  val (cv, s, c) = leftmostMCLeaf (rp, GCTop)
  in
    (mcleaf' (cv, s), c)
  end

fun cursorAtIxMC f b (rp, i) = let
  fun nav ((rp, (ls, rs, ds)), i) = (case rp
    of MCLeaf (cv, s) =>
         if Seq.length s = 1 then
	   (mcleaf' (cv, s), (ls, rs, ds))
	 else let
	   val (l, m, r) = seqSplitAtIx3 (s, i)
	   val c' = (mcleaf (f, b, l) :: ls, mcleaf (f, b, r) :: rs, Right :: Left :: ds)
	   in
	     (mcleaf (f, b, Seq.singleton m), c')
	   end
    | MCCat (_, _, _, l, r) =>
	if i < mclength l then
	  nav ((l, (ls, r :: rs, Left :: ds)), i)
	else
	  nav ((r, (l :: ls, rs, Right :: ds)), i - mclength l))
  in
    if mcInBounds (rp, i) then
      nav ((rp, (nil, nil, nil)), i)
    else
      subscript ()
  end

fun decodeMCRope (rp, n) = 
  if n = 1 then
    rp::nil
  else (case rp
    of MCCat (_, _, _, rp1, rp2) =>
       rp1 :: decodeMCRope (rp2, n - 1)
     | _ => failwith "decodeRope")

fun encodeMCRope f rps = let
  fun e rs = (case rs
    of rp::nil => 
         rp
     | rp :: rps =>
         mcnccat2 (f, rp, e rps)
     | _ => failwith "encodeRope")
  in
    (e rps, List.length rps)
  end

fun upsweepSequential f b rp = let
  fun up rp = (case rp
    of Leaf s => mcleaf (f, b, s)
     | Cat (_, _, rp1, rp2) => mcnccat2 (f, up rp1, up rp2))
  in
    up rp
  end

fun downsweepSequential f b mcrp = let
  fun down (mcrp, acc) = (case mcrp
    of MCLeaf (_, s) => 
         leaf (Seq.scan f acc s)
     | MCCat (_, _, _, mcrp1, mcrp2) => 
         nccat2 (down (mcrp1, acc), down (mcrp2, f (acc, mcval mcrp1))))
  in
    down (mcrp, b)
  end

fun scanETS SST f b rp = let
  fun upsweep rp =
    if length rp <= SST then
      upsweepSequential f b rp
    else let
      val (rp1, rp2) = split2 rp
      val (mcrp1, mcrp2) = RT.par2 (fn () => upsweep rp1, fn () => upsweep rp2)
      in
	mcnccat2 (f, mcrp1, mcrp2)
      end
  fun downsweep (mcrp, acc) =
    if mclength mcrp <= SST then
      downsweepSequential f acc mcrp
    else let
      val (mcrp1, mcrp2) = mcsplit2 (f, b, mcrp)
      val (rp1, rp2) = RT.par2 (fn () => downsweep (mcrp1, acc),
				fn () => downsweep (mcrp2, f (mcval mcrp1, acc)))
      in
        nccat2 (rp1, rp2)
      end
  in
    downsweep (upsweep rp, b)
  end

fun numUnprocessedUpsweep cur = numUnprocessed mclength length cur
fun nextUpsweep f cur = next leftmostLeaf (mcnccat2' f) cur
fun finishUpsweep f cur = finish (mcnccat2' f) cur

fun upsweepUntil cond f b cur = let
  fun u (s, c) = (case Seq.reduceUntil cond f b s
     of More (p, us) => let
	  val ps = mcleaf' (p, Seq.take (s, Seq.length s - Seq.length us))
	  in
	    if numUnprocessedUpsweep (leaf us, c) < 2 then
	      (case nextUpsweep f (mcccat2 (f, ps, mcleaf (f, b, us)), c)
		of Done p' => Done p'
		 | More (s', c') => u (s', c'))
	    else 
	      more mclength leaf (fn s => s) (us, ps, c)
	  end
      | Done p => (case nextUpsweep f (mcleaf' (p, s), c)
	  of Done p' => Done p'
	   | More (s', c') => u (s', c')))
  val (s, c) = leftmostLeaf cur
  in
    u (s, c)
  end

fun upsweepLTS PPT f b rp = let
  fun u cur = (case upsweepUntil RT.hungryProcs f b cur
    of Done mcrp => mcrp
     | More cur' => let
	 val mid = numUnprocessedUpsweep cur' div 2
	 val (rp1, rp2, reb) = 
	       splitAt length encodeRope cursorAtIx unzipCursor unzipCursor cur' mid
	 val (mcrp1, mcrp2) = RT.par2 (fn () => u (start rp1), fn () => u (start rp2))
	 fun finish cur = finishUpsweep f cur
	 in
	   finish (join decodeMCRope finish zipCursor (mcrp1, mcrp2, reb))
         end)
  in
    u (rp, GCTop)
  end

fun numUnprocessedDownsweep cur = numUnprocessed length mclength cur
fun finishDownsweep cur = finish nccat2 cur
fun nextDownsweep cur = next leftmostMCLeaf nccat2 cur

fun downsweepUntil cond f b acc cur = let
  fun d (s, c, acc) = (case Seq.scanUntil cond f acc s
    of (acc, More (us, ps)) => 
         if numUnprocessedDownsweep (mcleaf' (b, us), c) < 2 then let
	    val (acc', Done us') = Seq.scanUntil (fn _ => false) f acc us
            in
	      case nextDownsweep (leaf (Seq.cat2 (ps, us')), c)
	       of Done p' => Done p'
		| More (_, s', c') => d (s', c', acc')
            end
	  else let
            val c' = if Seq.length ps = 0 then c else GCRight (leaf ps, c)
	    in
              More (acc, (mcleaf (f, b, us), c'))
	    end
     | (acc, Done ps) => (case nextDownsweep (leaf ps, c)
         of Done rp => Done rp
	  | More (cv, s', c') => d (s', c', acc)))
  val (_, s, c) = leftmostMCLeaf cur
  in
    d (s, c, acc)
  end

fun downsweepLTS PPT f b mcrp = let
  fun d (cur, acc) = (case downsweepUntil RT.hungryProcs f b acc cur
    of Done rp => rp
     | More (acc, cur') => let
	 val mid = numUnprocessedDownsweep cur' div 2
	 val (mcrp1, mcrp2, reb) =
	       splitAt mclength (encodeMCRope f) (cursorAtIxMC f b) unzipCursor unzipCursor cur' mid
	 val (rp1, rp2) = RT.par2 (fn () => d ((startMC mcrp1), acc), 
				   fn () => d ((startMC mcrp2), f (acc, mcval mcrp1)))
         in
	   finishDownsweep (join decodeRope finishDownsweep zipCursor (rp1, rp2, reb))
         end)
  in
    d ((mcrp, GCTop), b)
  end

fun scanLTS PPT f b rp = downsweepLTS PPT f b (upsweepLTS PPT f b rp)
(*in*)
fun scan f b rp = (case ChunkingPolicy.get ()
  of ChunkingPolicy.Sequential => scanSequential f b rp
   | ChunkingPolicy.ETS SST => scanETS SST f b rp
   | ChunkingPolicy.LTS PPT => scanLTS PPT f b rp)
(*end*)

fun take (rp, n) = let
  val (l, _) = splitAtIx2 (rp, n - 1)
  in
    balance l
  end
fun drop (rp, n) = let
  val (_, r) = splitAtIx2 (rp, n - 1)
  in
    balance r
  end

(*local*)
fun filterSequential f rp =
  (case rp
    of Leaf s => 
         leaf (Seq.filter f s)
     | Cat(_, _, l, r) => 
         ccat2 (filterSequential f l, filterSequential f r))

fun filterETS SST f rp = let
  fun filt rp =
    if length rp <= SST then
      filterSequential f rp
    else let
      val (l, r) = splitAtIx2 (rp, length rp div 2 - 1)
      in
	ccat2 (RT.par2 (fn () => filt l, fn () => filt r))
      end
  in
    filt rp
  end

fun nextFilt cur = next leftmostLeaf ccat2 cur
fun numUnprocessedFilt cur = numUnprocessed length length cur

fun filterUntil PPT cond f cur = let
  fun flt (s, c) = (case Seq.filterUntil cond f s
    of More (us, ps) => 
         if numUnprocessedFilt (leaf us, c) < 2 then
	    (case nextFilt (leaf (Seq.cat2 (ps, Seq.filter f us)), c)
	      of Done p' => Done p'
	       | More (s', c') => flt (s', c'))
	  else
	    more Seq.length leaf leaf (us, ps, c)
     | Done ps => (case nextFilt (Leaf ps, c)
         of Done p' => Done p'
	  | More (s', c') => flt (s', c')))
  val (s, c) = leftmostLeaf cur
  in
    flt (s, c)
  end

fun filterLTS PPT f rp = let
  fun flt rp = (case filterUntil PPT RT.hungryProcs f (rp, GCTop) 
    of Done rp => rp
     | More cur => let
         val (p, u) = splitCursor (ccat2, empty ()) (ccat2, empty ()) cur
	 val mid = length u div 2
	 val (u1, u2) = splitAtIx2 (u, mid - 1)
         in
	   ccat2 (p, ccat2 (RT.par2 (fn () => flt u1, fn () => flt u2)))
         end)
  in
    flt rp
  end
(*in*)
fun filter' f rp = (case ChunkingPolicy.get ()
  of ChunkingPolicy.Sequential => filterSequential f rp
   | ChunkingPolicy.ETS SST => filterETS SST f rp
   | ChunkingPolicy.LTS PPT => filterLTS PPT f rp)
fun filter f rp = balance (filter' f rp)
(*end*)

fun app f rp = let
  fun doit rp = (case rp
    of Leaf s => Seq.app f s
     | Cat (_, _, rp1, rp2) => (doit rp1; doit rp2))
  in
    doit rp
  end

end
