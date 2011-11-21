structure IntRope = struct

val C = 2

fun failwith s = raise Fail s
fun subscript () = raise Fail "subscript"

structure RT = Runtime
structure Seq = IntSeq
structure RTy = IntRopeTy

datatype progress = datatype Progress.progress

type seq = RTy.Seq.seq
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

fun isBalanced rp =
  (case rp
    of Leaf _ => true
     | _ => depth rp <= C * Int.ceilingLg (length rp))

(*local*)
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
(*in*)
fun balance rp = (case ChunkingPolicy.get ()
  of ChunkingPolicy.Sequential => 
       balanceSequential rp
   | ChunkingPolicy.ETS SST => 
       balanceETS SST rp
   | ChunkingPolicy.LTS PPT => 
     (* balanceLTS PPT rp *)
     (* TODO: fix me *)
     balanceETS 10000 rp)
(*end*)

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
  (rope list * dir list * dir list * int * int * int * int)

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


end
