(* rope-pair.pml  
 *
 * COPYRIGHT (c) The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Analgous to ListPair.
 *)

structure RopePair (* : ROPE_PAIR *) = struct

structure R = Rope
structure RT = R.RT

structure Seq = R.Seq
type 'a seq = 'a Seq.seq
datatype rope = datatype R.rope
datatype gen_ctx = datatype R.gen_ctx
datatype progress = datatype Progress.progress

val length = R.length
val leaf = R.leaf
val nccat2 = R.nccat2
val ccat2 = R.ccat2
val leftmostLeaf = R.leftmostLeaf
val start = R.start
val finish = R.finish
val next = R.next
val cursorAtIx = R.cursorAtIx
val numUnprocessed = R.numUnprocessed
val join = R.join
val encodeRope = R.encodeRope
val decodeRope = R.decodeRope
val zipCursor = R.zipCursor
val unzipCursor = R.unzipCursor
val splitAt = R.splitAt
val toSeq = R.toSeq
val take = R.take
val drop = R.drop
val map = R.map
val seqDrop = R.Seq.drop
val seqSub = R.Seq.sub
val fromListRev = R.Seq.fromList
val sub = R.sub

fun failwith msg = (Print.printLn msg; (raise Fail msg))

fun unequalLengths () = failwith "RopePair: unequal lengths"

fun min (x, y) = if x < y then x else y

fun unzip rp = 
  RT.par2 (fn () => map (fn (x, _) => x) rp, fn () => map (fn (_, y) => y) rp)

fun numUnprocessedMap cur = numUnprocessed length length cur
fun finishMap cur = finish nccat2 cur
fun nextMap cur = next leftmostLeaf nccat2 cur

fun empty () = leaf (Seq.empty ())

fun equalizeLengths (rp0, rp1) = let
  val l0 = length rp0
  val l1 = length rp1
  val l = min (l0, l1)
  in
    if l0 = l1 then (rp0, rp1)
    else (take (rp0, l), take (rp1, l))
  end


fun nextICur (i, (rp, c)) =
  if i < length rp then ((i+1, (rp, c)), sub (rp, i))
  else (case nextMap (empty (), c)
   of Done rp' => ((i+1, (rp, GCTop)), sub (rp, i))
    | More (s', c') => nextICur (0, (leaf s', c')))

fun hdICur (i, (rp, _)) = sub (rp, i)

fun curOfICur (i, (rp, c)) = if i = 0 then (rp, c) else (drop (rp, i), c) 

fun icurOfCur cur = (0, cur)

fun seqMapPairUntil cond f (s0, icur1) = let
  val len = length (leaf s0)
  fun lp (i, icur1, acc) = 
    if i < len then
      if cond () then
	More (seqDrop (s0, i), icur1, fromListRev acc) 
      else let
	val (icur', x) = nextICur icur1 
	in
	  lp (i+1, icur', f (seqSub (s0, i), x)::acc) 
	end
    else
      Done (icur1, fromListRev acc)
  in
    lp (0, icur1, nil)
  end

fun mapUntil cond f (cur0, cur1) = let
  fun m (s0, c0, icur1) = (case seqMapPairUntil cond f (s0, icur1) 
     of More (us0, icur1', ps) => 
       if numUnprocessedMap (leaf us0, c0) < 2 then 
	 (case (seqMapPairUntil (fn _ => false) f (us0, icur1'))
	   of Done (icur1'', ps') =>
	      (case nextMap (leaf (Seq.cat2 (ps, ps')), c0)
		of Done p' => Done p'
		 | More (s0', c0') => m (s0', c0', icur1''))
	    | _ => failwith "bogus")
       else let
	 val c0' = if Seq.length ps = 0 then c0 else GCRight (leaf ps, c0)
	 val cur1 = curOfICur icur1'
	 in
	   More ((leaf us0, c0'), cur1)
	 end 
      | Done (icur1', ps) => (case nextMap (leaf ps, c0)
	  of Done p' => Done p'
	   | More (s0', c0') => m (s0', c0', icur1')))
  val (s0, c0) = leftmostLeaf cur0
  in
    m (s0, c0, icurOfCur cur1) 
  end


fun map f (rp0, rp1) = let
  fun m (cur0, cur1) = (case mapUntil RT.hungryProcs f (cur0, cur1)
    of Done rp => rp
     | More (cur0, cur1) => let
	 val ml = min (numUnprocessedMap cur0, numUnprocessedMap cur1)
	 val mid = ml div 2
	 val (rp00, rp01, reb) =
	     splitAt length encodeRope cursorAtIx unzipCursor unzipCursor cur0 mid
	 val (rp10, rp11, _) =
	     splitAt length encodeRope cursorAtIx unzipCursor unzipCursor cur1 mid
	 val (rp0, rp1) =
	     RT.par2 (fn () => m (start rp00, start rp10), fn () => m (start rp01, start rp11))
         in
	   finishMap (join decodeRope finishMap zipCursor (rp0, rp1, reb))
         end)
  val (rp0, rp1) = equalizeLengths (rp0, rp1)
  in
    m (start rp0, start rp1)
  end

fun mapEq f (rp0, rp1) = 
  if length rp0 = length rp1 then map f (rp0, rp1) else unequalLengths ()

fun zip (rp0, rp1) = map (fn x => x) (rp0, rp1)

fun zipEq (rp0, rp1) = 
  if length rp0 = length rp1 then zip (rp0, rp1) else unequalLengths ()


end
