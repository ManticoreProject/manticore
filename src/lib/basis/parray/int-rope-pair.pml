(* int-rope-pair.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Analogous to ListPair.
 *)

structure IntRopePair = struct

  val fail = Fail.fail "IntRopePair"

  structure R = IntRope
  structure S = R.S
  structure P = R.SPr
    
  type seq = S.seq

(* fastMapP_int : (int * int -> int) -> int_rope * int_rope -> int_rope *)
(* pre : both ropes have exactly the same structure *)
  fun fastMapP_int f (r1, r2) = let
    fun mapF ropes = (case ropes
      of (R.LEAF (n1, s1), R.LEAF (n2, s2)) => 
           R.mkLeaf (P.mapEq_int (f, s1, s2))
       | (R.CAT (d1, len1, r1L, r1R), R.CAT (d2, len2, r2L, r2R)) =>
           R.CAT (| d1, len1, mapF (r1L, r2L), mapF (r1R, r2R) |)
       | _ => fail "fastMapP_int" "bug"
      (* end case *))
    in
      mapF (r1, r2)
    end

(* mapEq_intPair *)
  fun mapEq_intPair (f : int * int -> int * int) (r1, r2) = let
    fun mapF ropes = (case ropes
      of (R.LEAF (n1, s1), R.LEAF (n2, s2)) => let
           val (s1', s2') = P.mapEq_intPair (f, s1, s2)
           in
	     (R.mkLeaf s1', R.mkLeaf s2')
	   end
       | (R.CAT (d1, len1, r1L, r1R), R.CAT (d2, len2, r2L, r2R)) => let
           val ((r1L', r2L'), (r1R', r2R')) = 
             (| mapF (r1L, r2L), mapF (r1R, r2R) |)
           in
             (R.CAT (d1, len1, r1L', r1R'),
	      R.CAT (d2, len2, r2L', r2R'))
	   end
       | _ => fail "mapEq_intPair" "bug"
      (* end case *))
    in
      mapF (r1, r2)
    end

(* tabFromToP : *)
(* from and to_ both inclusive. *)
(* If from > to_, a pair of empties is returned. *)
  val cwb = R.concatWithoutBalancing
  fun tabFromToP (from, to_, f) =
    if (from > to_) then
      (R.empty, R.empty)
    else let
      val nElts = to_-from+1
      in
        if (nElts <= R.maxLeafSize) then let
          fun f' n = f (n+from)
          val (s1, s2) = P.tabulate (nElts, f')
          in
            (R.LEAF (nElts, s1), R.LEAF (nElts, s2))
          end
        else let
          val m = (from+to_) div 2
          val ((r1L,r2L),(r1R,r2R)) = (| tabFromToP (from, m, f),
				         tabFromToP (m+1, to_, f) |)
          in
            (cwb (r1L, r1R), cwb (r2L, r2R))
          end
      end

  fun tabP (n, f) = tabFromToP (0, n-1, f)

(* tabFromToStepP : int * int * int * (int -> int * int) -> int_rope * int_rope *)
(* lo incl, hi incl *)
  fun tabFromToStepP (from, to_, step, f) = let
    fun f' i = f (from + (step * i))
    in (case Int.compare (step, 0)
      of EQUAL => fail "tabFromToStepP" "0 step"
       | LESS (* negative step *) =>
           if (to_ > from) then 
	     (R.empty, R.empty)
	   else
	     tabFromToP (0, (from-to_) div (~step), f')
       | GREATER (* positive step *) =>
           if (from > to_) then
             (R.empty, R.empty)
	   else
             tabFromToP (0, (to_-from) div step, f')
      (* end case *))
    end

(* let ip = int * int in *)
(* reduceP : (ip * ip -> ip) * ip * (int_rope * int_rope) -> ip * ip *)
(* Reduce with an associative operator. *)
  fun reduceP (assocOp, unit, (ropeA, ropeB)) = let
    fun red (rA, rB) = (case (rA, rB)
      of (R.LEAF (_, sA), R.LEAF (_, sB)) => P.reduce (assocOp, unit, (sA, sB))
       | (R.CAT (_, _, rA1, rA2), R.CAT (_, _, rB1, rB2)) =>
           assocOp (| red (rA1, rB1), red (rA2, rB2) |)
       | _ => fail "reduceP" "shapes"
      (* end case *))
    in
      red (ropeA, ropeB)
    end

(*
  (* mapP' : ('a * 'b -> 'g) * 'a rope * 'b rope -> 'g rope *)
  (* pre : the first rope's length is <= that of the second *)
  (* traversal follows the structure of the shorter rope *)
  (* post : the output rope has the same shape as the first rope *)
    fun mapP' (f, ropeS, ropeL) = let
      fun go (n, r) = 
       (case r
          of R.LEAF sS => let
               val (lo, hi) = (n, n+S.length sS)
	       val sL = R.partialSeq (ropeL, lo, hi)
	       val s = S.map2 (f, sS, sL)
               in
		 R.mkLeaf s
               end
	   | R.CAT (d, len, rL, rR) => let
	       val (rL', rR') = (| go (n, rL), go (n + R.length rL, rR) |)
               in
                 R.CAT (d, len, rL', rR')
	       end
          (* end case *))
      in
        go (0, ropeS)
      end

  (* mapP : ('a * 'b -> 'g) -> 'a rope * 'b rope -> 'g rope *)
  (* stop mapping when the elements of one rope run out *)
  (* post : the output has the same shape as the shorter input *)
  (* note : the sameStructureP test might not be worth it when leaf size is small *)
    (* note : since pcase is currently broken, i've temporarily disabled the sameStructureP
     * check below *)
    fun mapP (f, rope1, rope2)  =
(*     (if sameStructureP (rope1, rope2) then
        fastMapP (f, rope1, rope2)
      else *) if R.length rope1 > R.length rope2 then let
        fun f' x = (case x of (b, a) => f (a, b))
        in
          mapP' (f', rope2, rope1)
        end
      else
        mapP' (f, rope1, rope2)

  (* zipP : 'a rope * 'b rope -> ('a * 'b) rope *)
  (* Zipping silently completes when one rope runs out. *)
    fun zipP (r1, r2) = fail "zipP" "todo" (* mapP (fn (a,b) => (a,b), r1, r2) *)

  (* unzipP : ('a * 'b) rope -> 'a rope * 'b rope *)
  (* pre: input rope is balanced. *)
  (* post: output ropes have exactly the shape as the input rope. *)
    fun unzipP rope = 
     (case rope
        of R.LEAF l => let
	     val (l1, l2) = S.unzip l
	     in
	       (R.mkLeaf l1, 
		R.mkLeaf l2)
	     end
	 | R.CAT (d, len, r1, r2) => let
	     val ((r11, r12), (r21, r22)) = (| unzipP r1, unzipP r2 |)
	     in
	       (R.CAT (d, len, r11, r21),
		R.CAT (d, len, r12, r22))
	     end
       (* end case *))

*)

end
