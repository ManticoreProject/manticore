(* rope-pair.pml  
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Analgous to ListPair.
 *)

structure RopePair (* : ROPE_PAIR *) = struct

    structure S = Rope.S
    structure P = Rope.SPr
    structure R = Rope

    datatype option = datatype Option.option

    type 'a seq = 'a S.seq

  (* ***** UTILITIES ***** *)

  (* failwith : string -> 'a *)
  (* using this for the moment so we can observe the exception message at runtime *)
    fun failwith msg = (Print.printLn msg; (raise Fail msg))

(*
  (* sameStructureP : 'a rope * 'b rope -> bool *)
    fun sameStructureP (r1, r2) = 
     (if R.length r1 <> R.length r2 then
        false
      else let (* same length *)
        fun lp (r1, r2) =
         (case (r1, r2)
            of (R.LEAF (len1, _), R.LEAF (len2, _)) => (len1 = len2)
	     | (R.CAT (_, _, r1L, r1R), R.CAT (_, _, r2L, r2R)) =>
	         (pcase lp (r1L, r2L) & lp (r1R, r2R)
                     of false & ? => false
		      | ? & false => false
		      | true & true => true
		      | otherwise => failwith "bug: otherwise in sameStructureP"
		    (* end pcase *))

	     | _ => false
	   (* end case *))
	in
	  lp (r1, r2)
	end
     (* end if *))
*)
     
  (* fastMapP : ('a * 'b -> 'g) * 'a rope * 'b rope -> 'g rope *)
  (* pre : both ropes have exactly the same structure *)
    fun fastMapP (f, r1, r2) = let
      fun lp ropes = 
       (case ropes
	  of (R.LEAF s1, R.LEAF s2) => 
               R.mkLeaf (S.map2Eq (f, s1, s2))
	   | (R.CAT (d1, len1, r1L, r1R), R.CAT (d2, len2, r2L, r2R)) =>
               R.CAT (| d1, len1, lp (r1L, r2L), lp (r1R, r2R) |)
	   | _ => failwith "BUG" (* this shouldn't have been called *)
         (* end case *))
      in
	lp (r1, r2)
      end

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
    fun zipP (r1, r2) = failwith "todo" (* mapP (fn (a,b) => (a,b), r1, r2) *)

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

    val cwb = R.concatWithoutBalancing

  (* tabFromToP : int * int * (int -> 'a * 'b) -> 'a rope * 'b rope *)
  (* lo incl, hi incl *)
    fun tabFromToP (lo, hi, f) =
      if (lo > hi) then
        (R.empty, R.empty)
      else let
        fun f1 i = let val (x,_) = f(i) in x end
	fun f2 i = let val (_,x) = f(i) in x end
        fun lp (lo, hi) = let
          val nElts = hi-lo+1
	  in
            if nElts <= R.maxLeafSize then let
              val (sA, sB) = P.tabulate (nElts, fn i => f (lo+i))
              in
                (R.mkLeaf sA, R.mkLeaf sB)
              end
	    else let
              val m = (hi + lo) div 2
	      val ((r1L, r2L), (r1R, r2R)) = (| lp (lo, m), lp (m+1, hi) |)
              in
                (cwb (r1L, r1R), cwb (r2L, r2R))
	      end
	  end
        in
          lp (lo, hi)
	end

  (* tabP : int * (int -> 'a * 'b) -> 'a rope * 'b rope *)
    fun tabP (n, f) = tabFromToP (0, n-1, f)

  (* tabFromToStepP : int * int * int * (int -> 'a * 'b) -> 'a rope * 'b rope *)
  (* lo incl, hi incl *)
    fun tabFromToStepP (from, to_, step, f) = let
      fun f' i = f (from + (step * i))
      in (case Int.compare (step, 0)
        of EQUAL => (raise Fail "0 step")
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

end
