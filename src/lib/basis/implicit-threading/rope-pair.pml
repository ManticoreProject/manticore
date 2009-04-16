(* rope-pair.pml  
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Analgous to ListPair.
 *)

structure RopePair (* : ROPE_PAIR *) = struct

    structure S = ListSeq
    structure R = Ropes

    datatype option = datatype Option.option

    type 'a seq = 'a S.seq

  (* ***** UTILITIES ***** *)

  (* failwith : string -> 'a *)
  (* using this for the moment so we can observe the exception message at runtime *)
    fun failwith msg = (Print.printLn msg; (raise Fail msg))

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
     
  (* fastMapP : ('a * 'b -> 'g) * 'a rope * 'b rope -> 'g rope *)
  (* pre : both ropes have exactly the same structure *)
    fun fastMapP (f, r1, r2) = let
      fun lp ropes = 
       (case ropes
	  of (R.LEAF (len1, s1), R.LEAF (len2, s2)) => 
               R.LEAF (len1, S.map2 (f, s1, s2))
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
          of R.LEAF (len, sS) => let
               val (lo, hi) = (n, n+len)
	       val sL = R.partialSeq (ropeL, lo, hi)
	       val s = S.map2 (f, sS, sL)
               in
		 R.LEAF (len, s)
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
  (* note: the sameStructureP test might not be worth it when leaf size is small *)
    fun mapP (f, rope1, rope2) =
     (if sameStructureP (rope1, rope2) then
        fastMapP (f, rope1, rope2)
      else if R.length rope1 > R.length rope2 then let
        fun f' x = (case x of (b, a) => f (a, b))
        in
          mapP' (f', rope2, rope1)
        end
      else
        mapP' (f, rope1, rope2))
    
  (* zipP : 'a rope * 'b rope -> ('a * 'b) rope *)
  (* Zipping silently completes when one rope runs out. *)
    fun zipP (r1, r2) = mapP (fn (a,b) => (a,b), r1, r2)

  (* unzipP : ('a * 'b) rope -> 'a rope * 'b rope *)
  (* pre: input rope is balanced. *)
  (* post: output ropes have exactly the shape as the input rope. *)
    fun unzipP rope = 
     (case rope
        of R.LEAF (len, l) => let
	     val (l1, l2) = S.unzip l
	     in
	       (R.LEAF (len, l1), 
		R.LEAF (len, l2))
	     end
	 | R.CAT (d, len, r1, r2) => let
	     val ((r11, r12), (r21, r22)) = (| unzipP r1, unzipP r2 |)
	     in
	       (R.CAT (d, len, r11, r21),
		R.CAT (d, len, r12, r22))
	     end
       (* end case *))

  end
