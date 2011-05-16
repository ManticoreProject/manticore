(* rope-pair.pml  
 *
 * COPYRIGHT (c) The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Analgous to ListPair.
 *)

structure RopePair (* : ROPE_PAIR *) = struct

    structure S = Rope.Seq
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
            of (R.Leaf (len1, _), R.Leaf (len2, _)) => (len1 = len2)
	     | (R.Cat (_, _, r1L, r1R), R.Cat (_, _, r2L, r2R)) =>
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
    fun fastMap (f, r1, r2) = let
      fun lp ropes = 
       (case ropes
	  of (R.Leaf s1, R.Leaf s2) => 
               R.leaf (Seq.map2 (f, s1, s2))
	   | (R.Cat (d1, len1, r1L, r1R), R.Cat (d2, len2, r2L, r2R)) =>
               R.Cat (| d1, len1, lp (r1L, r2L), lp (r1R, r2R) |)
	   | _ => failwith "BUG" (* this shouldn't have been called *)
         (* end case *))
      in
	lp (r1, r2)
      end

  (* mapP' : ('a * 'b -> 'g) * 'a rope * 'b rope -> 'g rope *)
  (* pre : the first rope's length is <= that of the second *)
  (* traversal follows the structure of the shorter rope *)
  (* post : the output rope has the same shape as the first rope *)
    fun map' (f, ropeS, ropeL) = let
      fun go (n, r) = 
       (case r
          of R.Leaf sS => let
               val (lo, hi) = (n, n+S.length sS)
	       val sL = R.partialSeq (ropeL, lo, hi)
	       val s = Seq.map2 (f, sS, sL)
               in
		 R.leaf s
               end
	   | R.Cat (d, len, rL, rR) => let
	       val (rL', rR') = (| go (n, rL), go (n + R.length rL, rR) |)
               in
                 R.Cat (d, len, rL', rR')
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
    fun map (f, rope1, rope2)  =
(*     (if sameStructure (rope1, rope2) then
        fastMap (f, rope1, rope2)
      else *) if R.length rope1 > R.length rope2 then let
        fun f' x = (case x of (b, a) => f (a, b))
        in
          map' (f', rope2, rope1)
        end
      else
        map' (f, rope1, rope2)

  (* zipP : 'a rope * 'b rope -> ('a * 'b) rope *)
  (* Zipping silently completes when one rope runs out. *)
    fun zip (r1, r2) = failwith "todo" (* mapP (fn (a,b) => (a,b), r1, r2) *)

  (* unzipP : ('a * 'b) rope -> 'a rope * 'b rope *)
  (* pre: input rope is balanced. *)
  (* post: output ropes have exactly the shape as the input rope. *)
    fun unzip rope = 
     (case rope
        of R.Leaf l => let
	     val (l1, l2) = Seq.unzip l
	     in
	       (R.leaf l1, 
		R.leaf l2)
	     end
	 | R.Cat (d, len, r1, r2) => let
	     val ((r11, r12), (r21, r22)) = (| unzip r1, unzip r2 |)
	     in
	       (R.Cat (d, len, r11, r21),
		R.Cat (d, len, r12, r22))
	     end
       (* end case *))

  end
