(* float-scan.pml  
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Follows the algorithms in Blelloch's "Prefix Sums and their Applications."
 * Adapted for ropes here.
 * (Nov 1990, CMS-CS-90-190)
 *
 * Follows the names of the operations given in Keller's thesis (pp. 53 ff).
 * currently it provides:
 * 
 * plusScan : float_rope -> float_rope ([1.0,1.0,1.0,1.0] => [1.0,2.0,3.0,4.0])
 * plusScan2 : float -> float_rope -> float_rope (2.0 [1.0,1.0,1.0,1.0] => [3.0,4.0,5.0,6.0])
 * prePlusScan : float_rope -> float_rope ([1.0,1.0,1.0,1.0] => [0.0,1.0,2.0,3.0])
 * prePlusScan2 : float -> float_rope -> float_rope ([2.0 [1.0,1.0,1.0,1.0] => [2.0,3.0,4.0,5.0])
 *
 *)

structure FloatScan = struct

    structure S = FloatArraySeq
    structure R = FloatRope

    datatype option = datatype Option.option

    type seq = S.seq

  (* We need a rope that can store a datum at every node. *)
  (* This is different from our other rope type. *)
  (* That datum will be an accumulator. *)
    datatype scan_rope
      = Cat of (float *      (* datum *)
		int *     (* depth *)
		int *     (* length *)
		scan_rope * (* left subtree *)
		scan_rope)  (* right subtree *)
      | Leaf of (float *     (* datum *)
		 int *    (* length *)
		 seq)  (* data *)

  (* ***** UTILITIES ***** *)

  (* failwith : string -> 'a *)
  (* using this for the moment so we can observe the exception message at runtime *)
    fun failwith msg = (Print.printLn msg; (raise Fail msg))

  (* datumOf : scan_rope -> 'a *)
  (* Select the accumulator out of a scan_rope node. *)
    fun datumOf r =
     (case r
        of (Cat (x, _, _, _, _)) => x
	 | (Leaf (x, _, _)) => x
       (* end case *))

  (* ***** FULL SCANS (as opp. to short-circuiting) ***** *)
  
  (* seqscan : num * seq -> seq *)
  (* Does a prefix scan starting from the given seed value. *)
  (* Returns the scanned sequence and the total. *)
    fun seqscan seed seq = S.prefixPlusScan (seed, seq)

  (* seqsum : float -> seq -> float *)
    fun seqsum seed s = seed + (S.sum s)

  (* upsweep : float -> float_rope -> scan_rope *)
    fun upsweep seed t = let
      fun lp r = 
       (case r 
	  of (R.LEAF (len, s)) => Leaf (seqsum seed s, len, s)
	   | (R.CAT (d, len, rL, rR)) => let
               val (uL, uR) = (| lp rL, lp rR |)
               in 
                 Cat (datumOf uL + datumOf uR, d, len, uL, uR)
	       end)
      in
        lp t
      end   

  (* downsweep : float -> scan_rope -> float_rope *)
    fun downsweep seed t = let
      (* FIXME It seems odd that I'm underscoring the datums here... *)
      (* ...think about this more. *)
      fun lp (c, r) =
       (case r
          of (Cat (_, d, len, cL, cR)) => let
               val nL = datumOf cL
	       in
                 R.CAT (| d, len, lp (c, cL), lp (c+nL, cR) |)
               end
	   | (Leaf (_, len, s)) => R.LEAF (len, seqscan c s)
         (* end case *))
      in
        lp (seed, t)
      end

  (* plusScan2 : num -> num rope -> num rope *)
  (* ex: plusScan2 2 [1,1,1,1] => [3,4,5,6] *)
    fun plusScan2 seed r =
      if R.isEmpty r then r
      else let
        val elt0 = R.sub (r, 0)
	val seed' = elt0 + seed
        in
          downsweep seed' (upsweep seed' r)
	end

  (* plusScan : num rope -> num rope *)
  (* ex: plusScan [1,1,1,1] => [1,2,3,4] *)
  (* nb: plusScan is equiv. to (plusScan2 0) *)
    fun plusScan r = plusScan2 0.0 r

  (* prePlusScan2 : num -> num rope -> num rope *)
  (* ex: prePlusScan2 2 [1,1,1,1] => [2,3,4,5] *)
    fun prePlusScan2 seed r = downsweep seed (upsweep seed r)

  (* prePlusScan : num rope -> num rope *)
  (* ex: prePlusScan [1,1,1,1] => plusScan [0,1,2,3] *)
  (* nb: prePlusScan is equiv to (prePlusScan2 0) *)
    fun prePlusScan r = downsweep 0.0 (upsweep 0.0 r)
		     
end
