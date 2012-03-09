(* rope-pair.pml  
 *
 * COPYRIGHT (c) The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Analogous to ListPair.
 *)

structure IntDoubleRopePair = struct

    structure IS = IntSeq
    structure DS = DoubleSeq

    structure IR = IntRope
    structure DR = DoubleRope

    datatype option = datatype Option.option

  (* ***** UTILITIES ***** *)

  (* iddmap : (int * double -> double) * int_seq * dbl_seq -> dbl_seq *)
    fun iddmap (f : int * double -> double, is : IS.int_seq, ds : DS.double_seq) 
        : DS.double_seq = let
      val n = IS.length is
      val _ = if DS.length ds = n then ()
	      else (Print.printLn "iddmap: length mismatch";
		    raise Fail "iddmap -- length mismatch")
      val res = DS.tabulate (n, fn i => (f (IS.sub (is, i), DS.sub (ds, i))))
      in
        res
      end

  (* fastMapDbl : (int * double -> double) * int_rope * double_rope -> double_rope *)
  (* pre : both ropes have exactly the same structure *)
    fun fastMapDbl (f : int * double -> double, r1 : IR.int_rope, r2 : DR.double_rope) 
        : DR.double_rope = let
      fun lp ropes = (case ropes
        of (IR.Leaf s1, DR.Leaf s2) => 
             DR.leaf (iddmap (f, s1, s2))
	 | (IR.Cat (d1, len1, r1L, r1R), DR.Cat (d2, len2, r2L, r2R)) =>
             DR.Cat (| d1, len1+len2, lp (r1L, r2L), lp (r1R, r2R) |)
	 | _ => (Print.printLn "IDRP.fastMapDbl -- BUG: called on ropes of different shapes";
		 raise Fail "fastMapP -- BUG: called on ropes of different shapes")
        (* end case *))
      in
	lp (r1, r2)
      end

  end
