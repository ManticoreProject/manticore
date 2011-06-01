(* rope-util.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure RopeUtil = struct

  val fail = Fail.fail "RopeUtil"

  structure R = Rope
  structure IR = IntRope
  structure DR = DoubleRope

  (* map_int : ('a -> int) * 'a rope -> IR.int_rope *)
  (* post : the output has the same shape as the input *)
    fun map_int (f, rope) = let
val _ = Print.printLn ("RopeUtil.map_int " ^ Int.toString (R.length rope))
      fun m r = (case r
        of R.Leaf s => let 
             val s' = SeqUtil.map_int (f, s)
             in
               IR.leaf s'
             end
	 | R.Cat (dpt, len, r1, r2) => IR.Cat (| dpt, len, m r1, m r2 |)
        (* end case *))
      in
	m rope
      end

  (* map_double : ('a -> double) * 'a rope -> DR.double_rope *)
  (* post : the output has the same shape as the input *)
    fun map_double (f, rope) = let
val _ = Print.printLn "map_double"
      fun m r = (case r
        of R.Leaf s => let 
             val s' = SeqUtil.map_double (f, s)
             in
               DR.leaf s'
             end
	 | R.Cat (dpt, len, r1, r2) => DR.Cat (| dpt, len, m r1, m r2 |)
        (* end case *))
      in
	m rope
      end

  (* mapDDD : (double * double -> double) * double_rope * double_rope -> double_rope *)
    fun mapDDD (f : double * double -> double, r1, r2) = 
      if DR.sameShape (r1, r2) then let
        fun lp (r1, r2) = (case (r1, r2)
          of (DR.Leaf s1, DR.Leaf s2) => let
               val n = DoubleSeq.length s1
	       fun f' i = f (DoubleSeq.sub (s1, i), DoubleSeq.sub (s2, i))
	       val s = DoubleSeq.tabulate (n, f')
               in
		 DR.leaf s
	       end
	   | (DR.Cat (d, l, r1, r2), DR.Cat (_, _, r1', r2')) =>
	       DR.Cat (| d, l, lp (r1, r1'), lp (r2, r2') |)
          (* end case *))
        in
	  lp (r1, r2)
	end
      else 
        fail "mapDDD" "different shapes"

end
