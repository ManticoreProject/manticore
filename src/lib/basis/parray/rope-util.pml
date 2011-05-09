(* rope-utils.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure RopeUtil = struct

  structure R = Rope
  structure IR = IntRope
  structure DR = DoubleRope

  (* mapP_int : ('a -> int) * 'a rope -> IR.int_rope *)
  (* post : the output has the same shape as the input *)
    fun mapP_int (f, rope) = let
      fun m r = (case r
        of R.LEAF (n, s) => let 
             val s' = R.S.map (f, s)
             in
               IR.leafFromSeq s'
             end
	 | R.CAT (dpt, len, r1, r2) => IR.CAT (| dpt, len, m r1, m r2 |)
        (* end case *))
      in
	m rope
      end

  (* mapP_double : ('a -> double) * 'a rope -> DoubleRope.double_rope *)
  (* post : the output has the same shape as the input *)
    fun mapP_double (f, rope) = let
      fun m r = (case r
        of R.LEAF (n, s) => let 
             val s' = R.S.map (f, s)
             in
               DR.leafFromSeq s'
             end
	 | R.CAT (dpt, len, r1, r2) => DR.CAT (| dpt, len, m r1, m r2 |)
        (* end case *))
      in
	m rope
      end

end
