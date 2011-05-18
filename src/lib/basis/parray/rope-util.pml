(* rope-util.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure RopeUtil = struct

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

end
