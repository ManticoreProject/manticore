(* seq-util.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure SeqUtil = struct

  structure VS = Seq
  structure IS = IntSeq
  structure DS = DoubleSeq

  (* map_int : ('a -> int) * 'a VS.seq -> IS.int_seq *)
    fun map_int (f, s) = let
      val n = VS.length s
      val is = IS.unsafeCreate n
      fun lp i = 
        if (i >= n) then
          is
	else let
          val x = f (VS.sub (s, i))
          in
            (IS.update (is, i, x);
	     lp (i+1))
          end
      in
        lp 0
      end

  (* map_double : ('a -> double) * 'a VS.seq -> DS.double_seq *)
    fun map_double (f, s) = let
      val n = VS.length s
      val ds = DS.unsafeCreate n
      fun lp i = 
        if (i >= n) then
          ds
	else let
          val x = f (VS.sub (s, i))
          in
            (DS.update (ds, i, x);
	     lp (i+1))
          end
      in
        lp 0
      end

end
