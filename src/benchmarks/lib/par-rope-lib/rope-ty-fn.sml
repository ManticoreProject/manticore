(* rope-ty-fn.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Datatype definition for ropes.
 * 
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

functor RopeTyFn (

    structure S : SEQ

  ) : ROPE_TY = struct

    structure S = S

    datatype 'a rope
      = Cat of (int *         (* depth *)
		int *         (* length *)
		'a rope *     (* left subtree *)
		'a rope       (* right subtree *))
      | Leaf of 'a S.seq      (* sequence *)

    fun computeDepth rp =
	(case rp
	  of Leaf s => 0
	   | Cat (_, _, l, r) => Int.max (computeDepth l, computeDepth r) + 1)

    fun computeLength rp =
	(case rp
	  of Leaf s => S.length s
	   | Cat (_, _, l, r) => computeLength l + computeLength r)

    fun isOK rp =
	(case rp
	  of Leaf s => ()
	   | Cat (d, len, l, r) =>
	     let 
		 val rd = computeDepth rp
		 val rl = computeLength rp
	     in
		 if rd <> d then 
		     print ("bogus depth... cached:"^Int.toString d^"<>"^Int.toString rd^"\n")
		 else if rl <> len then
		     print ("bogus length... cached"^Int.toString len^"<>"^Int.toString rl^"\n")
		 else (isOK l; isOK r)
	     end)

  end
