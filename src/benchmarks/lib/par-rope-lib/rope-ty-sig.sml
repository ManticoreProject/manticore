(* rope-ty-sig.sml
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

signature ROPE_TY =
  sig

    structure S : SEQ

    datatype 'a rope
      = Cat of (int *         (* depth *)
		int *         (* length *)
		'a rope *     (* left subtree *)
		'a rope       (* right subtree *))
      | Leaf of 'a S.seq      (* sequence *)

    val isOK : 'a rope -> unit
    val computeLength : 'a rope -> int

  end
