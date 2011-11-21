(* zipper2.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A Zipper for ropes. The "2" in Zipper2 comes from the fact that an element left of the hole
 * has type 'b and one to the right has type 'a.
 * 
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

structure Zipper2 =
  struct

    structure RTy = RopeTy
    structure S = RTy.S

    datatype rope = datatype RTy.rope
    datatype either = datatype Either.either

    val empty = RTy.empty
    val append = RTy.appendWithoutBalancing
    val ropeOfRopes = RTy.ropeOfRopes

  (* the context representation of a rope *)
  (*   - Top represents the top of the rope *)
  (*   - L (d, len, c, r) represents the left part of a Cat in which r was the node in the right part, *)
  (*     c is the context of the parent node, d is the depth at the Cat node and len is the length *)
  (*   - R (d, len, c, r) similar to L (d, len, c, r) *)
    datatype ('a, 'b) ctx
      = Top
      | L of ('a, 'b) ctx * ('a rope, 'b rope) either
      | R of ('a rope, 'b rope) either * ('a, 'b) ctx

  (* rope location, e.g., (rp, c), where rp is the node in focus and c is the context around the focus *)
    type ('a, 'b) loc = ('a rope, 'b rope) either * ('a, 'b) ctx

  (* move the focus to the left child *)
    fun left loc =
	(case loc
	  of (LEFT (Leaf _), _) => NONE
	   | (RIGHT (Leaf _), _) => NONE
	   | (LEFT (Cat (_, _, l, r)), c) => SOME (LEFT l, L (c, LEFT r))
	   | (RIGHT (Cat (_, _, l, r)), c) => SOME (RIGHT l, L (c, RIGHT r)))

  (* move the focus up to the parent node *)
    fun up loc =
	(case loc
	  of (_, Top) => NONE
	   | (LEFT rp, L (c, LEFT r)) => SOME (LEFT (append (rp, r)), c)
	   | (RIGHT rp, L (c, RIGHT r)) => SOME (RIGHT (append (rp, r)), c)
	   | (LEFT rp, R (LEFT l, c)) => SOME (LEFT (append (l, rp)), c)
	   | (RIGHT rp, R (RIGHT l, c)) => SOME (RIGHT (append (l, rp)), c)
	   | _ => raise Fail "error")

  (* move the focus to the left-most child *)
    fun leftmost loc =
	(case left loc
	  of NONE => loc
	   | SOME loc' => leftmost loc')

  (* move the focus to the root node *)
    fun upmost loc =
	(case up loc
	  of NONE => loc
	   | SOME loc' => upmost loc')

  (* move the focus up to the nearest left-branching point, then move the focus down by one node *)
  (* to the right branch *)
    fun upUntilLThenRight loc =
	(case loc
	  of (rp, L (c, r)) => SOME (r, R (rp, c))
	   | _ => (case up loc
		    of NONE => NONE
		     | SOME loc' => upUntilLThenRight loc'))

  (* put the focus is at the root node of the given rope *)
    fun input rp = (LEFT rp, Top)

  (* put the focus into the hole *)
    fun output loc =
	(case upmost loc
	  of (RIGHT rp, _) => rp
	   | (LEFT _, _) => raise Fail "error")

  (* reverses the order of the given context *)
    fun revCtx c =
	let fun rev loc =
		(case loc
		  of (Top, c') => c'
		   | (L (c, r), c') => rev (c, L (c', r))
		   | (R (l, c), c') => rev (c, R (l, c'))
		(* end case *))
	in
	    rev (c, Top)
	end

  (* returns the pair (l, r), where l is the rope that records the elements that have not yet *)
  (* been visited (everything to the left in the context and "unprocessed") and r records the elements *)
  (* that have been visited (everything to the right in the context and "processed") *)
    fun split (unprd, prd, c) =
	let fun lp (c, lr, rr) =
		(case c
		  of Top => (lr, append (rr, Leaf prd))
		   | L (c, LEFT r) => lp (c, append (r, lr), rr)
		   | R (RIGHT l, c) => lp (c, lr, append (rr, l))
		   | _ => raise Fail "error"
		(* end case *))
	in
	    lp (revCtx (L (c, LEFT (Leaf unprd))), empty (), empty ())
	end

(*
alternative approach that tries to balance both result ropes
    fun split (unprd, prd, c) =
	let
	    fun splt (c, unprds, prds) =
		(case c
		  of Top => (unprds, List.rev (Leaf prd :: prds))
		   | L (c, LEFT r) => splt (c, r :: unprds, prds)
		   | R (RIGHT l, c) => splt (c, unprds, l :: prds)
		   | _ => raise Fail "error"
		(* end case *))
	    val (unprds, prds) = splt (revCtx (L (c, LEFT (Leaf unprd))), nil, nil)
	in
	    (ropeOfRopes unprds, ropeOfRopes prds)
	end
*)

  end
