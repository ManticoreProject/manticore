(* zipper1.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A Zipper for ropes. The "1" in Zipper1 comes from the fact that elements on either side of
 * the hole have the same type.
 * 
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

structure Zipper1 =
  struct

    structure RTy = RopeTy
    datatype rope = datatype RTy.rope
    val append = RTy.appendWithoutBalancing
    val empty = RTy.empty
    val ropeOfRopes = RTy.ropeOfRopes

  (* the context representation of a rope *)
  (*   - Top represents the top of the rope, i.e., the location encompassing the whole rope *)
  (*   - L (c, r) represents the left part of a Cat in which r was the node in the  *)
  (*     right part, c is the context of the parent Cat *)
  (*   - R (c, r) similar to L (c, r) *)
    datatype 'a ctx
      = Top
      | L of 'a ctx * 'a rope
      | R of 'a rope * 'a ctx

  (* rope location, e.g., (rp, c), where rp is the node in focus and c is the context around the focus *)
    type 'a loc = 'a rope * 'a ctx

    fun left loc =
	(case loc
	  of (Leaf _, _) => NONE
	   | (Cat (_, _, l, r), c) => SOME (l, L (c, r))
	(* end case *))

    fun right loc =
	(case loc
	  of (Leaf _, _) => NONE
	   | (Cat (_, _, l, r), c) => SOME (r, R (l, c))
	(* end case *))

    fun up loc =
	(case loc
	  of (_, Top) => NONE
	   | (rp, L (c, r)) => SOME (RTy.appendWithoutBalancing (rp, r), c)
	   | (rp, R (l, c)) => SOME (RTy.appendWithoutBalancing (l, rp), c)
	(* end case*))

    fun leftmost loc =
	(case left loc
	  of NONE => loc
	   | SOME loc' => leftmost loc'
	(* end case *))

    fun upmost loc =
	(case up loc
	  of NONE => loc
	   | SOME loc' => upmost loc'
	(* end case *))

    fun upUntilLThenRight loc =
	(case loc
	  of (rp, L (c, r)) => SOME (r, R (rp, c))
	   | _ => (case up loc
		    of NONE => NONE
		     | SOME loc' => upUntilLThenRight loc'
		  (* end case *))
	(* end case *))

    fun input rp = (rp, Top)

    fun output loc =
	let val (rp, _) = upmost loc
	in
	    rp
	end

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
		(case (c, lr, rr) 
		  of (Top, lr, rr) => (lr, append (rr, Leaf prd))
		   | (L (c, r), lr, rr) => lp (c, append (r, lr), rr)
		   | (R (l, c), lr, rr) => lp (c, lr, append (rr, l))
		(* end case *))
	in
	    lp (revCtx (L (c, Leaf unprd)), empty (), empty ())
	end

(*
alternative approach that tries to balance both result ropes
    fun split (unprd, prd, c) =
	let
	    fun splt (c, unprds, prds) =
		(case c
		  of Top => (unprds, List.rev (Leaf prd :: prds))
		   | L (c, r) => splt (c, r :: unprds, prds)
		   | R (l, c) => splt (c, unprds, l :: prds)
		(* end case *))
	    val (unprds, prds) = splt (revCtx (L (c, Leaf unprd)), nil, nil)
	in
	    (ropeOfRopes unprds, ropeOfRopes prds)
	end

*)

  end
