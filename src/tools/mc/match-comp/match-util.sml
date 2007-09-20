(* match-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *)

structure MatchUtil : sig

  (* return the variables bound in a pattern *)
    val varsOfPat : AST.pat -> Var.Set.set

  end = struct

    structure VSet = Var.Set

    fun varsOfPat pat = let
	  fun analyse (AST.ConPat(_, _, p), vs) = analyse (p, vs)
	    | analyse (AST.TuplePat ps, vs) = analyseList(ps, vs)
	    | analyse (AST.VarPat x, vs) = VSet.add(vs, x)
	    | analyse (AST.WildPat _, vs) = vs
	    | analyse (AST.ConstPat _, vs) = vs
	  and analyseList ([], vs) = vs
	    | analyseList (pat::pats, vs) = analyseList(pats, analyse(pat, vs))
	  in
	    analyse (pat, VSet.empty)
	  end

  end
