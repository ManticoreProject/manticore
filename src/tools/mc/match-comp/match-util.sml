(* match-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *)

structure MatchUtil : sig

  (* return the variables bound in a pattern *)
    val varsOfPat : AST.pat -> Var.Set.set

  (* return true if a pattern is a "simple" pattern *)
    val isSimplePat : AST.pat -> bool

  (* return true if a list of matches are all simple *)
    val areSimpleMatches : AST.match list -> bool

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

    fun isVarOrWild (AST.VarPat _) = true
      | isVarOrWild (AST.WildPat _) = true
      | isVarOrWild _ = false

    fun singletonDC dc = let
	  val Types.DataTyc{nCons, ...} = DataCon.ownerOf dc
	  in
	    !nCons = 1
	  end

    fun isSimplePat (AST.ConPat(dc, _, p)) = let
	  val Types.DataTyc{nCons, ...} = DataCon.ownerOf dc
	  in
	    singletonDC dc andalso isVarOrWild p
	  end
      | isSimplePat (AST.TuplePat ps) = List.all isVarOrWild ps
      | isSimplePat (AST.VarPat _) = true
      | isSimplePat (AST.WildPat _) = true
      | isSimplePat (AST.ConstPat(AST.DConst(dc, _))) = singletonDC dc
      | isSimplePat _ = false

    fun areSimpleMatches ms = let
	  fun f (AST.PatMatch(p, _)) = isSimplePat p
	    | f _ = false
	  in
	    List.all f ms
	  end

  end
