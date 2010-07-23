(* match-compile.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Two things:
 * - a simple pattern irrefutability check, and 
 * - pattern match checking, following Maranget (JFP 2007).
 *
 * The irrefutability check and the pattern-match checking don't interact;
 *   they're together since they are thematically related.
 *
 * The irrefutability check is a simple common-sense check of individual patterns.
 *
 * The pattern match check follows Maragnet (JFP 2007) (also adapted in ocamlc).
 * 
 * Maranget's work considers *checking* for pattern redundancy and exhaustiveness
 *   as its own phase in compilation, as opposed to an integral part of some other
 *   phase (such as pattern-match compilation).
 *
 * Maranget's model pattern language consists of 
 * - constructors with (0 or more) arguments,
 * - wildcards, and
 * - or patterns.
 * The present code is adapted to account for tuple patterns and variables.
 *
 * All constants are treated as nullary constructors.
 *
 * Tuple patterns are treated here as constructor patterns, where
 *  all tuples of a given arity are assumed to share a constructor.
 *  That is, a pair (a, b) is thought of as belonging to the type
 *    datatype ('a, 'b) tup2 = Tup2 of 'a * 'b
 *  and then the constructor pattern rules apply.
 *
 * Variable patterns are treated as wildcards, which, for checking purposes, 
 *   are equivalent.
 *
 * There are no or patterns in PML, so no or patterns here either.
 *)

structure MatchCheck (* : sig

    val irrefutable  : AST.pat -> bool
    val irrefutableP : AST.ppat -> bool

    val check : match list -> bool

  end *) = struct

(* count the number of constructors in a datatype *)
  fun nConsOf (t: Types.tycon) : int = 
    (case t
      of Types.Tyc {def, ...} =>
           (case def
	     of Types.AbsTyc => raise Fail "abstract type"
	      | Types.DataTyc {nCons, ...} => !nCons
	    (* end case *))
        (* end case *))

(* a test to check if constructor c is the only one in its datatype *)
  fun soleConstructor (c: AST.dcon) : bool = 
    (nConsOf (DataCon.ownerOf c) = 1)

(* a pattern is irrefutable if *)
(* - it is a constructor pattern K(p) where *)
(*   - K is the sole constructor in its datatype, and *)
(*   - p is irrefutable, *)
(* - it is a tuple of irrefutable patterns, *)
(* - it is a variable, *)
(* - it is a wildcard, or *)
(* - it is a nullary constructor pattern K where *)
(*   - K is the sole constructor in its datatype, or *)
(* - it is unit. *)
  fun irrefutable (AST.ConPat (c, ts, p)) =
        soleConstructor c andalso irrefutable p
    | irrefutable (AST.TuplePat ps) = List.all irrefutable ps
    | irrefutable (AST.VarPat _) = true
    | irrefutable (AST.WildPat _) = true
    | irrefutable (AST.ConstPat (AST.DConst (c, _))) = 
        soleConstructor c
    | irrefutable (AST.ConstPat (AST.LConst (lit, ty))) =
        TypeUtil.same (ty, Basis.unitTy)
       
(* irrefutability for parallel patterns *)
  fun irrefutableP (AST.NDWildPat _) = true
    | irrefutableP (AST.HandlePat _) = raise Fail "not supported: HandlePat"
        (* A handlePat is irrefutable in certain cases... *)
        (* for example if the expression is raise E and the pattern is *)
        (*   handle E... *)
        (* but in general a HandlePat will not be irrefutable. *)
        (* Leaving this alone for now. *)
    | irrefutableP (AST.Pat p) = irrefutable p

  structure DConSet = RedBlackSetFn (struct
				       type ord_key = AST.dcon
				       val compare  = DataCon.compare
				     end)

(* select the pattern out of a match *)
  fun patOf (AST.PatMatch (p, _)) = p
    | patOf (AST.CondMatch (p, _, _)) = p

  type patlist = AST.pat list

(* A patmat (pattern matrix) is a regular (non-jagged) matrix of patterns. *) 
(* Invariant 1: regularity -- all rows have the same width. *)
(* Invariant 2: no variables -- wildcards in their place.   *)
(* The invariants must be maintained by smart constructors, etc. *)
  type patmat = AST.pat list list 

  fun isTuplePat (AST.TuplePat _) = true
    | isTuplePat _ = false

(* elimVarPat replaces all variables in a pattern with *)
(*   appropriately-typed wildcard patterns. *)
(* Wildcards are sufficient for checking pattern "usefulness" (Maranget's term). *)
  fun elimVarPat (AST.ConPat (c, ts, p)) = AST.ConPat (c, ts, elimVarPat p)
    | elimVarPat (AST.TuplePat ps) = AST.TuplePat (List.map elimVarPat ps)
    | elimVarPat (p as AST.VarPat _) = AST.WildPat (TypeOf.pat p)
    | elimVarPat (p as AST.WildPat _)  = p
    | elimVarPat (p as AST.ConstPat _) = p

(* mkPatMat is a smart constructor for pattern matrices. *)
(* A patmat is built from a match list as follows. *)
(* If you select the patterns only out of a match list, you will have *)
(* A) a list of conpats with vars, wilds, and dconsts, *)
(* B) a list of tuples of some fixed arity n with vars and wilds, or *)
(* C) a list of consts, vars, and wilds. *)
(* In cases A and C, the patmat is of width 1, the list of *)
(*   singleton lists of patterns. *)
(* In case B, *)
(* - all tuple patterns are split into their n subpatterns, and   *)
(* - all vars and wilds generate n wilds of the appropriate types. *)
  fun mkPatMat (ms: AST.match list) : patmat = let
    val ps = List.map (elimVarPat o patOf) ms
    in
      case ms 
       of [] => raise Fail "empty match list"
	| _ => if List.exists isTuplePat ps 
	       then tups ps
	       else List.map (fn p => [p]) ps
    end
(* tups takes two passes over the given pattern list: *)
(* the first pass gets the tuple type of all the patterns, and verifies *)
(*   that the tuple type is shared among all patterns *)
(*   (which is probably not strictly necessary) *)
(* and the second pass builds the corresponding pattern matrix as *)
(*   described in the previous comment (case B). *)
  and tups (ps: AST.pat list) : patmat = let
    fun getTypes ([], SOME (Types.TupleTy ts)) = ts
      | getTypes ([], SOME _) = raise Fail "BUG: expected tuple ty"
      | getTypes ([], NONE) = raise Fail "BUG"
      | getTypes ((p as AST.TuplePat _)::t, NONE) = 
          getTypes (t, SOME (TypeOf.pat p))
      | getTypes ((p as AST.TuplePat _)::t, SOME ty) = 
          if TypeUtil.same (TypeOf.pat p, ty)
          then getTypes (t, SOME ty)
	  else raise Fail "ill-typed match list"
      | getTypes ((p as AST.WildPat ty)::t, NONE) = 
          getTypes (t, SOME ty)
      | getTypes ((p as AST.WildPat ty')::t, SOME ty) = 
          if TypeUtil.same (ty', ty)
	  then getTypes (t, SOME ty)
	  else raise Fail "ill-typed match list"
      | getTypes ((p as AST.VarPat _)::t, _) =
	  (* VarPats should have been replaced by WildcardPats *)
          raise Fail "BUG: unexpected VarPat"
      | getTypes (AST.ConPat _::_, _)   = 
          (* ConPats should not occur in the same match list as TuplePats *)
          raise Fail "BUG: unexpected ConPat"
      | getTypes (AST.ConstPat _::_, _) = 
          (* ConstPats should not occur in the same match list as TuplePats *)
          raise Fail "BUG: unexpected ConstPat"
    val wilds = List.map AST.WildPat (getTypes (ps, NONE))
    fun trx ([], acc) = List.rev acc
      | trx (AST.TuplePat(ps)::t, acc) = trx (t, ps::acc)
      | trx (AST.WildPat(_)::t, acc)   = trx (t, wilds::acc)
      | trx (AST.VarPat(_)::t, acc) = raise Fail "BUG: unexpected VarPat"
      | trx (AST.ConPat(_)::_, _)   = raise Fail "BUG: unexpected ConPat"
      | trx (AST.ConstPat(_)::_, _) = raise Fail "BUG: unexpected ConstPat"
    in
      trx (ps, [])
    end

(*
  fun mkPatMatP ([]: AST.pmatch list) : patmat  = raise Fail "empty pmatch list"
    | mkPatMatP (ms as m::_) = let
        fun ppat (AST.NDWildPat ty) = AST.WildPat ty
	  | ppat (AST.HandlePat _) = raise Fail "not supported: HandlePat"
	  | ppat (AST.Pat p) = p
        val ty = 
          (case m 
	     of AST.PMatch (ps, _) => Types.TupleTy (List.map TypeOf.pat ...
        fun pmatch (AST.PMatch (ps, e)) = let
              val ps' = List.map ppat ps
              in
                AST.Match (AST.TuplePat ps', e)
              end
	  | pmatch (AST.Otherwise e) = AST.Match (AST.WildPat ty, e)
	val ms' = List.map pmatch ms 
        in
          mkPatMat ms'
        end
*)

(* dim returns the dimensions of a pattern matrix. *)
(* It relies on the invariant that a patmat has uniform width at all rows. *)
(* The two values returned are always nonnegative. *)
  fun dim (p: patmat) : {width: int, length: int} = let
    val w = List.length (List.nth (p, 0))
    val n = List.length p
    in
      {width=w, length=n}
    end

(* complete is a predicate to test whether a set of data constructors *) 
(*   comprises the *whole* set of data constructors for that datatype *)
  fun complete (s: DConSet.set) : bool = 
    (case DConSet.listItems s
       of [] => false
        | c::_ => (nConsOf (DataCon.ownerOf c)) = (DConSet.numItems s)
      (* end case *))

(* firstColCons builds a set of all constructors appearing in *)
(*   the first (leftmost) column of a pattern matrix. *)
  fun firstColCons (p: patmat) : DConSet.set = let
    fun lp ([], acc) = acc
      | lp ((p::ps)::pss, acc) = 
          (case p
	    of AST.ConPat (c, _, _) => lp (pss, DConSet.add (acc, c))
	     | _ => lp (pss, acc))
      | lp ([]::pss, acc) = raise Fail "malformed pattern matrix"
	     
    in
      lp (p, DConSet.empty)
    end

  val mkWildPat: AST.pat -> AST.pat = AST.WildPat o TypeOf.pat

(* conWilds consumes a constructor and produces 0 or 1 wildcards as follows: *)
(* - if the constructor is nullary, it returns the empty list *)
(* - if the constructor is non-nullary, it returns the singleton list of *)
(*     a WildPat of the constructor's argument type *)
  fun conWilds (c: AST.dcon) : AST.pat list = 
    (case c
      of Types.DCon {argTy, ...} =>
           (case argTy
	      of NONE => []
	       | SOME t => [AST.WildPat t]
	     (* end case *))
           (* end case *))

(* s computes the "specialized matrix" as per Maranget P. 393. *)
  fun s (c: AST.dcon, p: patmat) : patmat = let
    fun lp ([], acc) = List.rev acc
      | lp ((AST.ConPat (c', ts, p')::ps)::pss, acc) = 
          if DataCon.same (c, c') then
            (case p'
	      of AST.VarPat _   => raise Fail "unexpected VarPat in pattern matrix"
	       | _  => lp (pss, (p'::ps)::acc)
             (* end case *))
	  else lp (pss, acc)
      | lp ((AST.TuplePat(qs)::ps)::pss, acc) =
          (* TuplePats are considered ConPats not matching the constructor c *)
          lp (pss, acc)
      | lp ((AST.VarPat(_)::ps)::pss, acc) = 
          raise Fail "unexpected VarPat in pattern matrix"
      | lp (((w as AST.WildPat _)::ps)::pss, acc) = lp (pss, (w::ps)::acc)
      | lp ((AST.ConstPat(k)::ps)::pss, acc) = 
          (case k
	    of AST.DConst (c', ts) => if DataCon.same (c, c') 
				      then lp (pss, ps::acc)
				      else lp (pss, acc)
	     | AST.LConst _ => lp (pss, acc)
	   (* end case *))
      | lp ([]::_, _) = raise Fail "?"
    in
      lp (p, [])
    end

(* sK is a version of s to compute the "specialized matrix" from a constant. *)
(* Constants are considered nullary constructors. *)
  fun sK (AST.DConst (c, ts), p: patmat) : patmat = let
        fun lp ([], acc) = List.rev acc
	  | lp ((q::ps)::pss, acc) = 
             (case q 
	       of AST.ConstPat (AST.DConst (c', _)) =>
                    if DataCon.same (c, c') 
		    then lp (pss, ps::acc)
		    else lp (pss, acc)
		| AST.WildPat _ => lp (pss, ps::acc)
		| AST.VarPat _ => raise Fail "unexpected VarPat in pattern matrix"
		| _ => lp (pss, acc)
	      (* end case *))
	  | lp ([]::_, acc) = raise Fail "?"
        in
          lp (p, [])
        end
    | sK (AST.LConst (l, t), p) = let
        fun lp ([], acc) = List.rev acc
	  | lp ((q::ps)::pss, acc) = 
              (case q
		of AST.ConstPat (AST.LConst (l', t')) =>
                     if TypeUtil.same (t, t') andalso Literal.same (l, l')
		     then lp (pss, ps::acc)
		     else lp (pss, acc)
		 | AST.WildPat _ => lp (pss, ps::acc)
		 | AST.VarPat _ => raise Fail "unexpected VarPat in pattern matrix"
		 | _ => lp (pss, acc)
	       (* end case *))
	  | lp ([]::_, _) = raise Fail "?"
        in
	  lp (p, [])
	end

(* sT is a version of s to compute the "specialized matrix" from a tuples. *)
(* Tuples are considered to have "invisible" constructors, that are the    *)
(*   sole constructors of their type. *)
(* The arity of all tuples in the first column of p is checked *)
(*   (though it should never vary). *)
  fun sT (arity: int, p: patmat) : patmat = let
    fun lp ([], acc) = List.rev acc
      | lp ((q::ps)::pss, acc) = 
          (case q
	    of AST.TuplePat rs => 
                 if List.length rs = arity 
		 then lp (pss, (rs@ps)::acc)
		 else raise Fail "BUG: tuple arity"
	     | AST.WildPat (Types.TupleTy ts) => let
                 val wilds = List.map AST.WildPat ts
                 in
		   lp (pss, (wilds@ps)::acc)
	         end
	     | AST.WildPat t => 
                 raise Fail "unexpected WildPat with non-tuple type"
	     | AST.VarPat _ =>
                 raise Fail "unexpected VarPat in pattern matrix"
	     | AST.ConPat _ =>
                 (* ConPats shouldn't be in the same column as tuples *)
                 raise Fail "unexpected ConPat in pattern matrix"
	     | AST.ConstPat _ =>
                 (* ConstPats shouldn't be in the same column as tuples *)
                 raise Fail "unexpected ConstPat in pattern matrix"
	   (* end case *))
      | lp ([]::_, _) = raise Fail "?"
    in
      lp (p, [])
    end

(* d computes the "default matrix" as per Maranget p. 394. *)
(* For given p of width n, the output patmat has width (n-1). *)
  fun d (p: patmat) : patmat = let
    fun lp ([], acc) = List.rev acc
      | lp ((AST.ConPat(_)::ps)::t, acc) = lp (t, acc)
      | lp ((AST.WildPat(_)::ps)::t, acc) = lp (t, ps::acc)
      | lp ((AST.TuplePat(_)::ps)::t, acc) =
          (* TuplePats are treated as ConPats *)
          lp (t, acc)
      | lp ((AST.VarPat(_)::_)::_, _) = 
          raise Fail "unexpected VarPat in pattern matrix"
      | lp ((AST.ConstPat(_)::_)::t, acc) = lp (t, acc)
      | lp ([]::_, _) = raise Fail "?"
    in
      lp (p, [])
    end

(* u tests whether pattern vector v is useful with respect to patmat p. *)
(*  invariant: width of p equals width of v (checked) *)
  fun u (p: patmat, v: patlist) : bool = let
    val {width, length} = dim p
    val chk = if width = List.length v then () 
	      else raise Fail "unequal widths"
    in
      if (width = 0) then (length = 0) (* note length cannot be negative *)
      else let
        val (q, qs) =
          (case v 
	    of h::t => (h, t)
	     | nil => raise Fail "broken invariant")
        in
	  case q
	   of AST.ConPat (c, ts, r) => u (s (c, p), r::qs)
	    | AST.ConstPat k => u (sK (k, p), qs)
	    | AST.VarPat x => raise Fail "unexpected VarPat in pattern matrix"
	    | AST.WildPat ty => wild (p, qs)
	    | AST.TuplePat rs => let
                val arity = List.length rs 
                in 
		  u (sT (arity, p), rs@qs)
                end
	end
    end
  and wild (p: patmat, qs: patlist) = let
    val sigma = firstColCons p
    in
      if complete sigma then let
        val cs = DConSet.listItems sigma
        fun u' c = u (s (c, p), conWilds(c) @ qs)
        in
          List.exists u' cs
        end        
      else
        u (d p, qs)
    end

(* exhaustive *)
  fun exhaustive (p: patmat) : bool =
    (case p
       of [] => false
	| ps::pss => let
            val ts = List.map TypeOf.pat ps
            val ws = List.map AST.WildPat ts
            in
              (* the patlist of all wildcards should not be useful wrt p *)
	      not (u (p, ws))
	    end
      (* end case *))

(* irredundant *)
(* Checks that each row of p is useful with respect to its predecessors. *)
  fun irredundant (p: patmat) : bool = 
   (case p
     of [] => (* vacuously *) true
      | v::[] => true
      | v::vs => let
          fun lp ([], _) = true
	    | lp (ps::pss, m) = u (m, ps) andalso lp (pss, m@[ps])
          in
            lp (vs, [v]) 
	  end
    (* end case *))

  fun checkMatchList (ms: AST.match list) : unit = let
    val p = mkPatMat ms
    in
      if not (exhaustive p) then raise Fail "inexhaustive"
      else if not (irredundant p) then raise Fail "redundant"
      else ()
    end

(*
  fun checkPMatchList (ms: AST.pmatch list) : unit = let
    val 
*)

end
