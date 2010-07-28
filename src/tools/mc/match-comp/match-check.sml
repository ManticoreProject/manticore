(* match-compile.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Original implementation of this module by Adam Shaw.
 *
 * This module provides wo things:
 * - a simple pattern irrefutability check, and 
 * - pattern match checking, following Maranget (JFP 2007).
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
 *
 * TODO: Beginning in section 5 of Maranget, there is a description of how to 
 *   adapt the algorithm here so it not only checks for redundancy and 
 *   inexhaustiveness, but also produces witnesses to its findings.
 *   I have not implemented those adaptations here.
 *)

(* TODO: Errors should not all be raise Fail. *)

structure MatchCheck (* : sig

    val irrefutable  : AST.pat -> bool
    val irrefutableP : AST.ppat -> bool
    val checkExp     : AST.exp -> unit

  end *) = struct

(* Bugs are things that *really* shouldn't happen, in the sense they will only
 * occur if the code itself is flawed or some other part of the compiler (i.e.
 * the typechecker) isn't doing its job.
 *)
  fun bug msg = raise Fail msg

  fun errRedundant msg  = raise Fail "todo"
  fun warnInexMatch msg = raise Fail "todo"
  fun warnInexBind msg  = raise Fail "todo"

  structure DConSet = RedBlackSetFn (struct
				       type ord_key = AST.dcon
				       val compare  = DataCon.compare
				     end)
  fun dconSetFromList cs = DConSet.addList (DConSet.empty, cs)

  structure LitSet = RedBlackSetFn (struct
				      type ord_key = Literal.literal
				      val compare  = Literal.compare
				    end)
  fun litSetFromList lits = LitSet.addList (LitSet.empty, lits)

  type typed_lit_set = Types.ty * LitSet.set

  val unitLitSet = litSetFromList [Literal.unitLit]
  val boolLitSet = litSetFromList [Literal.trueLit, Literal.falseLit]

(* count the number of constructors in a datatype *)
  val nConsOf : Types.tycon -> int = 
    (fn Types.Tyc {def, ...} =>
      (case def
	 of Types.AbsTyc => bug "trying to count the constructors in an abstract type"
	  | Types.DataTyc {nCons, ...} => !nCons
	(* end case *))
      (* end fn *))

(* return the set of constructors for a datatype *)
  val allCons : Types.tycon -> DConSet.set =
    (fn Types.Tyc {def, ...} =>
      (case def
	 of Types.AbsTyc => DConSet.empty
	  | Types.DataTyc {cons, ...} => dconSetFromList (!cons)
        (* end case *))
      (* end fn *))

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
  fun irrefutable (AST.VarPat _) = true
    | irrefutable (AST.WildPat _) = true
    | irrefutable (AST.TuplePat ps) = List.all irrefutable ps
    | irrefutable (AST.ConPat (c, _, p)) =
        soleConstructor c andalso irrefutable p
    | irrefutable (AST.ConstPat (AST.DConst (c, _))) = 
        soleConstructor c
    | irrefutable (AST.ConstPat (AST.LConst (lit, ty))) =
        TypeUtil.same (ty, Basis.unitTy)
       
(* irrefutability for parallel patterns *)
  fun irrefutableP (AST.NDWildPat _) = true
    | irrefutableP (AST.HandlePat _) = false (* HandlePats are refutable. *)
    | irrefutableP (AST.Pat p) = irrefutable p

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
       of [] => bug "empty match list"
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
      | getTypes ([], SOME _) = bug "expected tuple ty"
      | getTypes ([], NONE) = bug "no type found"
      | getTypes ((p as AST.TuplePat _)::t, NONE) = 
          getTypes (t, SOME (TypeOf.pat p))
      | getTypes ((p as AST.TuplePat _)::t, SOME ty) = 
          if TypeUtil.same (TypeOf.pat p, ty)
          then getTypes (t, SOME ty)
	  else bug "tups (TuplePat): ill-typed match list"
      | getTypes ((p as AST.WildPat ty)::t, NONE) = 
          getTypes (t, SOME ty)
      | getTypes ((p as AST.WildPat ty')::t, SOME ty) = 
          if TypeUtil.same (ty', ty)
	  then getTypes (t, SOME ty)
	  else bug "tups (WildPat): ill-typed match list"
      | getTypes ((p as AST.VarPat _)::t, _) =
	  (* VarPats should have been replaced by WildcardPats *)
          bug "tups: unexpected VarPat"
      | getTypes (AST.ConPat _::_, _)   = 
          (* ConPats should not occur in the same match list as TuplePats *)
          bug "tups: unexpected ConPat"
      | getTypes (AST.ConstPat _::_, _) = 
          (* ConstPats should not occur in the same match list as TuplePats *)
          bug "tups: unexpected ConstPat"
    val wilds = List.map AST.WildPat (getTypes (ps, NONE))
    fun trx ([], acc) = List.rev acc
      | trx (AST.TuplePat(ps)::t, acc) = trx (t, ps::acc)
      | trx (AST.WildPat(_)::t, acc)   = trx (t, wilds::acc)
      | trx (AST.VarPat(_)::t, acc) = bug "trx: unexpected VarPat"
      | trx (AST.ConPat(_)::_, _)   = bug "trx: unexpected ConPat"
      | trx (AST.ConstPat(_)::_, _) = bug "trx: unexpected ConstPat"
    in
      trx (ps, [])
    end

(* dim returns the dimensions of a pattern matrix. *)
(* It relies on the invariant that a patmat has uniform width at all rows. *)
(* The two values returned are always nonnegative. *)
  fun dim (p: patmat) : {width: int, length: int} = 
    (case p
       of [] => {width=0, length=0}
	| ps::pss => let
            val w = List.length ps
            val n = List.length p
            in
              {width=w, length=n}
            end
      (* end case *))

(* completeCons is a predicate to test whether a set of data constructors *) 
(*   comprises the *whole* set of data constructors for that datatype *)
  fun completeCons (s: DConSet.set) : bool = 
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
	     | AST.ConstPat (AST.DConst (c, _)) => lp (pss, DConSet.add (acc, c))
	     | _ => lp (pss, acc))
      | lp ([]::pss, acc) = bug "firstColCons: malformed pattern matrix"
    in
      lp (p, DConSet.empty)
    end

(* completeLits tests, for the unit and bool types, whether the literal set *)
(*   is complete. *)
(* All other literal sets are judged incomplete. *)
  fun completeLits ((ty, s): typed_lit_set) : bool = 
    if TypeUtil.same (Basis.unitTy, ty) then
      LitSet.equal (unitLitSet, s)
    else if TypeUtil.same (Basis.boolTy, ty) then
      LitSet.equal (boolLitSet, s)
    else false

(* firstColContainsLits is a predicate to test whether the first column *)
(*   of a pattern matrix contains literals. *)
  val firstColContainsLits : patmat -> bool = let
    fun pred (AST.ConstPat (AST.LConst _)::_) = true
      | pred _ = false
    in
      List.exists pred
    end

(* firstColLits builds a set of all literals appearing in *)
(*   the first (leftmost) column of a pattern matrix. *)
  fun firstColLits (p: patmat) : typed_lit_set = let
    fun lp ([], SOME ty, acc) = (ty, acc)
      | lp ([], NONE, acc) = bug "firstColLits: no type?"
      | lp ((p::ps)::pss, optTy, acc) = 
          (case p
	    of AST.ConstPat (AST.LConst (lit, ty)) =>
                 (case optTy
		    of NONE => lp (pss, SOME ty, LitSet.add (acc, lit))
		     | SOME _ => lp (pss, optTy, LitSet.add (acc, lit)))
	     | _ => lp (pss, optTy, acc))
      | lp ([]::_, _, _) = bug "firstColLits: malformed pattern matrix"
    in
      lp (p, NONE, LitSet.empty)
    end
	     
(* mkWildPat consumes a pattern p and constructs a wildcard pattern with p's type. *)
(* This is the identity operation on wildcard patterns.  *)
(* For example, mkWildPat(true) --> (_ : bool) *)
  fun mkWildPat (w as AST.WildPat _) = w
    | mkWildPat p = (AST.WildPat o TypeOf.pat) p

(* conWilds consumes a constructor and produces 0 or 1 wildcards as follows: *)
(* - if the constructor is nullary, it returns the empty list *)
(* - if the constructor is non-nullary, it returns the singleton list of *)
(*     a WildPat of the constructor's argument type *)
(* Returning a list instead of an option enables use of @ with the result later. *)
  val conWilds : AST.dcon -> AST.pat list = 
    (fn Types.DCon {argTy, ...} =>
      (case argTy
	 of NONE => []
	  | SOME t => [AST.WildPat t]
        (* end case *))
      (* end fn *))

(* s computes the "specialized matrix" as per Maranget P. 393. *)
  fun s (c: AST.dcon, p: patmat) : patmat = let
    fun lp ([], acc) = List.rev acc
      | lp ((AST.ConPat (c', ts, p')::ps)::pss, acc) = 
          if DataCon.same (c, c') then
            (case p'
	      of AST.VarPat _   => bug "s (ConPat): unexpected VarPat in pattern matrix"
	       | _  => lp (pss, (p'::ps)::acc)
             (* end case *))	  else lp (pss, acc)
      | lp ((AST.TuplePat(qs)::ps)::pss, acc) =
          (* TuplePats are considered ConPats not matching the constructor c *)
          lp (pss, acc)
      | lp ((AST.VarPat(_)::ps)::pss, acc) = 
          bug "s (VarPat): unexpected VarPat in pattern matrix"
      | lp (((w as AST.WildPat _)::ps)::pss, acc) = lp (pss, (w::ps)::acc)
      | lp ((AST.ConstPat(k)::ps)::pss, acc) = 
          (case k
	    of AST.DConst (c', ts) => if DataCon.same (c, c') 
				      then lp (pss, ps::acc)
				      else lp (pss, acc)
	     | AST.LConst _ => lp (pss, acc)
	   (* end case *))
      | lp ([]::_, _) = bug "s: malformed pattern matrix"
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
		| AST.VarPat _ => bug "sK: unexpected VarPat in pattern matrix"
		| _ => lp (pss, acc)
	      (* end case *))
	  | lp ([]::_, acc) = bug "sK: malformed pattern matrix"
        in
          lp (p, [])
        end
    | sK (AST.LConst (l, t), p) = sL (l, t, p)
(* sL is a version of s to compute the "specialized matrix" from a literal. *)
(* It is written this way (as a separate function) since it's useful to call *)
(*   it directly (see below) and not just as a branch of sK. *)
  and sL (l, t, p) = let
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
    in
      if (width = 0) then (length = 0) (* note length cannot be negative *)
      else let
        val chk = let
	  val vlen = List.length v 
	  in
            if width = vlen then () 
	    else let
              val msg = "pat matrix width = " ^ Int.toString width ^
			", vector width = " ^ Int.toString vlen
              in
                raise Fail ("unequal widths: " ^ msg)
              end
          end
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
  and wild (p: patmat, qs: patlist) =
    (* there are two cases to deal with: *)
    (* 1) the first column has ConPats and ConstPat (DConst _) pats in it, or *)
    (* 2) the first column has ConstPat (LConst ...) patterns in it *)
    if firstColContainsLits p then let
      val (ty, sigma) = firstColLits p
      in
        if completeLits (ty, sigma) then let
          val ls = LitSet.listItems sigma
          fun u' l = u (sL (l, ty, p), qs)
          in
            List.exists u' ls
	  end
        else
          u (d p, qs)
      end
    else let
      val sigma = firstColCons p
      in
        if completeCons sigma then let
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
(* TODO Rewrite for efficiency (currently it's quadratic). *)
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

  val inexhaustive = not o exhaustive
  val redundant    = not o irredundant

(* checkMatchList checks that the match list is both exhaustive and irredundant. *)
  fun checkMatchList (ms: AST.match list) : unit = let
    val p = mkPatMat ms
    in
      if inexhaustive p then raise Fail "inexhaustive"
      else if redundant p then raise Fail "redundant"
      else ()
    end

(* checkHandleMatches checks that the match list in a handle is not redundant. *)
(* It does not check for exhaustiveness. *)
  fun checkHandleMatches (ms: AST.match list) : unit = let
    val p = mkPatMat ms
    in
      if redundant p then raise Fail "redundant"
      else ()
    end

(* checkExp checks all match lists recursively in an expression. *)
  val checkExp : AST.exp -> unit = let
    fun exp (AST.LetExp (b, e)) = (binding b; exp e)
      | exp (AST.IfExp (e1, e2, e3, _)) = (exp e1; exp e2; exp e3)
      | exp (AST.CaseExp (e, ms, _)) = (exp e; checkMatchList ms)
      | exp (AST.PCaseExp (es, ms, _)) = List.app exp es
          (* note: pcase matches are not checked, other than that *)
          (* they are typechecked elsewhere *)
      | exp (AST.HandleExp (e, ms, _)) = (exp e; checkHandleMatches ms)
          (* note: handle patterns are checked for redundancy only *)
      | exp (AST.RaiseExp (e, _)) = exp e
      | exp (AST.FunExp (_, e, _)) = exp e
      | exp (AST.ApplyExp (e1, e2, _)) = (exp e1; exp e2)
      | exp (AST.VarArityOpExp _) = ()
      | exp (AST.TupleExp es) = List.app exp es
      | exp (AST.RangeExp (e1, e2, optE, _)) = 
          (exp e1; exp e2; Option.app exp optE)
      | exp (AST.PTupleExp es) = List.app exp es
      | exp (AST.PArrayExp (es, _)) = List.app exp es
      | exp (AST.PCompExp (e, pes, optE)) = 
          (exp e; List.app (fn (p,e) => exp e) pes; Option.app exp optE)
      | exp (AST.PChoiceExp (es, _)) = List.app exp es
      | exp (AST.SpawnExp e) = exp e
      | exp (AST.ConstExp _) = ()
      | exp (AST.VarExp _) = ()
      | exp (AST.SeqExp (e1, e2)) = (exp e1; exp e2)
      | exp (AST.OverloadExp _) = ()
      | exp (AST.ExpansionOptsExp (_, e)) = exp e
  and binding (AST.ValBind (p, e)) = (pat p; exp e)
    | binding (AST.PValBind (p, e)) = (pat p; exp e)
    | binding (AST.FunBind fs) = lambdas fs
    | binding (AST.PrimVBind _) = ()
    | binding (AST.PrimCodeBind _) = ()
  and pat p = if irrefutable p then () else raise Fail "refutable pat in binding"
  and lambdas fs = List.app (fn AST.FB (_, _, e) => exp e) fs
  in
    exp
  end

end
