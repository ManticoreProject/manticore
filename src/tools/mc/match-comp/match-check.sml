(* match-compile.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Original implementation of this module by Adam Shaw.
 *
 * This module provides two things:
 * - a simple pattern irrefutability check, and 
 * - pattern match checking, following Maranget (JFP 2007).
 *
 * The irrefutability check is a simple common-sense check of individual patterns.
 *
 * The pattern match check follows Maranget (JFP 2007) (also adapted in ocamlc).
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

(* TODO (maybe): HandlePats are currently not supported.
 *)

(* FIXME Bogus locations for errors (this issue exists elsewhere too, e.g.,
 *   match-compile).
 *)

structure MatchCheck (* : sig

    val irrefutable  : AST.pat -> bool
    val irrefutableP : AST.ppat -> bool
    val checkExp     : AST.exp -> unit

  end *) = struct

(* bug : string -> string -> 'a (raises an exn)
 * Bugs are things that *really* shouldn't happen, in the sense they will only
 * occur if the code here is flawed or some other part of the compiler (e.g.
 * the typechecker) isn't doing its job.
 *)
  fun bug whence msg = raise Fail ("BUG (" ^ whence ^"). " ^ msg)

(* println *)
  fun println s = (TextIO.print s; TextIO.print "\n")

(* types *)
  type patlist = AST.pat list

(* A patmat (pattern matrix) is a regular (non-jagged) matrix of patterns. *) 
(* Invariant 1: regularity -- all rows have the same width. *)
(* Invariant 2: no variables -- wildcards in their place.   *)
(* The invariants must be maintained by smart constructors, etc. *)
  type patmat = AST.pat list list 

(* pretty printers and related utilities *)

  val lower: string -> string = implode o List.map Char.toLower o explode    

  val isBool: Types.ty -> bool = (fn t => TypeUtil.same (Basis.boolTy, t))
  val isUnit: Types.ty -> bool = (fn t => TypeUtil.same (Basis.unitTy, t))

  val patToString : AST.pat -> string = let
    fun tos (AST.ConPat (c, ts, p)) =
          (case p
	     of AST.TuplePat _ => DataCon.nameOf c ^ tos p
	      | _ => DataCon.nameOf c ^ "(" ^ tos p ^ ")")
      | tos (AST.TuplePat ps) = 
          String.concat ["(", String.concatWith "," (List.map tos ps), ")"]
      | tos (AST.VarPat x) = Var.nameOf x
      | tos (AST.WildPat ty) = "_" (* "(_:" ^ TypeUtil.toString ty ^ ")" *)
      | tos (AST.ConstPat (AST.DConst (c, _))) = DataCon.nameOf c
      | tos (p as AST.ConstPat (AST.LConst (lit, ty))) = let
            val s = Literal.toString lit
            in
              if isBool ty then lower s 
	      else if isUnit ty then "()"
	      else s
            end
    in
      tos
    end

  fun patlistToString (ps: patlist) : string = 
    String.concatWith "\t" (List.map patToString ps)

  fun patmatToString (p: patmat) : string = let
    fun tos ([], acc) = String.concatWith "\n" (List.rev acc)
      | tos (ps::pss, acc) = let
          val s = patlistToString ps
          in
	    tos (pss, (s ^ "\t=> ...")::acc)
	  end
    in
      tos (p, [])
    end

(* error handling *)
  type err_stream = MatchErrors.err_stream

  val bogusLocation = (0,0) (* FIXME *)

  fun errRedundant errStrm msg = 
    (TextIO.print (msg ^ "\n");
     MatchErrors.errRedundantMatch (errStrm, bogusLocation))

  fun warnInexMatch errStrm msg = 
    (TextIO.print (msg ^ "\n");
     MatchErrors.warnNonexhaustiveMatch (errStrm, bogusLocation))

  fun warnInexBind errStrm p = let
    val pStr = patToString p
    in
      TextIO.print ("inexhaustive binding: pattern " ^ pStr ^ "\n");
      MatchErrors.warnNonexhaustiveBind (errStrm, bogusLocation)
    end

  structure DConSet = RedBlackSetFn (struct
				       type ord_key = AST.dcon
				       val compare  = DataCon.compare
				     end)

  structure LitSet = RedBlackSetFn (struct
				      type ord_key = Literal.literal
				      val compare  = Literal.compare
				    end)

  type typed_lit_set = Types.ty * LitSet.set

  fun dconSetFromList cs  = DConSet.addList (DConSet.empty, cs)
  fun litSetFromList lits = LitSet.addList (LitSet.empty, lits)

(* common constant sets *)
  val unitLitSet = litSetFromList [Literal.unitLit]
  val boolLitSet = litSetFromList [Literal.trueLit, Literal.falseLit]

(* count the number of constructors in a datatype *)
  val nConsOf : Types.tycon -> int = 
    (fn Types.Tyc {def, ...} =>
      (case def
	 of Types.AbsTyc => bug "nConsOf" "trying to count the constructors in an abstract type"
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

(* consOwner *)
  fun consOwner (Types.DCon {owner, ...}) = owner

(* dconSetOwner *)
  fun dconSetOwner (s: DConSet.set) : Types.tycon =
    (case DConSet.listItems s
       of [] => bug "dconSetOwner" "empty argument"
	| c::_ => consOwner c)

(* return the set of all unrepresented constructors for given datatype *)
  fun unrepresentedCons (s: DConSet.set) : DConSet.set =
    DConSet.difference (allCons (dconSetOwner s), s)

  datatype 'a num_dict 
    = NumDict of {min : 'a, 
		  max : 'a, 
		  zero: 'a,
		  add1: 'a -> 'a, 
		  sub1: 'a -> 'a,
		  cmp : 'a * 'a -> order}

  fun numDict (min, max, zero, add1, sub1, cmp) = 
    NumDict {min=min, max=max, zero=zero, add1=add1, sub1=sub1, cmp=cmp}

(* missingIntegral *)
(* pre: argument list is sorted ascending *)
(* Given a list of sorted (asc) integral numbers, return one that *)
(*   does not appear. *)
(* missingIntegral will probably return 1 less than the current min. *)
  fun missingIntegral (NumDict {zero, ...}) [] = SOME zero
    | missingIntegral (NumDict {min, max, add1, sub1, cmp, zero}) (ns as m::_) =
        (case cmp (min, m)
	   of LESS => SOME (sub1 m)
	    | _ => let (* in this case m = min *)
                (* look for a gap in the sorted list *)
		fun lp [n] =
                      (case cmp (n, max)
			 of LESS => SOME (add1 n)
			  | equal => NONE (* unlikely, but accounted for *))
		  | lp (m::n::ns) = 
                      (case cmp (add1 m, n)
                         of EQUAL => lp (n::ns)
			  | less => SOME (add1 m))
		  | lp [] = bug "missingIntegral" "unreachable"
                in
                  lp ns
                end)

(* missingInt *)
(* pre: argument list is sorted ascending *)
(* Given a list of sorted (asc) integers, return one of the integers that *)
(*   does not appear. *)
(* missingInt will probably return 1 less than the current min. *)
  val missingInt : int list -> int option = let
    val min = valOf Int.minInt
    val max = valOf Int.maxInt
    fun add1 (n: int) = n+1
    fun sub1 (n: int) = n-1
    val cmp = Int.compare
    val z = (0: int)
    in
      missingIntegral (numDict (min, max, z, add1, sub1, cmp))
    end

(* nextInt : LitSet.set -> Literal.literal option *)
(* pre: set contains only int literals *)
(* This implementation uses that fact that ORD_SET's listItems returns a *)
(*   sorted (ascending) list. *)
(* Usually returns (SOME (min-1)). *)
  fun nextInt (s: LitSet.set) : Literal.literal option = let
    fun int (Literal.Int n) = IntInf.toInt n
      | int _ = raise Fail "precondition not met"
    val fromInt = Literal.Int o IntInf.fromInt
    val ns = List.map int (LitSet.listItems s)
    in
      Option.map fromInt (missingInt ns)
    end

(* missingString *)
(* Returns a string that doesn't appear in the set. *)
(* Tries an initial unlikely string ("Mxyzptlk") and, if that's present, *)
(*   appends backticks onto it upto some limit (currently 10000). *)
(* Returns NONE if all those strings are in the set. *)
  fun missingString (s: LitSet.set) : Literal.literal option = let
    val lim = 10000
    fun lp (i, t) =
      if i >= lim then NONE (* very unlikely *)
      else if LitSet.member (s, (Literal.String t)) then lp (i+1, t^"`")
      else SOME (Literal.String t)
    in
      lp (0, "Mxyzptlk")
    end     

(* unrepresentedLit *)
(* pre: given literal set is incomplete *)
  fun unrepresentedLit (ty: Types.ty, s: LitSet.set) : AST.pat option =
    if TypeUtil.same (Basis.boolTy, ty) then
      (if LitSet.member (s, Literal.trueLit) then 
         SOME (AST.ConstPat (AST.LConst (Literal.falseLit, Basis.boolTy)))
       else 
         SOME (AST.ConstPat (AST.LConst (Literal.trueLit, Basis.boolTy))))
    else if TypeUtil.same (Basis.intTy, ty) then
      case nextInt s
        of NONE => SOME (AST.WildPat ty)
	 | SOME n => SOME (AST.ConstPat (AST.LConst (n, ty)))
    else if TypeUtil.same (Basis.stringTy, ty) then
      case missingString s
        of SOME t => SOME (AST.ConstPat (AST.LConst (t, ty)))
	 | NONE => SOME (AST.WildPat ty)
    else SOME (AST.WildPat ty)

(* a test to check if constructor c is the only one in its datatype *)
  fun soleConstructor (c: AST.dcon) : bool = 
    (nConsOf (DataCon.ownerOf c) = 1)

(* mkConPat will construct a ConPat with an appropriately-typed wildcard *)
(*   as its subpattern. *)
  fun mkConPat (c as Types.DCon {argTy, ...}, ts) = 
    (case argTy 
       of NONE => AST.ConstPat (AST.DConst (c, ts))
	| SOME ty => AST.ConPat (c, ts, AST.WildPat ty))

(* a pattern is irrefutable if *)
(* - it is a constructor pattern K(p) where *)
(*   - K is the sole constructor in its datatype, and *)
(*   - p is irrefutable, *)
(* - it is a tuple of irrefutable patterns, *)
(* - it is a variable, *)
(* - it is a wildcard, or *)
(* - it is a nullary constructor pattern K where *)
(*     K is the sole constructor in its datatype, or *)
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
    | irrefutableP (AST.HandlePat _) = false (* HandlePats are always refutable. *)
    | irrefutableP (AST.Pat p) = irrefutable p

(* select the pattern out of a match *)
  fun patOf (AST.PatMatch (p, _)) = p
    | patOf (AST.CondMatch (p, _, _)) = p

(* elimVarPat replaces all variables in a pattern with *)
(*   appropriately-typed wildcard patterns. *)
(* Wildcards are sufficient for checking pattern "usefulness" (Maranget's term). *)
  fun elimVarPat (AST.ConPat (c, ts, p)) = AST.ConPat (c, ts, elimVarPat p)
    | elimVarPat (AST.TuplePat ps) = AST.TuplePat (List.map elimVarPat ps)
    | elimVarPat (p as AST.VarPat _) = AST.WildPat (TypeOf.pat p)
    | elimVarPat (p as AST.WildPat _)  = p
    | elimVarPat (p as AST.ConstPat _) = p

(* mkPatMat is a smart constructor for pattern matrices. *)
(* All variables are replaced with appropriately-typed wildcards, and *)
(*   a singleton list is made of each pattern. *)
  fun mkPatMat (ms: AST.match list) : patmat =
    (case ms 
       of [] => bug "mkPatMat" "empty match list"
	| _  => List.map (fn m => [elimVarPat (patOf m)]) ms)

(* mkPatMatP builds a patmat from pmatches.
 * The implementation is by reduction to mkPatMat: it merges all 
 *   ppat lists into tuples of pats, and calls mkPatMat on the result.
 *)
  fun mkPatMatP (ms: AST.pmatch list) : patmat = let
    fun getTupleTy ([], optTys) =
          (case optTys
	     of SOME ts => Types.TupleTy ts
	      | NONE => bug "mkPatMatP" "no types found?")
      | getTupleTy (AST.PMatch (ps, _)::ms, optTys) = let
          val ts' = List.map TypeOf.ppat ps
          in
            case optTys
              of SOME ts =>
	           if ListPair.all TypeUtil.same (ts, ts') 
		   then getTupleTy (ms, SOME ts)
		   else bug "mkPatMatP" "ill-typed pmatches in parallel case"
	       | NONE => getTupleTy (ms, SOME ts')
	  end
      | getTupleTy (AST.Otherwise(_)::ms, optTys) = getTupleTy (ms, optTys) 
    (* make ppats into plain pats *)
    fun elim (AST.NDWildPat ty) = AST.WildPat ty
      | elim (h as AST.HandlePat _) = 
          (* TODO: consider adding a HandlePat variant for patmats *)
          (* I think handles would count nothing towards redundancy or *)    
          (* inexhaustiveness checks. *)
          raise Fail "mkPatMatP: HandlePat not supported"
      | elim (AST.Pat p) = elimVarPat p
    val wild = AST.WildPat (getTupleTy (ms, NONE))
    fun trx ([], acc) = List.rev acc
      | trx (AST.PMatch (ps, e)::t, acc) = trx (t, [AST.TuplePat (List.map elim ps)]::acc)
      | trx (AST.Otherwise(_)::t, acc) = trx (t, [wild]::acc)
    in
      trx (ms, [])
    end

(* dim returns the dimensions of a pattern matrix. *)
(* It relies on the invariant that a patmat has uniform width at all rows. *)
(* post: the two ints returned are nonnegative. *)
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
      | lp ([]::pss, acc) = bug "firstColCons" "malformed pattern matrix"
    in
      lp (p, DConSet.empty)
    end

(* completeLits tests, for unit and bool types, whether the literal set *)
(*   is complete. *)
(* All other literal sets are judged incomplete. *)
  fun completeLits ((ty, s): typed_lit_set) : bool = 
    if TypeUtil.same (Basis.unitTy, ty) then
      LitSet.equal (unitLitSet, s)
    else if TypeUtil.same (Basis.boolTy, ty) then
      LitSet.equal (boolLitSet, s)
    else false

(* firstColContainsCons is a predicate to test whether the first column *)
(*   of a pattern matrix contains constructors. *)
  val firstColContainsCons : patmat -> bool = let
    fun pred (AST.ConstPat (AST.DConst _)::_) = true
      | pred (AST.ConPat(_)::_) = true
      | pred _ = false
    in
      List.exists pred
    end

(* firstColContainsLits is a predicate to test whether the first column *)
(*   of a pattern matrix contains literals. *)
  val firstColContainsLits : patmat -> bool = let
    fun pred (AST.ConstPat (AST.LConst _)::_) = true
      | pred _ = false
    in
      List.exists pred
    end

(* firstColContainsTuples *)
  val firstColContainsTuples : patmat -> bool = let
    fun pred ((AST.TuplePat _)::_) = true
      | pred _ = false
    in
      List.exists pred
    end

(* firstColTy : patmat -> Types.ty *)
  fun firstColTy ((p::ps)::pss) = TypeOf.pat p
    | firstColTy p = (println "badness"; 
		      println (patmatToString p); 
		      bug "firstColTy" "malformed pattern matrix")

(* firstColTupleTy *)
  fun firstColTupleTy (p: patmat) : bool =
    (case firstColTy p
       of Types.TupleTy [] => false (* that's unit *)
	| Types.TupleTy ts => true
	| _ => false)

(* firstColLits builds a set of all literals appearing in *)
(*   the first (leftmost) column of a pattern matrix. *)
  fun firstColLits (p: patmat) : typed_lit_set = let
    fun lp ([], SOME ty, acc) = (ty, acc)
      | lp ([], NONE, acc) = bug "firstColLits" "no type?"
      | lp ((p::ps)::pss, optTy, acc) = 
          (case p
	    of AST.ConstPat (AST.LConst (lit, ty)) =>
                 (case optTy
		    of NONE => lp (pss, SOME ty, LitSet.add (acc, lit))
		     | SOME _ => lp (pss, optTy, LitSet.add (acc, lit)))
	     | _ => lp (pss, optTy, acc))
      | lp ([]::_, _, _) = bug "firstColLits" "malformed pattern matrix"
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
	      of AST.VarPat _   => bug "s" "(ConPat) unexpected VarPat in pattern matrix"
	       | _  => lp (pss, (p'::ps)::acc)
             (* end case *))	  else lp (pss, acc)
      | lp ((AST.TuplePat(qs)::ps)::pss, acc) =
          (* TuplePats are considered ConPats not matching the constructor c *)
          lp (pss, acc)
      | lp ((AST.VarPat(_)::ps)::pss, acc) = 
          bug "s" "(VarPat) unexpected VarPat in pattern matrix"
      | lp (((w as AST.WildPat _)::ps)::pss, acc) = lp (pss, (conWilds(c)@ps)::acc)
      | lp ((AST.ConstPat(k)::ps)::pss, acc) = 
          (case k
	    of AST.DConst (c', ts) => if DataCon.same (c, c') 
				      then lp (pss, ps::acc)
				      else lp (pss, acc)
	     | AST.LConst _ => lp (pss, acc)
	   (* end case *))
      | lp ([]::_, _) = bug "s" "malformed pattern matrix"
    in
      lp (p, [])
    end

(* sK is a version of s to compute the "specialized matrix" from a constant. *)
(* Constants are treated as nullary constructors. *)
  fun sK (AST.DConst (c, ts), p: patmat) : patmat = let
        fun lp ([], acc) = List.rev acc
	  | lp ((q::ps)::pss, acc) = 
             (case q 
	       of AST.ConstPat (AST.DConst (c', _)) =>
                    if DataCon.same (c, c') 
		    then lp (pss, ps::acc)
		    else lp (pss, acc)
		| AST.WildPat _ => lp (pss, ps::acc)
		| AST.VarPat _ => bug "sK" "unexpected VarPat in pattern matrix"
		| _ => lp (pss, acc)
	      (* end case *))
	  | lp ([]::_, acc) = bug "sK" "malformed pattern matrix"
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
		 | AST.VarPat _ => bug "sL" "unexpected VarPat in pattern matrix"
		 | _ => lp (pss, acc)
	       (* end case *))
	  | lp ([]::_, _) = bug "sL" "malformed pattern matrix"
        in
	  lp (p, [])
	end

(* sT is a version of s to compute the "specialized matrix" from a tuples. *)
(* Tuples are considered to have "invisible" constructors that are the     *)
(*   sole constructors of their type. *)
(* The arity of all tuples in the first column of p is checked for consistency *)
(*   (though it should never vary). *)
  fun sT (arity: int, p: patmat) : patmat = let
    fun lp ([], acc) = List.rev acc
      | lp ((q::ps)::pss, acc) = 
          (case q
	    of AST.TuplePat rs => 
                 if List.length rs = arity 
		 then lp (pss, (rs@ps)::acc)
		 else bug "sT" "tuple arity"
	     | AST.WildPat (Types.TupleTy ts) =>
                 if List.length ts = arity then let
                   val wilds = List.map AST.WildPat ts
                   in
		     lp (pss, (wilds@ps)::acc)
	           end
                 else bug "sT" "wild tuple arity"
	     | AST.WildPat t => 
                 bug "sT" "unexpected WildPat with non-tuple type"
	     | AST.VarPat _ =>
                 bug "sT" "unexpected VarPat in pattern matrix"
	     | AST.ConPat _ =>
                 (* ConPats shouldn't be in the same column as tuples *)
                 bug "sT" "unexpected ConPat in pattern matrix"
	     | AST.ConstPat _ =>
                 (* ConstPats shouldn't be in the same column as tuples *)
                 bug "sT" "unexpected ConstPat in pattern matrix"
	   (* end case *))
      | lp ([]::_, _) = bug "sT" "malformed pattern matrix"
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
          bug "d" "unexpected VarPat in pattern matrix"
      | lp ((AST.ConstPat(_)::_)::t, acc) = lp (t, acc)
      | lp ([]::_, _) = bug "d" "malformed pattern matrix"
    in
      lp (p, [])
    end

(* u tests whether pattern vector v is useful with respect to patmat p. *)
(*  invariant: width of p equals width of v (checked) *)
 fun u (p: patmat, v: patlist) : bool = let
    val {width, length} = dim p
    in
      if (width = 0) then (length = 0)
      else let
        val chk = let
	  val vlen = List.length v 
	  in
            if width = vlen then () 
	    else let
              val msg = "pat matrix width = " ^ Int.toString width ^
			", vector width = " ^ Int.toString vlen
              in
                bug "u" ("unequal widths: " ^ msg)
              end
          end
        val (q, qs) =
          (case v 
	    of h::t => (h, t)
	     | nil => bug "u" "broken invariant")
        in
	  case q
	   of AST.ConPat (c, ts, r) => u (s (c, p), r::qs)
	    | AST.ConstPat k => u (sK (k, p), qs)
	    | AST.VarPat x => bug "u" "unexpected VarPat in pattern matrix"
	    | AST.WildPat ty => 
                (case ty
		   of Types.TupleTy [] (* unit *) => wild (p, qs)
		    | Types.TupleTy ts => let
                        val p' = breakTups p
			val ws = List.map AST.WildPat ts
                        in
                          u (p', ws @ qs)
		        end
		    | _ => wild (p, qs))
	    | AST.TuplePat rs => let
                val arity = List.length rs 
                in 
		  u (sT (arity, p), rs@qs)
                end
	end
    end
  and wild (p: patmat, qs: patlist) =
    (* there are two cases to deal with: *)
    (*   1) the first column has ConstPat (LConst ...) (literal) patterns in it, or *)
    (*   2) the first column has ConPats and ConstPat (DConst _) pats in it *)    
    if firstColContainsLits p then let
      val (ty, sigma) = firstColLits p
      in
        if completeLits (ty, sigma) then let
          val _ = () (* println ("lits of type " ^ TypeUtil.toString ty ^ " in first col, complete set") *)
          val ls = LitSet.listItems sigma
          fun u' l = u (sL (l, ty, p), qs)
          in
            List.exists u' ls
	  end
        else let
          val _ = () (* println ("lits of type " ^ TypeUtil.toString ty ^ " in first col, incomplete set") *)
          in
            u (d p, qs)
          end
      end
    else let
      (* val _ = println "no lits in first col" *)
      val sigma = firstColCons p
      in
        if completeCons sigma then let
          (* val _ = println "all constructors found in first col" *)
          val cs = DConSet.listItems sigma
          fun u' c = u (s (c, p), conWilds(c) @ qs)
          in
            List.exists u' cs
          end        
        else let
          (* val _ = println "not all constructors found in first col" *)
          in
            u (d p, qs)
	  end
      end
  and breakTups (p: patmat) : patmat = let
    fun lp ([], acc) = List.rev acc
      | lp ((AST.TuplePat(ps)::qs)::pss, acc) = lp (pss, (ps@qs)::acc)
      | lp ((AST.WildPat(ty)::qs)::pss, acc) = 
          (case ty
	     of Types.TupleTy ts => lp (pss, ((List.map AST.WildPat ts)@qs)::acc)
	      | _ => bug "breakTups" ("wild pat with non-tuple type: " ^
				      TypeUtil.toString ty)
	    (* end case *))
      | lp ((AST.ConPat _::_)::_, _) = bug "breakTups" "unexpected ConPat"
      | lp ((AST.VarPat(_)::_)::_, _) = bug "breakTups" "unexpected VarPat"
      | lp ((AST.ConstPat(_)::_)::_, _) = bug "breakTups" "unexpected ConstPat"
      | lp ([]::_, _) = bug "breakTups" "malformed pattern matrix"
    in
      lp (p, [])
    end

(* algorithm I *)
(* Checks for inexhaustiveness. *)
(* If the given patmat is inexhaustive, I returns a witness to the inexhautiveness. *)
(* If the patmat is exhaustive, I returns NONE. *)
(* ex: test of (case x of true => y) will return SOME [false]. *)
(* ex: test of (case x of 0 => y)    will return SOME [~1]. *)
(* ex: test of (case x of () => y)   will return NONE *)
(* ex: test of (case (x,y) of ((), true) => z) will return SOME [(), false]. *)
  fun i (p: patmat, n: int) : patlist option =
    if n = 0 then let
      val {width, length} = dim p
      in
        if length > 0 then NONE else SOME []
      end
    else if firstColTupleTy p then let
    (* If the first column has a tuple type, assuming well-typedness of the whole, *)
    (*   it can only contain tuples and vars/wildcards. *)
    (* First col automatically contains a "complete signature of constructors" by the assumption *)
    (*   that a tuple is a member of an implicit datatype with exactly one constructor. *)
      val ts = case firstColTy p
		of Types.TupleTy ts => ts
		 | _ => bug "i" "expected TupleTy"
      val arity = List.length ts
      in
        case i (sT (arity, p), arity+n-1)
         of SOME ps => SOME [AST.TuplePat ps]
	  | NONE => NONE
      end
    else if firstColContainsLits p then let
      val (ty, sigma) = firstColLits p
      in
        if completeLits (ty, sigma) then let
          fun lp [] = NONE
	    | lp (l::ls) =
                case i (sL (l, ty, p), n-1)
                  of SOME v => SOME (AST.ConstPat(AST.LConst(l,ty))::v)
		   | NONE => lp ls
          in
            lp (LitSet.listItems sigma)
          end
	else 
          case i (d p, n-1)
            of NONE => NONE
	     | SOME v => 
                 (case unrepresentedLit (ty, sigma)
		    of SOME p => SOME (p::v)
		     | NONE => raise Fail "algorithm I lit")
      end
    else 
      (* If we reach this point, the first column contains a mix of *)
      (*   constructor patterns, var patterns and wildcards. *)
      if firstColContainsCons p then let
        val sigma = firstColCons p
        val ts = (case firstColTy p
		    of Types.ConTy (ts, _) => ts
		     | _ => bug "i" "unreachable")
        in
          if completeCons sigma then let
            fun lp [] = NONE
	      | lp (c::cs) = let
                  val cArity = (case DataCon.argTypeOf c
				  of SOME _ => 1
				   | NONE => 0) 				     
                  in
                    case i (s (c, p), cArity + n - 1)
		      of NONE => lp cs
		       | SOME (p::ps) => SOME (AST.ConPat (c, ts, p)::ps)
		       | SOME [] => bug "i" "illegal conpat arg"
	  	  end
            in
              lp (DConSet.listItems sigma)
	    end
	  else (* incomplete set *)
            (case i (d p, n-1)
               of NONE => NONE
		| SOME v => let
                    val c = case DConSet.listItems (unrepresentedCons sigma)
                              of [] => bug "i" "sigma is incomplete"
			       | c::_ => c
	 	    val c' = mkConPat (c, ts)
                    in
		      SOME (c'::v)
		    end
		 (* end case *))
	end
      else let
        (* Here there are no constructor pats in the first column -- just vars and wilds. *)
        val ty = firstColTy p 
        in
          case i (d p, n-1)
           of NONE => NONE
	    | SOME v => SOME (AST.WildPat(ty)::v)
       end

(* inexhaustive : patmat -> patlist option *)
(* Attempts to produce a witness to inexhaustiveness. *)
(* If no such witness can be produced, the argument is exhaustive. *)
(* Returns NONE if p is exhaustive. *)
(* Returns (SOME witness) if p is inexhaustive. *)
  fun inexhaustive (p: patmat) : patlist option = let
    val {width, length} = dim p
    in
      i (p, width)
    end

(* redundant : patmat -> patlist option *)
(* Attempts to produce a witness to redundancy. *)
(* If no such witness can be produced, the argument is irredundant. *)
(* Returns NONE if p is irredundant. *)
(* Returns (SOME witness) is p is redundant. *)
  fun redundant (p: patmat) : patlist option =
    (case p
       of [] => NONE
	| [v] => NONE
	| v::vs => let
            fun lp ([], _) = NONE
	      | lp (ps::pss, m) = 
                  if u (m, ps) then lp (pss, m@[ps])
		  else SOME ps
            in
	      lp (vs, [v])
	    end
      (* end case *))

(* checkPatMat *)
(* Checks a patmat for inexhaustiveness and redundancy, with side effects for failed checks. *)
(* If the patmat is inexhaustive, a warning is issued to the error stream. *)
(* If the patmat is redundant, an error is thrown to the error stream. *)
(* If the patmat is exhaustive and irredundant, unit is returned. *)
  fun checkPatMat (err: err_stream) (p: patmat) : unit = let
    fun tos ps = String.concatWith "|" (List.map patToString ps)
    fun redund p =
      (case redundant p
	of SOME witness => let
             val msg = tos witness
             in
               errRedundant err ("redundant match: " ^ msg)
             end
	 | NONE => ()
       (* end case *))
    in
      case inexhaustive p
       of SOME witness => let
            val msg = "cannot match, for example, " ^ tos witness
            in
	      warnInexMatch err ("inexhaustive match: " ^ msg)
	    end
	| NONE => redund p
    end

(* checkMatchList checks that the match list is both exhaustive and irredundant. *)
(* Redundancy yields a warning; inexhaustiveness is an error. *)
  fun checkMatchList (err: err_stream, ms: AST.match list) : unit =
    (checkPatMat err) (mkPatMat ms)

(* checkPMatchList checks that the pmatch list is both exhaustive and irredundant. *)
(* Redundancy yields a warning; inexhaustiveness is an error. *)
  fun checkPMatchList (err: err_stream, ms: AST.pmatch list) : unit = 
    (checkPatMat err) (mkPatMatP ms) 

(* checkHandleMatches checks that the match list in a handle is not redundant. *)
(* It does not check for exhaustiveness. *)
  fun checkHandleMatches (err: err_stream, ms: AST.match list) : unit = let
    val p = mkPatMat ms
    in
      case redundant p 
        of SOME ps => let
             val msg = String.concatWith "," (List.map patToString ps)
             in
               errRedundant err ("redundant match: " ^ msg)
             end
	 | NONE => ()       
    end

(* bodyOf : AST.lambda -> AST.exp *)
  val bodyOf : AST.lambda -> AST.exp = (fn AST.FB (_, _, b) => b)

(* checkExp checks all match lists recursively in an expression. *)
(* That is, it checks all cases, pcases, and handles within given expression. *)
  fun checkExp (err: err_stream, e: AST.exp) : unit = let
    fun exp (AST.LetExp (b, e)) = (binding b; exp e)
      | exp (AST.IfExp (e1, e2, e3, _)) = (exp e1; exp e2; exp e3)
      | exp (AST.CaseExp (e, ms, _)) = (exp e; checkMatchList (err, ms))
      | exp (AST.PCaseExp (es, ms, _)) = (List.app exp es; checkPMatchList (err, ms))
      | exp (AST.HandleExp (e, ms, _)) = (exp e; checkHandleMatches (err, ms))
          (* note: handle match lists are checked for redundancy only *)
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
  and pat p = if irrefutable p then () else warnInexBind err p
  and lambdas fs = List.app (exp o bodyOf) fs
  in
    exp e
  end

end
