(* match-compile.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Pattern match checking, following Maranget (JFP 2007).
 *)

structure MatchCheck (* : sig

    val check : match list -> bool

  end *) = struct

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
    | irrefutable (AST.ConstPat (AST.DConst (c, ts))) = 
        soleConstructor c
    | irrefutable (AST.ConstPat (AST.LConst (lit, ty))) =
        TypeUtil.same (ty, Basis.unitTy)
       
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

  fun patOf (AST.PatMatch (p, _)) = p
    | patOf (AST.CondMatch (p, _, _)) = p

(* a patmat (pattern matrix) is a regular (non-jagged) matrix of patterns *) 
(* the invariant (regularity) must be maintained by smart constructors *)
  type patmat = AST.pat list list 

(* a patmat is built from a match list as follows. *)
(* If you select the patterns only out of a match list, you will have *)
(* A) a list of conpats with vars, wilds, and dconsts, *)
(* B) a list of tuples of some fixed arity n with vars and wilds, or *)
(* C) a list of consts, vars, and wilds. *)
(* In cases A and C, the patmat is of width 1, just the list of patterns. *)
(* If case B, *)
(* - all tuples are divided into their n components, and *)
(* - all vars and wilds are transformed into n wilds of the approriate types. *)
  fun mkPatMat (ms: AST.match list) : patmat = let
    fun tupleInMatch (AST.PatMatch (AST.TuplePat _, _)) = true
      | tupleInMatch (AST.CondMatch (AST.TuplePat _, _, _)) = true
      | tupleInMatch _ = false
    in
      case ms 
       of [] => raise Fail "empty match list"
	| _ => if List.exists tupleInMatch ms 
	       then tups ms
	       else List.map (fn m => [patOf m]) ms
    end
  and tups (ms: AST.match list) : patmat = let
    fun getTypes ([], SOME (Types.TupleTy ts)) = ts
      | getTypes ([], SOME _) = raise Fail "BUG: expected tuple ty"
      | getTypes ([], NONE) = raise Fail "BUG"
      | getTypes ((p as AST.TuplePat _)::t, NONE) = 
          getTypes (t, SOME (TypeOf.pat p))
      | getTypes ((p as AST.TuplePat _)::t, SOME ty) = 
          if TypeUtil.same (TypeOf.pat p, ty)
          then getTypes (t, SOME ty)
	  else raise Fail "ill-typed match list"
      | getTypes ((p as AST.VarPat _)::t, NONE) =
          getTypes (t, SOME (TypeOf.pat p))
      | getTypes ((p as AST.VarPat _)::t, SOME ty) =
          if TypeUtil.same (TypeOf.pat p, ty)
	  then getTypes (t, SOME ty)
	  else raise Fail "ill-typed match list"
      | getTypes ((p as AST.WildPat ty)::t, NONE) = 
          getTypes (t, SOME ty)
      | getTypes ((p as AST.WildPat ty')::t, SOME ty) = 
          if TypeUtil.same (ty', ty)
	  then getTypes (t, SOME ty)
	  else raise Fail "ill-typed match list"
      | getTypes (AST.ConPat _::_, _)   = raise Fail "BUG: unexpected ConPat"
      | getTypes (AST.ConstPat _::_, _) = raise Fail "BUG: unexpected ConstPat"
    val ps = List.map patOf ms
    val ts = getTypes (ps, NONE)
    val wilds = List.map AST.WildPat ts
    fun trx ([], acc) = List.rev acc
      | trx (AST.TuplePat(ps)::t, acc) = trx (t, ps::acc)
      | trx (AST.VarPat(_)::t, acc) = trx (t, wilds::acc)
      | trx (AST.WildPat(_)::t, acc) = trx (t, wilds::acc)
      | trx (AST.ConPat(_)::_, _)   = raise Fail "BUG: unexpected ConPat"
      | trx (AST.ConstPat(_)::_, _) = raise Fail "BUG: unexpected ConstPat"
    in
      trx (ps, [])
    end

  fun dim (p: patmat) : {width: int, length: int} = let
    val w = List.length (List.nth (p, 0))
    val n = List.length p
    in
      {width=w, length=n}
    end

  fun complete (s: DConSet.set) : bool = 
    (case DConSet.listItems s
       of [] => false
        | c::_ => (nConsOf (DataCon.ownerOf c)) = (DConSet.numItems s)
      (* end case *))

  fun firstColCons (p: patmat) : DConSet.set = let
    fun lp ([], acc) = acc
      | lp ((p::ps)::pss, acc) = 
          (case p
	    of AST.ConPat (c, _, _) => lp (pss, DConSet.add (acc, c))
	     | _ => lp (pss, acc))
      | lp ([]::pss, acc) = raise Fail "malformed patmat"
	     
    in
      lp (p, DConSet.empty)
    end

  (* invariant: width of p equals width of v *)
  fun u (p: patmat, v: AST.pat list) : bool = let
    val {width, length} = dim p
    in
      if width = 0 then
        if length > 0 then false
        else true
      else let
        val (q, qs) =
          (case v 
	    of h::t => (h, t)
	     | nil => raise Fail "broken invariant")
        in
	  case q
	   of AST.ConPat (c, ts, p) => raise Fail "todo - cons case"
	    | AST.TuplePat ps => raise Fail "todo - tuple case"
	    | AST.VarPat x => raise Fail "todo - wild case"
	    | AST.WildPat t => raise Fail "todo - wild case"
	    | AST.ConstPat k => raise Fail "todo - cons case"
	end
    end

end
