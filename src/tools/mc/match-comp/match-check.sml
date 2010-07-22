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
       
  structure DConSet = RedBlackSetFn (struct
				       type ord_key = AST.dcon
				       val compare  = DataCon.compare
				     end)

(* a patmat (pattern matrix) is a regular (non-jagged) matrix of patterns *) 
(* the invariant (regularity) must be maintained by smart constructors *)
  type patmat = AST.pat list list 

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
