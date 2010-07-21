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

  structure DConSet = RedBlackSetFn (struct
				       type ord_key = AST.dcon
				       val compare  = DataCon.compare
				     end)

  type patmat = AST.pat list list (* a regular (non-jagged) matrix of patterns *)

  fun dim (p: patmat) : {width: int, length: int} = let
    val w = List.length (List.nth (p, 0))
    val n = List.length p
    in
      {width=w, length=n}
    end

  fun complete (s: DConSet.set) : bool = 
    (case DConSet.listItems s
       of [] => false
        | c::_ => let
            val Types.Tyc {def, ...} = DataCon.ownerOf c
            in
              case def
	       of Types.AbsTyc => raise Fail "shouldn't be an AbsTyc"
	        | Types.DataTyc {nCons, ...} => (!nCons) = (DConSet.numItems s)
            end
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
