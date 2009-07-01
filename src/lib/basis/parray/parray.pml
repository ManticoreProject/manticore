(* parray.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel array utilities.
 *)

structure PArray = struct

    _primcode (
	define inline @to-rope (x : parray / _ : exh) : Ropes.rope =
	    return ((Ropes.rope)x)
	  ;
	define inline @from-rope (x : Ropes.rope / _ : exh) : parray =
	    return ((parray)x)
	  ;
      )

    type 'a parray = 'a parray

    local
      val toRope : 'a parray -> 'a Ropes.rope = _prim(@to-rope)
      val fromRope : 'a Ropes.rope -> 'a parray = _prim(@from-rope)
    in

  (* FIXME too tightly coupled *)
    fun sub (pa, i) = Ropes.sub(toRope pa, i)
    fun length pa = Ropes.length(toRope pa)
    fun reduce (rator, init, pa) = Ropes.reduceP (rator, init, toRope pa)
    fun filter (pred, pa) = fromRope(Ropes.filterP (pred, toRope pa))
    fun map (f, pa) = fromRope(Ropes.mapP (f, toRope pa))
    fun rev pa = fromRope(Ropes.revP(toRope pa))
    fun fromList l = fromRope(Ropes.fromList l)
    fun concat (pa1, pa2) = fromRope(Ropes.concat(toRope pa1, toRope pa2))
    fun tabulateWithPred (n, f) = fromRope(Ropes.tabP(n, f))

  (* repP : int * 'a -> 'a parray *)
  (* called "dist" in NESL and Keller *)
  (* called "replicateP" in DPH impl *)
    fun repP (n, x) = fromRope(Ropes.tabP (n, fn _ => x))

  end (* local *)

  (* toString : ('a -> string ) -> string -> 'a parray -> string *)
  (* FIXME: should we exploit the fact that we're dealing with a rope? *)
    fun toString eltToString sep parr = let
	  val n = length parr
	  fun lp (m, acc) = if (m >= n)
		then List.rev ("|]" :: acc)
		else let
		  val s = eltToString (sub (parr, m))
		  in
		    if (m = (n-1)) then
		      List.rev ("|]" :: s :: acc)
		    else
		      lp (m+1, sep :: s :: acc)
		  end
	  val init = "[|" :: nil
	  in
	    String.concat (lp (0, init))
	  end

end

(* FIXME: the following definitions should be in a separate
 * file (a la sequential/pervasives.pml)
 *)
(* below is the subset of the parallel array module that should bound at the top level. *)
val reduceP = PArray.reduce
val filterP = PArray.filter
val subP = PArray.sub
val revP = PArray.rev
val lengthP = PArray.length
val mapP = PArray.map
val fromListP = PArray.fromList
val concatP = PArray.concat
val tabP = PArray.tabulateWithPred

