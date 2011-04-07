(* parray.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel array utilities.
 *)

structure PArray = struct

    _primcode (
    	define inline @to-rope (x : parray / _ : exh) : Rope.rope =
    	    return ((Rope.rope)x)
    	  ;
    	define inline @from-rope (x : Rope.rope / _ : exh) : parray =
    	    return ((parray)x)
    	  ;
      )

    type 'a parray = 'a parray

(*   (\* FIXME too tightly coupled with Rope *\) *)

(* FIXME had to expose these to enable special treatment of them in the flatt. trns. *)
    (* local *)
      val toRope : 'a parray -> 'a Rope.rope = _prim(@to-rope)
      val fromRope : 'a Rope.rope -> 'a parray = _prim(@from-rope)
    (* in *)

    fun sub (pa, i) = Rope.sub(toRope pa, i)
    fun length pa = Rope.length(toRope pa)
    fun tab (n, f) = fromRope(Rope.tabP(n, f))
    fun map f pa = fromRope(Rope.mapP (f, toRope pa))
    fun reduce assocOp init pa = Rope.reduceP (assocOp, init, toRope pa)

(*     fun filter (pred, pa) = fromRope(Rope.filterP (pred, toRope pa)) *)
(*     fun rev pa = fromRope(Rope.revP(toRope pa)) *)
(*     fun fromList l = fromRope(Rope.fromList l) *)
(*     fun concat (pa1, pa2) = fromRope(Rope.concat(toRope pa1, toRope pa2)) *)
(*     fun tabulateWithPred (n, f) = fromRope(Rope.tabP(n, f)) *)
(*     fun forP (n, f) = Rope.forP(n,f) *)

(*   (\* repP : int * 'a -> 'a parray *\) *)
(*   (\* called "dist" in NESL and Keller *\) *)
(*   (\* called "replicateP" in DPH impl *\) *)
(*     fun repP (n, x) = fromRope(Rope.tabP (n, fn _ => x)) *)

    (* end (\* local *\) *)

(*   (\* toString : ('a -> string ) -> string -> 'a parray -> string *\) *)
(*   (\* FIXME: should we exploit the fact that we're dealing with a rope? *\) *)
(*     fun toString eltToString sep parr = let *)
(* 	  val n = length parr *)
(* 	  fun lp (m, acc) = if (m >= n) *)
(* 		then List.rev ("|]" :: acc) *)
(* 		else let *)
(* 		  val s = eltToString (sub (parr, m)) *)
(* 		  in *)
(* 		    if (m = (n-1)) then *)
(* 		      List.rev ("|]" :: s :: acc) *)
(* 		    else *)
(* 		      lp (m+1, sep :: s :: acc) *)
(* 		  end *)
(* 	  val init = "[|" :: nil *)
(* 	  in *)
(* 	    String.concat (lp (0, init)) *)
(* 	  end *)

end

(* (\* FIXME: the following definitions should be in a separate *)
(*  * file (a la sequential/pervasives.pml) *)
(*  *\) *)
(* (\* below is the subset of the parallel array module that should bound at the top level. *\) *)
(* val reduceP = PArray.reduce *)
(* val filterP = PArray.filter *)
(* val subP = PArray.sub *)
(* val revP = PArray.rev *)
(* val lengthP = PArray.length *)
(* val mapP = PArray.map *)
(* val fromListP = PArray.fromList *)
(* val concatP = PArray.concat *)
(* val tabP = PArray.tabulateWithPred *)
(* val forP = PArray.forP *)

