(* parray.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel array utilities.
 *)

structure PArray = struct

  val fail = Fail.fail "PArray"

  _primcode (
    define inline @to-rope (x : parray / _ : exh) : Rope.rope =
      return ((Rope.rope)x);
    define inline @from-rope (x : Rope.rope / _ : exh) : parray =
      return ((parray)x);
    )

  type 'a parray = 'a parray

  (* local *)

  (* I would prefer these were local but I had to expose them to the compiler for the FT. *)
  val toRope : 'a parray -> 'a Rope.rope = _prim(@to-rope)
  val fromRope : 'a Rope.rope -> 'a parray = _prim(@from-rope)

  (* in *)

  (* Rope implementations are the default. *)
  (* These functions are swapped out when the FT is turned on. *)
  fun sub (pa, i) = Rope.sub (toRope pa, i)
  fun length pa = Rope.length (toRope pa)
  fun tab (n, f) = fromRope (Rope.tab (n, f))
  fun tabFromToStep (a, b, step, f) = fromRope (Rope.tabFromToStep (a, b, step, f))
  fun map f pa = fromRope (Rope.map f (toRope pa))
  fun reduce rator init pa = Rope.reduce rator init (toRope pa)
  fun range (from, to_, step) = fromRope (Rope.range (from, to_, step))
  fun app f pa = Rope.app f (toRope pa)
  fun tab2D (iFrom, iTo, iStep, jFrom, jTo, jStep, f) = 
    tabFromToStep (iFrom, iTo, iStep, fn i => 
      tabFromToStep (jFrom, jTo, jStep, fn j => f (i, j)))

  (* fun filter (pred, pa) = fromRope(Rope.filter pred (toRope pa)) *)
  (* fun rev pa = fromRope(Rope.rev(toRope pa)) *)
  (* fun fromList l = fromRope(Rope.fromList l) *)
  (* fun concat (pa1, pa2) = fromRope(Rope.concat(toRope pa1, toRope pa2)) *)
  (* fun tabulateWithPred (n, f) = fromRope(Rope.tabulate(n, f)) *)
  (* fun forP (n, f) = Rope.for (n,f) *)
  (* fun repP (n, x) = fromRope(Rope.tabulate (n, fn _ => x)) *)

  (* end (* local *) *)

(* (* I can't write polymorphic toString, unfortunately. Specific implementations below. *)
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
*)

  fun tos_int (parr : int parray) = let
    fun tos i = Int.toString (parr ! i)
    fun lp (i, acc) =
      if (i<0) then
        String.concat ("[|"::acc)
      else
        lp (i-1, tos(i)::","::acc)
    val n = length parr
    in
      if (n<0) then
        fail "tos_int" "BUG: negative length"
      else if (n=0) then 
        "[||]"
      else let
        val init = [tos(n-1),"|]"]
        in
          lp (n-2, init)
        end
    end

  fun tos_intParr (parr : int parray parray) = let
    fun tos i = tos_int (parr ! i)
    fun lp (i, acc) = 
      if (i<0) then
        String.concat ("[|"::acc)
      else
        lp (i-1, tos(i)::",\n"::acc)
    val n = length parr
    in
      if (n<0) then
        fail "tos_intp" "BUG: negative length"
      else if (n=0) then
        "[||]"
      else let
        val init = [tos(n-1), "\n|]"]
        in
          lp (n-2, init)
        end
    end

  fun tos_intPair parr = let
    val itos = Int.toString
    fun tos i = let
      val (m,n) = parr!i 
      in
        "(" ^ itos m ^ "," ^ itos n ^ ")"
      end
    fun lp (i, acc) =
      if (i<0) then
        String.concat ("[|"::acc)
      else
        lp (i-1, tos(i)::","::acc)
    val n = length parr
    in
      if (n<0) then
        fail "tos_intPair" "BUG: negative length"
      else if (n=0) then "[||]"
      else let
        val init = [tos(n-1),"|]"]
        in
          lp (n-2, init)
        end
    end

end

(* (\* FIXME: the following definitions should be in a separate *)
(*  * file (a la sequential/pervasives.pml) *)
(*  *\) *)
(* (\* Below is the subset of the parallel array module that should bound at the top level. *\) *)
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
(* *\) *)
