(* int-parray.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel array utilities.
 *)

structure IntPArray = struct

  val fail = Fail.fail "IntPArray"

  _primcode (
	define inline @to-rope (x : parray / _ : exh) : IntRope.int_rope =
    	    return ((IntRope.int_rope)x)
    	  ;
    	define inline @from-rope (x : IntRope.int_rope / _ : exh) : parray =
    	    return ((parray)x)
    	  ;
    )

  type int_parray = int parray

  structure R = IntRope

(* FIXME had to expose toRope and fromRope to enable special treatment of them in the flatt. trns. *)
  (* local *)
  val toRope : int_parray -> R.int_rope = _prim(@to-rope)
  val fromRope : R.int_rope -> int_parray = _prim(@from-rope)
  (* in *)

  fun sub (pa, i) = R.sub (toRope pa, i)
  fun length pa = R.length (toRope pa)
  fun tab (n, f) = fromRope (R.tabP (n, f))
  fun tabFromToStep (a, b, step, f) = fromRope (R.tabFromToStepP (a, b, step, f))
  fun map f pa = fromRope (R.mapP (f, toRope pa))
  fun reduce assocOp init pa = R.reduceP (assocOp, init, toRope pa)
  fun range (from, to_, step) = fromRope (R.rangeP (from, to_, step))
  fun app f pa = R.app (f, toRope pa)

  fun tos parr = let
    fun tos i = Int.toString(parr!i)
    fun lp (i, acc) =
      if (i<0) then
        String.concat ("[|"::acc)
      else
        lp (i-1, tos(i)::","::acc)
    val n = length parr
    in
      if (n<0) then fail "tos" "bug"
      else if (n=0) then "[||]"
      else let
        val init = [tos(n-1),"|]"]
        in
          lp (n-2, init)
        end
    end

end
