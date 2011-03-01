(* float-array.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Arrays of unboxed floats.
 *)

structure FloatArray = struct

structure U = UnsafeFloatArray

_primcode (
  typedef array = U.array;
  define inline @length (a : array / exh : exh) : ml_int =
    return(alloc(#1(a)))
  ;
)

type array = U.array

val maxLen = 16777215  

val update = U.update
val sub = U.sub

fun tabulate (n, f) = 
  if n < 0 orelse n > maxLen then
    raise Fail "Size"
  else let
    val a = U.create n
    fun lp i =
      if i < n then
	(U.update (a, i, f i); lp (i + 1))
      else
	()
    in
      lp 0;
      a
    end

fun array (n, init : float) = tabulate (n, fn _ => init)

val length : array -> int = _prim (@length)

fun modify f a = let
  val n = length a
  fun m i =
    if i < n then
      (update (a, i, f (sub (a, i))); m (i + 1))
    else
      ()
  in
    m 0
  end

end
