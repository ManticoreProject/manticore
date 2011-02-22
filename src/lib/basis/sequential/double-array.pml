(* double-array.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Monomorphic arrays of doubles.
 *)

structure DoubleArray = struct

structure U = UnsafeDoubleArray

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

fun array (n, init : double) = 
  if n < 0 orelse n > maxLen then
    raise Fail "Size"
  else let
    val a = U.create n
    fun fill i =
      if i < n then
	(update (a, i, init); fill (i + 1))
      else
	()
    in
      fill 0;
      a
    end

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
