(* int-array.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Arrays of unboxed ints.
 *)

structure IntArray = struct

structure U = UnsafeIntArray

_primcode (
  typedef array = U.array;
  define inline @length (a : array / exh : exh) : ml_int =
    return(alloc(#1(a)))
  ;
)

type array = U.array

val length : array -> int = _prim (@length)

fun isIxInBounds (a, i) = i >= 0 andalso i < length a

fun update (a, i, x) = 
  if isIxInBounds (a, i) then
    U.update (a, i, x)
  else
    Debug.failwith "IntArray.update: index out of bounds"

fun sub (a, i) =
  if isIxInBounds (a, i) then
    U.sub (a, i)
  else
    Debug.failwith "IntArray.sub: index out of bounds"

fun tabulate (n, f) = 
  if n < 0 then let
    val msg = "IntArray.tabulate: size " ^ Int.toString n
    in 
      Debug.failwith msg
    end
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

fun array (n, init) = tabulate (n, fn _ => init)

fun unsafeCreate n = U.create n

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
