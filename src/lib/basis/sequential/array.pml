(* array.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Polymorphic arrays.
 *)

structure Array = struct

structure U = UnsafeArray

_primcode (
  typedef array = U.array;
  define inline @length (a : array / exh : exh) : ml_int =
    return(alloc(#1(a)))
  ;
)

type 'a array = 'a U.array

val maxLen = 16777215

val length : 'a array -> int = _prim (@length)

fun isIxInBounds (a, i) = i >= 0 andalso i < length a

fun update (a, i, x) = 
  if isIxInBounds (a, i) then
    U.update (a, i, x)
  else
    Debug.failwith "Array.update: index i out of bounds"
fun sub (a, i) =
  if isIxInBounds (a, i) then
    U.sub (a, i)
  else
    Debug.failwith "Array.sub: index i out of bounds"

fun tabulate (n, f) = 
  if n <= 0 orelse n > maxLen then
    raise Fail "Size"
  else let
    val a = U.create (n, f 0)
    fun lp i =
      if i < n then
	(U.update (a, i, f i); lp (i + 1))
      else
	()
    in
      lp 1;
      a
    end

fun array (n, init) = tabulate (n, fn _ => init)

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

fun map f a = tabulate (length a, fn i => f (U.sub (a, i)))

fun app f a = let
  val len = length a
  fun app i = 
    if i < len then 
      (f (sub (a, i)); app (i + 1)) 
    else 
      ()
  in
    app 0
  end

fun appi f a = let
  val len = length a
  fun app i = 
    if i < len then 
      (f (i, sub (a, i)); app (i + 1)) 
    else 
      ()
  in
    app 0
  end

fun foldl f z s = let
  val len = length s
  fun lp (i, acc) =
    if i >= len then acc
    else lp (i+1, f (sub(s,i), acc))
  in
    lp (0, z)
  end       

end
