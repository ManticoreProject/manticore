(* double-array.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Arrays of unboxed doubles.
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

  val length : array -> int = _prim (@length)

  fun isIxInBounds (a, i) = i >= 0 andalso i < length a

  fun update (a, i, x) = 
    if isIxInBounds (a, i) then
      U.update (a, i, x)
    else
      Debug.failwith "DoubleArray.update: index out of bounds"

  fun sub (a, i) =
    if isIxInBounds (a, i) then
      U.sub (a, i)
    else
      Debug.failwith "DoubleArray.sub: index out of bounds"

  fun tabulate (n, f) = 
    if n < 0 then
      raise Fail "Size"
    else let
      val a = U.create n
      fun lp i =
        if i < n then
	  (U.update (a, i, f i); lp (i + 1))
        else
	  a
      in
        lp 0
      end

  fun array (n, init : double) = tabulate (n, fn _ => init)

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

  fun foldl f init arr = let
    val len = length arr
    fun fold (i, a) =
      if (i >= len) then a
      else fold (i+1, f (sub (arr, i), a))
    in
      fold (0, init)
    end

  fun foldr f init arr = let
    val len = length arr
    fun fold (i, a) = 
      if (i < 0) then a
      else fold (i-1, f (sub (arr, i), a))
    in
      fold (len-1, init)
    end

  fun map (f: double -> double) arr = let
    fun f' i = f (sub (arr, i))
    in
      tabulate (length arr, f')
    end

  fun app f arr = let
    val len = length arr
    fun appf i = 
      if (i >= len) then ()
      else (f (sub (arr, i)); appf (i+1))
    in
      appf 0
    end

  fun fromList (xs : double list) = let
    val len = List.length xs
    val arr = U.create len
    fun lp (i, ys) = case ys
      of nil => arr
       | h::t => (update (arr, i, h); lp (i+1, t))
    in
      lp (0, xs)
    end

end
