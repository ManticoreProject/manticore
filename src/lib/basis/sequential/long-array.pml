(* long-array.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Monomorphic arrays of longs.
 *)

structure LongArray = struct

structure U = UnsafeLongArray

_primcode (
  typedef array = U.array;
  define inline @length (a : array / exh : exh) : ml_int =
    return(alloc(#1(a)))
  ;
  define @modify (arg : [fun (ml_long / exh -> ml_long), array] / exh : exh) : unit =
    cont mlp (i : int, n : int, data : any, f : fun (ml_long / exh -> ml_long), exh : exh) =
      if I32Lt (i, n) then
	return(UNIT)
      else
	let y : long = ArrLoadI64 (data, i)
	let x : ml_long = apply f (alloc(y) / exh)
	do ArrStoreI64 (data, i, #0(x))
	throw mlp (I32Add (i, 1), n, data, f, exh)
    let a : array = #1(arg)
    let data : any = #0(a)
    let n : int = #1(a)
    throw mlp (0, n, data, #0(arg), exh)
  ;
)

type array = U.array

val maxLen = 16777215  (* FIXME: what is the right value to use for Manticore? *)

val update = U.update
val sub = U.sub

fun array (n, init) = 
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

(*
FIXME! this is the version that we would like to use, but 
we do not at the moment because of poor performance. the
issue seems to be that closure conversion does bad things
to the modify function. in particular, it places
values like the array offset and the value to store in
the array into the environment pointer that we pass
along to the block generated for the "then" arm of the
if statement. -- mike

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
*)
val modify' : (long -> long) * array -> unit  = _prim(U.@modify)

fun modify f a = modify' (f, a)

end
