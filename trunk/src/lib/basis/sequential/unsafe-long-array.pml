(* unsafe-long-array.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Unsafe, monomorphic arrays of longs.
 *)

#include <prim.def>

#define MAX_LOCAL_ARRAY_SZ      I32Div(MAX_LOCAL_ARRAY_SZB, 8:int)

structure UnsafeLongArray = struct

_primcode (
  extern void* AllocBigLongArray (void*, int) __attribute__((alloc,pure));
  typedef array = PrimTypes.array;
  define inline @create (a : ml_int / exh : exh) : array =
    let n : int = #0(a)
    if I32Lt (n, MAX_LOCAL_ARRAY_SZ) then
      let a : array = AllocLongArray (n)
      return(a)
    else
      let data : any = ccall AllocBigLongArray (host_vproc, n)
      let a : array = alloc (data, n)
      return(a)
  ;
  define inline @sub (arg : [array, ml_int] / exh : exh) : ml_long =
    let a : array = #0(arg)
    let data : any = #0(a)
    let ix : int = #0(#1(arg))
    let x : long = ArrLoadI64 (data, ix)
    return (alloc(x))
  ;
  define inline @update (arg : [array, ml_int, ml_long] / exh : exh) : unit =
    let a : array = #0(arg)
    let data : any = #0(a)
    let ix : int = #0(#1(arg))
    let x : long = #0(#2(arg))
    do ArrStoreI64 (data, ix, x)
    return(UNIT)
  ;
)

type array = _prim (array)
(* create n *)
(* allocates an array of size n (elements of the array are not initialized) *)
val create : int -> array = _prim (@create)
(* sub (a, i) *)
(* returns the ith element of array a *)
(* it is required that 0 <= i < |a|, where |a| is the size of a *)
val sub : array * int -> long = _prim (@sub)
(* update (a, i, x) *)
(* sets the ith element of array a to x *)
(* it is required that 0 <= i < |a|, where |a| is the size of a *)
val update : array * int * long -> unit = _prim (@update)

end
