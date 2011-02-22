(* unsafe-double-array.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Unsafe, monomorphic arrays of doubles.
 *)

#include <prim.def>

#define MAX_LOCAL_ARRAY_SZ      I32Div(MAX_LOCAL_ARRAY_SZB, 8:int)
#define MAX_GLOBAL_ARRAY_SZ     I32Div(MAX_GLOBAL_ARRAY_SZB, 8:int)
#define MAX_ARRAY_SZ            I32Div(MAX_ARRAY_SZB, 8:int)

structure UnsafeDoubleArray = struct

_primcode (
  extern void* GlobalAllocDoubleArray (void*, int);
  extern void* AllocBigDoubleArray (void*, int);
  typedef array = PrimTypes.array;
  define inline @create (a : ml_int / exh : exh) : array =
    let n : int = #0(a)
    if I32Lt (n, MAX_LOCAL_ARRAY_SZ) then
      let a : array = AllocDoubleArray (n)
      return(a)
    else if I32Lt (n, MAX_GLOBAL_ARRAY_SZ) then
      let data : any = ccall GlobalAllocDoubleArray (host_vproc, n)
      let a : array = alloc (data, n)
      return(a)
    else if I32Lt (n, MAX_ARRAY_SZ) then
      let data : any = ccall AllocBigDoubleArray (host_vproc, n)
      let a : array = alloc (data, n)
      return(a)
    else
      throw exh (Fail (@"UnsafeDoubleArray: requested array size too big"))
  ;
  define inline @sub (arg : [array, ml_int] / exh : exh) : ml_double =
    let a : array = #0(arg)
    let data : any = #0(a)
    let ix : int = #0(#1(arg))
    let x : double = ArrLoadF64 (data, ix)
    return (alloc(x))
  ;
  define inline @update (arg : [array, ml_int, ml_double] / exh : exh) : unit =
    let a : array = #0(arg)
    let data : any = #0(a)
    let ix : int = #0(#1(arg))
    let x : double = #0(#2(arg))
    do ArrStoreF64 (data, ix, x)
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
val sub : array * int -> double = _prim (@sub)
(* update (a, i, x) *)
(* sets the ith element of array a to x *)
(* it is required that 0 <= i < |a|, where |a| is the size of a *)
val update : array * int * double -> unit = _prim (@update)

end
