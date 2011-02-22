(* unsafe-int-array.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Unsafe, monomorphic arrays of ints.
 *)

#include <prim.def>

#define MAX_LOCAL_ARRAY_SZ      I32Div(MAX_LOCAL_ARRAY_SZB, 4:int)
#define MAX_GLOBAL_ARRAY_SZ     I32Div(MAX_GLOBAL_ARRAY_SZB, 4:int)
#define MAX_ARRAY_SZ            I32Div(MAX_ARRAY_SZB, 4:int)

structure UnsafeIntArray = struct

_primcode (
  extern void* GlobalAllocIntArray (void*, int);
  extern void* AllocBigIntArray (void*, int);
  typedef array = PrimTypes.array;
  define inline @create (a : ml_int / exh : exh) : array =
    let n : int = #0(a)
    if I32Lt (n, MAX_LOCAL_ARRAY_SZ) then
      let a : array = AllocIntArray (n)
      return(a)
    else if I32Lt (n, MAX_GLOBAL_ARRAY_SZ) then
      let data : any = ccall GlobalAllocIntArray (host_vproc, n)
      let a : array = alloc (data, n)
      return(a)
    else if I32Lt (n, MAX_ARRAY_SZ) then
      let data : any = ccall AllocBigIntArray (host_vproc, n)
      let a : array = alloc (data, n)
      return(a)
    else
      throw exh (Fail (@"UnsafeIntArray: requested array size too big"))
  ;
  define inline @sub (arg : [array, ml_int] / exh : exh) : ml_int =
    let a : array = #0(arg)
    let data : any = #0(a)
    let ix : int = #0(#1(arg))
    let x : int = ArrLoadI32 (data, ix)
    return (alloc(x))
  ;
  define inline @update (arg : [array, ml_int, ml_int] / exh : exh) : unit =
    let a : array = #0(arg)
    let data : any = #0(a)
    let ix : int = #0(#1(arg))
    let x : int = #0(#2(arg))
    do ArrStoreI32 (data, ix, x)
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
val sub : array * int -> int = _prim (@sub)
(* update (a, i, x) *)
(* sets the ith element of array a to x *)
(* it is required that 0 <= i < |a|, where |a| is the size of a *)
val update : array * int * int -> unit = _prim (@update)

end
