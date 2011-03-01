(* unsafe-array.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Unsafe, polymorphic arrays.
 *)

#include <prim.def>

#define MAX_ARRAY_SZ            I32Div(MAX_ARRAY_SZB, 8)

structure UnsafeArray = struct

_primcode (
  typedef array = PrimTypes.array;
  extern void* AllocBigPolyArray (void*, int, void*) __attribute__((alloc));
  define inline @create (arg : [ml_int, any] / exh : exh) : array =
    let n : int = #0(#0(arg))
    if I32Lt (n, MAX_ARRAY_SZ) then
      let init : any = (any)#1(arg)
      let init : any = promote(init)
      let data : any = ccall AllocBigPolyArray (host_vproc, n, init)
      let a : array = alloc (data, n)
      return(a)
    else
      throw exh (Fail (@"UnsafeArray: requested array size too big"))
  ;
  define inline @sub (arg : [array, ml_int] / exh : exh) : any =
    let a : array = #0(arg)
    let data : any = #0(a)
    let ix : int = #0(#1(arg))
    let d : any = ArrLoad (data, ix)
    return(d)
  ;
  define inline @update (arg : [array, ml_int, any] / exh : exh) : unit =
    let a : array = #0(arg)
    let data : any = #0(a)
    let ix : int = #0(#1(arg))
    let x : any = (any)#2(arg)
    let x : any = promote(x)
    do ArrStore (data, ix, x)
    return(UNIT)
  ;  
)

type 'a array = _prim(array)
(* create n *)
(* allocates an array of size n *)
val create : int * 'a -> 'a array = _prim (@create)
(* sub (a, i) *)
(* returns the ith element of array a *)
(* it is required that 0 <= i < |a|, where |a| is the size of a *)
val sub : 'a array * int -> 'a = _prim (@sub)
(* update (a, i, x) *)
(* sets the ith element of array a to x *)
(* it is required that 0 <= i < |a|, where |a| is the size of a *)
val update : 'a array * int * 'a -> unit = _prim (@update)

end
