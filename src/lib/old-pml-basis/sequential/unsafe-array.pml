(* unsafe-array.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Unsafe, polymorphic arrays.
 *)

#include <prim.def>

structure UnsafeArray = struct

_primcode (
  typedef array = PrimTypes.array;
  extern void* AllocBigPolyArray (void*, int, void*) __attribute__((alloc));
  define inline @create (n : int, init : any / exh : exh) : array =
    let init : any = (any)init
    let init : any = promote(init)
    let data : any = ccall AllocBigPolyArray (host_vproc, n, init)
    return(alloc (data, n))
  ;
  define inline @create-w (arg : [ml_int, any] / exh : exh) : array =
    @create (#0(#0(arg)), #1(arg) / exh)
  ;
  define inline @sub (a : array, ix : int / exh : exh) : any =
    return(ArrLoad (#0(a), ix))
  ;
  define inline @sub-w (arg : [array, ml_int] / exh : exh) : any =
    @sub (#0(arg), #0(#1(arg)) / exh)
  ;
  define inline @update (a : array, ix : int, x : any / exh : exh) : () =
    let x : any = promote((any)x)
    do ArrStore (#0(a), ix, x)
    return()
  ;
  define inline @update-w (arg : [array, ml_int, any] / exh : exh) : unit =
    do @update (#0(arg), #0(#1(arg)), #2(arg) / exh)
    return(UNIT)
  ;
)

type 'a array = _prim(array)
(* create n *)
(* allocates an array of size n *)
val create : int * 'a -> 'a array = _prim (@create-w)
(* sub (a, i) *)
(* returns the ith element of array a *)
(* it is required that 0 <= i < |a|, where |a| is the size of a *)
val sub : 'a array * int -> 'a = _prim (@sub-w)
(* update (a, i, x) *)
(* sets the ith element of array a to x *)
(* it is required that 0 <= i < |a|, where |a| is the size of a *)
val update : 'a array * int * 'a -> unit = _prim (@update-w)

end
