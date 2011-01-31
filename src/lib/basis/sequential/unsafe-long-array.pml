(* unsafe-long-array.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Unsafe, monomorphic arrays of longs.
 *)

structure UnsafeLongArray = struct

_primcode (
  typedef array = PrimTypes.array;
  define inline @create (n : ml_int / exh : exh) : array =
    let a : array = AllocLongArray (#0(n))
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
val create : int -> array = _prim (@create)
val sub : array * int -> long = _prim (@sub)
val update : array * int * long -> unit = _prim (@update)

end
