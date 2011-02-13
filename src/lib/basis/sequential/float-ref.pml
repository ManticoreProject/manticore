(* float-ref.pml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

#include <prim.def>

structure FloatRef =
  struct

    _primcode (

      typedef ref = ![float];

      define inline @new (x : ml_float / exh: exh) : ref =
        let cell : ref = alloc(#0(x))
        return(cell)
        ;

      define inline @set (args : [ref, ml_float] / exh: exh) : unit =
        let r : ref = #0(args)
        let x : float = unwrap(#1(args))
        do #0(r) := x
        return(UNIT)
        ;

      define inline @get (r: ref / exh: exh) : ml_float = 
        return(wrap(#0(r)))
        ;
    )

    type ref = _prim(ref)

    val new : float -> ref        = _prim(@new)
    val set : ref * float -> unit = _prim(@set)
    val get : ref -> float        = _prim(@get)

  end
